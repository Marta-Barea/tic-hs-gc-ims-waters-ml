# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialization and Package Loading
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(doParallel)
library(readxl)
library(Boruta)
library(stringr)
library(caret)
library(dplyr)
library(themis)
library(recipes)
library(ggplot2)
library(viridis)
library(GA)
library(gridExtra)

# Set up parallel processing
cl <- makePSOCKcluster(8)
registerDoParallel(cl)

# Create output directories if needed
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Loading and Preparation
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clean_data <- read_excel("data/clean_dataset_hsgcims_waters.xlsx") %>% select(-1)
set.seed(8564)
intrain <- createDataPartition(clean_data$Group, p = 0.7, list = FALSE)
imbal_train <- clean_data[intrain, ] %>% mutate(Group = as.factor(Group))
imbal_test <- clean_data[-intrain, ] %>% mutate(Group = as.factor(Group))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to Run Experiment Based on Feature Selection Method
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
run_experiment <- function(method, imbal_train, imbal_test, output_prefix) {
    cat("Running experiment for method:", method, "\n")

    if (method == "none") {
        selected_features <- setdiff(names(imbal_train), "Group")
    } else if (method == "boruta") {
        set.seed(7234)
        boruta_out <- Boruta(Group ~ .,
            data = imbal_train,
            doTrace = 2,
            maxRuns = 500,
            pValue = 0.01,
            mcAdj = TRUE
        )
        boruta_fixed <- TentativeRoughFix(boruta_out)
        selected_features <- names(boruta_fixed$finalDecision)[
            boruta_fixed$finalDecision == "Confirmed"
        ] %>% str_remove_all("`")
        if (length(selected_features) == 0) {
            warning("Boruta did not select any feature. Using all features.")
            selected_features <- setdiff(names(imbal_train), "Group")
        }
    } else if (method == "ga") {
        all_features <- setdiff(names(imbal_train), "Group")
        ga_fitness <- function(binary_vector) {
            if (sum(binary_vector) == 0) {
                return(0)
            }
            sel <- all_features[which(binary_vector == 1)]
            data_sel <- imbal_train %>% select(all_of(sel), Group)
            ctrl <- trainControl(method = "cv", number = 3, verboseIter = FALSE)
            # For RF, use a default mtry value (e.g., floor(sqrt(num_features)))
            mtry_default <- floor(sqrt(ncol(data_sel) - 1))
            grid <- expand.grid(mtry = mtry_default)
            set.seed(7234)
            mod <- tryCatch(
                train(Group ~ .,
                    data = data_sel, method = "rf",
                    trControl = ctrl, tuneGrid = grid, ntree = 100
                ),
                error = function(e) {
                    return(NULL)
                }
            )
            if (is.null(mod) || !is.list(mod)) {
                return(0)
            }
            return(max(mod$results$Accuracy))
        }
        set.seed(7234)
        ga_model <- ga(
            type = "binary", fitness = ga_fitness, nBits = length(all_features),
            popSize = 50, maxiter = 50, run = 10, parallel = FALSE
        )
        best_solution <- ga_model@solution[1, ]
        selected_features <- all_features[which(best_solution == 1)]
        if (length(selected_features) == 0) {
            warning("GA did not select any feature. Using all features.")
            selected_features <- all_features
        }
    } else {
        stop("Unknown feature selection method")
    }

    # Subset and preprocess data
    train_data <- imbal_train %>% select(all_of(selected_features), Group)
    test_data <- imbal_test %>% select(all_of(selected_features), Group)
    preProc <- preProcess(train_data %>% select(-Group), method = c("center", "scale"))
    train_processed <- predict(preProc, train_data) %>%
        mutate(Group = as.factor(Group)) %>%
        setNames(make.names(names(.), unique = TRUE))
    test_processed <- predict(preProc, test_data) %>%
        mutate(Group = as.factor(Group)) %>%
        setNames(make.names(names(.), unique = TRUE))

    # Balance classes using upsampling and SMOTE
    par_samples <- train_processed %>%
        filter(Group == "Par") %>%
        slice_sample(n = 6, replace = TRUE)
    smote_recipe <- recipe(Group ~ ., data = bind_rows(train_processed, par_samples)) %>%
        step_smote(Group, over_ratio = 0.5) %>%
        prep()
    smote_train <- bake(smote_recipe, new_data = NULL)

    # Train Random Forest model with cross-validation
    trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
    num_vars <- ncol(smote_train) - 1 # Excluding response variable
    grid_rf <- expand.grid(mtry = seq(1, num_vars, length.out = min(5, num_vars)) %>% round())

    start_time <- Sys.time()
    rf_model <- train(Group ~ .,
        data = smote_train,
        method = "rf",
        trControl = trctrl,
        tuneGrid = grid_rf,
        ntree = 500,
        metric = "Accuracy"
    )
    training_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))

    # Create training performance plot
    training_plot <- ggplot(rf_model$results, aes(x = factor(mtry), y = Accuracy)) +
        geom_point(color = viridis(11)[5], size = 3) +
        geom_line(aes(group = 1), color = viridis(11)[5]) +
        labs(
            title = paste("Training Performance -", toupper(method)),
            x = "mtry", y = "CV Accuracy"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    ggsave(
        filename = file.path(output_dir, paste0("rf_training_plot_", output_prefix, ".png")),
        plot = training_plot, width = 10, height = 6, dpi = 300
    )

    # Final model training and evaluation
    final_rf <- train(Group ~ .,
        data = smote_train,
        method = "rf",
        tuneGrid = rf_model$bestTune,
        ntree = 500
    )
    best_mtry <- final_rf$bestTune$mtry
    best_cv_accuracy <- max(rf_model$results$Accuracy)
    best_cv_kappa <- max(rf_model$results$Kappa)
    pred_test <- predict(final_rf, newdata = test_processed)
    cmatrix_test <- confusionMatrix(pred_test, test_processed$Group)

    # Create confusion matrix plot
    confusion_df <- as.data.frame.table(cmatrix_test$table)
    colnames(confusion_df) <- c("Prediction", "Reference", "Freq")
    confusion_plot <- ggplot(confusion_df, aes(Reference, Prediction, fill = Freq)) +
        geom_tile(color = "white") +
        geom_text(aes(label = Freq), color = "black", size = 6) +
        scale_fill_viridis(
            option = "D", direction = -1,
            breaks = seq(0, max(confusion_df$Freq), length.out = 4),
            limits = c(0, max(confusion_df$Freq)),
            guide = "none"
        ) +
        labs(
            title = paste("Confusion Matrix -", toupper(method)),
            x = "Actual Class", y = "Predicted Class", fill = "Frequency"
        ) +
        theme_minimal(base_size = 14) +
        theme(
            plot.title = element_text(hjust = 0.5, face = "bold"),
            panel.grid = element_blank(), legend.position = "right"
        )
    ggsave(
        filename = file.path(output_dir, paste0("rf_confusion_plot_", output_prefix, ".png")),
        plot = confusion_plot, width = 10, height = 6, dpi = 300
    )

    # Return results and plots
    list(
        method = method,
        selected_features = paste(selected_features, collapse = ", "),
        training_time_sec = training_time,
        best_mtry = best_mtry,
        best_cv_accuracy = best_cv_accuracy,
        best_cv_kappa = best_cv_kappa,
        test_accuracy = cmatrix_test$overall["Accuracy"],
        test_kappa = cmatrix_test$overall["Kappa"],
        confusion_matrix = cmatrix_test$table,
        training_plot = training_plot,
        confusion_plot = confusion_plot
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Running Experiments for Each Method
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
methods <- c("none", "boruta", "ga")
results_list <- lapply(methods, function(m) {
    run_experiment(method = m, imbal_train = imbal_train, imbal_test = imbal_test, output_prefix = m)
})
names(results_list) <- methods

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creating a Summary Table and Saving Results
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary_df <- do.call(rbind, lapply(results_list, function(x) {
    data.frame(
        Method = toupper(x$method),
        Num_Variables = length(strsplit(x$selected_features, ", ")[[1]]),
        Training_Time_sec = round(x$training_time_sec, 2),
        Best_mtry = x$best_mtry,
        Best_CV_Accuracy = round(x$best_cv_accuracy, 4),
        Best_CV_Kappa = round(x$best_cv_kappa, 4),
        Test_Accuracy = round(as.numeric(x$test_accuracy), 4),
        Test_Kappa = round(as.numeric(x$test_kappa), 4)
    )
}))
print(summary_df)
write.csv(summary_df, file = file.path("data", "summary_results_rf.csv"), row.names = FALSE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Combine and Save Training and Confusion Plots
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
training_plots <- lapply(results_list, function(x) x$training_plot)
combined_training_plot <- arrangeGrob(grobs = training_plots, ncol = 3)
ggsave(
    filename = file.path(output_dir, "combined_rf_training_plots.png"),
    plot = combined_training_plot, width = 15, height = 6, dpi = 300
)

confusion_plots <- lapply(results_list, function(x) x$confusion_plot)
combined_confusion_plot <- arrangeGrob(grobs = confusion_plots, ncol = 3)
ggsave(
    filename = file.path(output_dir, "combined_rf_confusion_plots.png"),
    plot = combined_confusion_plot, width = 15, height = 6, dpi = 300
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Cleanup: Stop Parallel Cluster
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
stopCluster(cl)
registerDoSEQ()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export package versions to requirements.txt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}
existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)

packages <- c("doParallel", "readxl", "Boruta", "stringr", "caret", "dplyr", "themis", "recipes", "ggplot2", "viridis", "GA", "gridExtra")
to_add <- setdiff(packages, existing_pkgs)

if (length(to_add) > 0) {
    new_lines <- paste0(to_add, "==", sapply(to_add, function(x) as.character(packageVersion(x))))
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
