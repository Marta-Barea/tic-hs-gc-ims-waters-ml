# =============================================================================
# Data Loading & Preprocessing
# =============================================================================

## Load required libraries ----
library(readxl)
library(caret)
library(dplyr)
library(cluster)
library(purrr)
library(factoextra)
library(viridis)
library(ggplot2)
library(ggrepel)
library(data.table)
library(reshape2)

## Set output directory for figures ----
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

## Load dataset and remove unnecessary columns ----
clean_data <- read_excel("data/clean_dataset_hsgcims_waters.xlsx")
first_column <- clean_data[, 1] # Preserve the first column
clean_data <- clean_data[, -c(1, 2)] # Remove non-relevant columns

## Preprocessing ----
preprocessor <- preProcess(
    clean_data,
    method = c("center", "scale")
)
data_transformed <- predict(preprocessor, clean_data)

## Reattach first column and update row names ----
data_transformed <- cbind(first_column, data_transformed)
rownames(data_transformed) <- data_transformed$Sample

# =============================================================================
# Principal Component Analysis (PCA)
# =============================================================================

pca_data <- data_transformed[, -1]
pca_result <- prcomp(pca_data, scale. = FALSE)

print(pca_result)
summary(pca_result)

# =============================================================================
# Scree Plot (Eigenvalues)
# =============================================================================

scree_plot <- fviz_eig(
    pca_result,
    xlab = "Principal Components (PCs)",
    ylab = "Explained Variance (%)",
    main = "",
    addlabels = TRUE,
    ggtheme = theme_minimal(),
    barcolor = "#404788FF",
    barfill = "#404788FF",
    linecolor = "#000000"
)

print(scree_plot)

ggsave(
    filename = file.path(output_dir, "scree_plot.png"),
    plot = scree_plot,
    width = 10,
    height = 6,
    dpi = 300
)

# =============================================================================
# Score Plot (PC1 vs PC2)
# =============================================================================

pca_scores <- as.data.frame(pca_result$x)
pca_scores$Sample <- rownames(pca_scores)

pca_scores <- pca_scores %>%
    mutate(Group = case_when(
        grepl("Gas", Sample, ignore.case = TRUE) ~ "Gas",
        grepl("Dies", Sample, ignore.case = TRUE) ~ "Dies",
        grepl("FW", Sample, ignore.case = TRUE) ~ "FW",
        grepl("Lub", Sample, ignore.case = TRUE) ~ "Lub",
        grepl("Ker", Sample, ignore.case = TRUE) ~ "Ker",
        grepl("Par", Sample, ignore.case = TRUE) ~ "Par",
        TRUE ~ "Other"
    ))

score_plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, col = Group, label = Sample)) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
    geom_point(alpha = 0.1, size = 6, color = "black") +
    geom_point(alpha = 1, size = 2, shape = 16) +
    guides(color = guide_legend(title = "Category")) +
    labs(
        x = paste0("PC1 (", round(summary(pca_result)$importance[2, 1] * 100, 1), "%)"),
        y = paste0("PC2 (", round(summary(pca_result)$importance[2, 2] * 100, 1), "%)"),
        title = ""
    ) +
    theme(
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 10, face = "bold"),
        legend.text = element_text(size = 12)
    ) +
    theme_test() +
    scale_color_viridis(discrete = TRUE, option = "D")

print(score_plot)

ggsave(
    filename = file.path(output_dir, "score_plot.png"),
    plot = score_plot,
    width = 10,
    height = 6,
    dpi = 300
)

# =============================================================================
# Loadings Plot
# =============================================================================

loadings <- as.data.frame(pca_result$rotation[, 1:2])

setDT(loadings, keep.rownames = TRUE)[]
ld <- melt(loadings, "rn")
ld$rn <- as.numeric(ld$rn)

loadings_plot <- ggplot(ld, aes(x = rn, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    theme_test() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8),
        plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(x = "Drift Time [RIP Relative]", y = "Loadings PCs", title = "") +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 50),
        labels = scales::number_format(accuracy = 0.01),
        expand = c(0, 0)
    ) +
    scale_fill_viridis_d(option = "D")

print(loadings_plot)

ggsave(
    filename = file.path(output_dir, "loadings_plot.png"),
    plot = loadings_plot,
    width = 10,
    height = 6,
    dpi = 300
)

# =============================================================================
# Export Package Versions to requirements.txt
# =============================================================================

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}

existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)

packages <- c(
    "readxl", "caret", "dplyr", "cluster", "purrr",
    "factoextra", "viridis", "ggplot2", "ggrepel", "data.table", "reshape2"
)

to_add <- setdiff(packages, existing_pkgs)

if (length(to_add) > 0) {
    new_lines <- paste0(
        to_add, "==",
        sapply(to_add, function(x) as.character(packageVersion(x)))
    )
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
