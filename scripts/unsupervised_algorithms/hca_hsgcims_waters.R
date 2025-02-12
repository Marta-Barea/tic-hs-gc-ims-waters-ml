# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial Setup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load required libraries ----
library(readxl)
library(caret)
library(dplyr)
library(cluster)
library(purrr)
library(factoextra)
library(viridis)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Loading & Preparation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Determine Optimal Linkage Method ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Define clustering methods ----
linkage_methods <- c("average", "single", "complete", "ward")
names(linkage_methods) <- linkage_methods

## Compute agglomerative coefficients for different methods ----
agglomerative_coefficients <- function(method) {
    agnes(data_transformed, method = method)$ac
}
linkage_scores <- map_dbl(linkage_methods, agglomerative_coefficients)

## Print agglomerative coefficients ----
print(linkage_scores)

## Select the best linkage method ----
best_method <- names(which.max(linkage_scores))
cat("Best linkage method:", best_method, "\n")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Hierarchical Clustering ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Compute dissimilarity matrix (Euclidean distance) ----
distance_matrix <- dist(data_transformed, method = "euclidean")

## Perform hierarchical clustering using the best linkage method ----
hc_best <- hclust(distance_matrix, method = best_method)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Determine Optimal Number of Clusters ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Use Silhouette Method to find optimal number of clusters ----
silhouette_plot <- fviz_nbclust(
    data_transformed,
    FUN = hcut, # Hierarchical clustering
    method = "silhouette",
    diss = dist(data_transformed, method = "euclidean")
) +
    labs(
        title = "Silhouette Method for Optimal Clusters",
        x = "Number of Clusters",
        y = "Average Silhouette Width"
    ) +
    theme_minimal()

## Save silhouette plot ----
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)
ggsave(
    filename = file.path(output_dir, "silhouette_plot.png"),
    plot = silhouette_plot,
    width = 10,
    height = 6,
    dpi = 300
)

## Display silhouette plot ----
silhouette_plot


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualization: Dendrogram ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Generate and customize dendrogram ----

# Select specific colors from the viridis palette
custom_colors <- viridis(n = 4, option = "viridis")[c(1, 3)]

dendrogram <- fviz_dend(
    x = hc_best,
    k = 2, # Set the number of clusters (from silhouette method)
    show_labels = TRUE,
    cex = 0.7,
    lwd = 0.5,
    main = "",
    xlab = "Samples",
    ylab = "Height",
    sub = "",
    ggtheme = theme_classic(),
    horiz = FALSE,
    type = "rectangle",
    rect = TRUE,
    rect_fill = TRUE,
    k_colors = custom_colors
) +
    theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        axis.title.x = element_text(size = 12, face = "italic"),
        axis.title.y = element_text(size = 12, face = "italic"),
        axis.text = element_text(size = 10)
    )

## Save dendrogram as PNG ----
ggsave(
    filename = file.path(output_dir, "hca_plot.png"),
    plot = dendrogram,
    width = 10,
    height = 6,
    dpi = 300
)

## Display the dendrogram ----
dendrogram

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export package versions to requirements.txt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}
existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)
packages <- c("readxl", "caret", "dplyr", "cluster", "purrr", "factoextra", "viridis")
to_add <- setdiff(packages, existing_pkgs)
if (length(to_add) > 0) {
    new_lines <- paste0(to_add, "==", sapply(to_add, function(x) as.character(packageVersion(x))))
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
