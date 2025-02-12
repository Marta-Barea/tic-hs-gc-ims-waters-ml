# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial Setup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load required packages ----
library(readxl)
library(ggplot2)
library(dplyr)
library(writexl)
library(viridis)

## Load raw data ----
raw_data <- read_excel("data/raw_dataset_hsgcims_waters.xlsx")

## Select numeric columns ----
numeric_data <- raw_data %>% select(where(is.numeric))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Detect Outliers Using Z-Scores ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Standardize the data (Z-Scores) ----
z_scores <- scale(numeric_data)

## Compute the average absolute z-score per observation ----
average_abs_z <- rowMeans(abs(z_scores), na.rm = TRUE)

## Define a threshold (e.g., 3 is a common cutoff) ----
z_threshold <- 3

## Add outlier flag to the data ----
numeric_data_with_outliers <- numeric_data %>%
    mutate(
        Observation = row_number(),
        Average_Abs_Z = average_abs_z,
        Is_Outlier = Average_Abs_Z > z_threshold
    )

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualize Outliers ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create a scatter plot of Average Absolute Z-Scores ----
zscore_plot <- ggplot(numeric_data_with_outliers, aes(x = Observation, y = Average_Abs_Z)) +
    geom_point(aes(color = Is_Outlier), size = 2) +
    geom_hline(yintercept = z_threshold, linetype = "dashed", color = "red") +
    scale_color_viridis_d(
        option = "viridis",
        begin = 0.2, end = 0.8, # Adjust the range for better contrast
        name = "Status",
        labels = c("Inlier", "Outlier")
    ) +
    labs(
        title = "Outliers in Observations (Z-Scores)",
        x = "Observation",
        y = "Average Absolute Z-Score"
    ) +
    theme_minimal() +
    theme(
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)
    )

# Save the plot ----
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(file.path(output_dir, "zscore_outliers_plot.png"), plot = zscore_plot, width = 10, height = 6, dpi = 300)

# Display the plot ----
print(zscore_plot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Summary of Outliers ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Filter outliers ----
outliers_summary <- numeric_data_with_outliers %>%
    filter(Is_Outlier) %>%
    select(Observation, Average_Abs_Z)

## Print summary of outliers ----
print("Summary of Outlier Observations:")
print(outliers_summary)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Remove Outliers and Save Clean Data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Remove outliers from the dataset ----
clean_data <- raw_data %>%
    filter(!row_number() %in% outliers_summary$Observation)

## Save the clean dataset to an Excel file ----
clean_data_path <- "data/clean_dataset_hsgcims_waters.xlsx"
write_xlsx(clean_data, clean_data_path)

print(paste("Clean dataset saved to:", clean_data_path))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export package versions to requirements.txt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}
existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)
packages <- c("readxl", "ggplot2", "dplyr", "writexl", "viridis")
to_add <- setdiff(packages, existing_pkgs)
if (length(to_add) > 0) {
    new_lines <- paste0(to_add, "==", sapply(to_add, function(x) as.character(packageVersion(x))))
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
