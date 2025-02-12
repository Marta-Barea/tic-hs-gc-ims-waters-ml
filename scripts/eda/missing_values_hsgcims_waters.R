# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial Setup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load required packages ----
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggcorrplot)
library(gridExtra)
library(viridis)

## Load raw data ----
raw_data <- read_excel("data/raw_dataset_hsgcims_waters.xlsx")

## Select numeric columns ----
numeric_data <- raw_data %>% select(where(is.numeric))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Handle Missing Values (NA) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Convert numeric data to long format for heatmap visualization ----
heatmap_data <- numeric_data %>%
    mutate(Row = row_number()) %>% # Add a column for row indices
    pivot_longer(cols = -Row, names_to = "Variable", values_to = "Value") %>%
    mutate(Missing = factor(ifelse(is.na(Value), "Missing", "Present")))

## Format X-axis labels to display two decimal places ----
formatted_labels <- function(labels) {
    sprintf("%.2f", as.numeric(labels))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Generate Heatmap for Missing and Present Values ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Create heatmap ----
heatmap_plot <- ggplot(heatmap_data, aes(x = Variable, y = Row, fill = Missing)) +
    geom_tile() +
    scale_fill_viridis_d(name = "Status", option = "viridis", direction = -1) +
    labs(
        title = "Heatmap of Present and Missing Values",
        x = "Variable",
        y = "Row"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10)
    ) +
    scale_x_discrete(
        breaks = levels(factor(heatmap_data$Variable))[seq(1, ncol(numeric_data), by = 75)],
        labels = formatted_labels
    )

# Save the plot ----
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(file.path(output_dir, "missing_values_plot.png"), plot = heatmap_plot, width = 10, height = 6, dpi = 300)

# Display the plot ----
print(heatmap_plot)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export package versions to requirements.txt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}
existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)
packages <- c("readxl", "ggplot2", "dplyr", "tidyr", "ggcorrplot", "gridExtra", "viridis")
to_add <- setdiff(packages, existing_pkgs)
if (length(to_add) > 0) {
    new_lines <- paste0(to_add, "==", sapply(to_add, function(x) as.character(packageVersion(x))))
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
