# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial Setup ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Load required libraries ----
library(readxl)
library(dplyr)
library(data.table)
library(ggplot2)
library(egg)
library(viridis)
library(reshape2)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data Loading & Processing ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

clean_data <- read_excel("data/clean_dataset_hsgcims_waters.xlsx")
clean_data <- clean_data[, -1]

mean_data <- clean_data %>%
    group_by(Group) %>%
    summarise_all(mean)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Spectra Plot Generation ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

df <- melt(mean_data, "Group")

df$variable <- as.numeric(as.character(df$variable))

spectra_plot <- ggplot(df, aes(x = variable, y = value, color = Group, group = Group)) +
    geom_line() +
    labs(
        x = "Drift Time [RIP Relative]",
        y = "Total Intensity (V)"
    ) +
    theme_test() +
    theme(
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        axis.text = element_text(size = 8, hjust = 1, angle = 90),
        axis.title = element_text(size = 8)
    ) +
    scale_x_continuous(
        breaks = scales::pretty_breaks(n = 100),
        labels = scales::number_format(accuracy = 0.01),
        expand = c(0, 0)
    ) +
    scale_color_viridis(discrete = TRUE, option = "viridis") +
    scale_fill_viridis(discrete = TRUE)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save and Display Plot ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir)

ggsave(
    filename = file.path(output_dir, "tic_plot.png"),
    plot = spectra_plot,
    width = 10,
    height = 6,
    dpi = 300
)

spectra_plot

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export package versions to requirements.txt ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (file.exists("requirements.txt")) {
    existing_lines <- readLines("requirements.txt")
} else {
    existing_lines <- character(0)
}
existing_pkgs <- sub("(.*)==.*", "\\1", existing_lines)
packages <- c("readxl", "ggplot2", "dplyr", "data.table", "egg", "viridis", "reshape2")
to_add <- setdiff(packages, existing_pkgs)
if (length(to_add) > 0) {
    new_lines <- paste0(to_add, "==", sapply(to_add, function(x) as.character(packageVersion(x))))
    write(new_lines, file = "requirements.txt", append = TRUE, sep = "\n")
}
