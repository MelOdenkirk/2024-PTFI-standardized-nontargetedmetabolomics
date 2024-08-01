# Load necessary packages
library(ggplot2)
library(GGally)
library(scales)

# Function to subset data, replace 0 with NA, and retain complete cases
subset_data <- function(df, ri_col, col1, col2) {
  subset_df <- df[, c(ri_col, col1, col2)]
  subset_df[subset_df == 0] <- NA
  subset_df <- subset_df[complete.cases(subset_df), ]
  return(subset_df)

# Remove values only seen in one sample
  subset_df <- subset_df[rowSums(!is.na(subset_df[, c(col1, col2)])) > 1, ]
  
  return(subset_df)
}

# Read the CSV file
df <- read.csv("output_food-overlap.csv", header = TRUE)

# Define the columns for each subset
cols <- list(
  LabA = list(GSapple = c(3, 6, 7), potato = c(3, 8, 9), gapple = c(3, 10, 11), beef = c(3, 12, 13), onion = c(3, 14, 15)),
  LabB = list(GSapple = c(3, 24, 25), potato = c(3, 20, 21), gapple = c(3, 26, 27), beef = c(3, 22, 23), onion = c(3, 18, 19)),
  LabC = list(GSapple = c(3, 32, 33), potato = c(3, 34, 35), gapple = c(3, 38, 39), beef = c(3, 36, 37), onion = c(3, 30, 31)),
  LabD = list(GSapple = c(3, 40, 41), potato = c(3, 42, 43), gapple = c(3, 46, 47), beef = c(3, 44, 45), onion = c(3, 28, 29))
)

# Create subsets for each lab and food type
for (lab in names(cols)) {
  for (food in names(cols[[lab]])) {
    assign(paste0(lab, "_", food), subset_data(df, cols[[lab]][[food]][1], cols[[lab]][[food]][2], cols[[lab]][[food]][3]))
  }
}

# Function to create and save customized correlation plots
create_custom_correlation_plot <- function(data, lab, food, color) {
  # Ensure only numeric columns are used for plotting
  data_numeric <- data[sapply(data, is.numeric)]
  
  if (ncol(data_numeric) > 2) {
    # Adjust this to fit your needs; here we take the first two numeric columns
    data_numeric <- data_numeric[, 1:2]
  }
  
  plot <- ggpairs(data_numeric,
                  upper = list(continuous = wrap("cor", color = color)),
                  lower = list(continuous = wrap("smooth", color = color)), # Add smooth lines
                  diag = list(continuous = wrap("barDiag", color = color)),
                  title = paste(lab, food, "Correlation")) +
    theme_bw() + # Sets a plain white background
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(colour = "black", size = 1), # Adds axis lines
      axis.text.x = element_text(size = 14, face = "bold", color = "black"),
      axis.text.y = element_text(size = 14, face = "bold", color = "black"),
      axis.title.x = element_text(size = 14, face = "bold", color = "black"),
      axis.title.y = element_text(size = 14, face = "bold", color = "black"),
      plot.title = element_text(size = 16, face = "bold", color = "black")
    ) +
    scale_x_continuous(labels = scientific_format(digits= 1)) + # Scientific notation for x-axis
    scale_y_continuous(labels = scientific_format(digits= 1)) # Scientific notation for y-axis
  
  ggsave(filename = paste0(lab, "_", food, "_correlation_no1hits.png"), plot = plot)
}

# Define custom colors for each subset (example values)
colors <- list(
  LabA = list(GSapple = "#99d95a", potato = "#764508", gapple = "#a23636", beef = "lightcoral", onion = "#eb9c5a"),
  LabB = list(GSapple = "#99d95a", potato = "#764508", gapple = "#a23636", beef = "lightcoral", onion = "#eb9c5a"),
  LabC = list(GSapple = "#99d95a", potato = "#764508", gapple = "#a23636", beef = "lightcoral", onion = "#eb9c5a"),
  LabD = list(GSapple = "#99d95a", potato = "#764508", gapple = "#a23636", beef = "lightcoral", onion = "#eb9c5a")
)

# Create and save customized correlation plots for each lab and food type
for (lab in names(cols)) {
  for (food in names(cols[[lab]])) {
    data <- get(paste0(lab, "_", food))
    create_custom_correlation_plot(data, lab, food, colors[[lab]][[food]])
  }
}

