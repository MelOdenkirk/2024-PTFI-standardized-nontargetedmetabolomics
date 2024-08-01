# Load necessary libraries
library(dplyr)
library(tidyr)
library(VennDiagram)

# Define the error windows for RI and mass
RI_error <- 3.5  # Example value, adjust as needed
mass_error <- 0.05  # Example value, adjust as needed

# Function to read and process datasets
process_dataset <- function(file) {
  df <- read.csv(file)
  dataset_name <- tools::file_path_sans_ext(basename(file))  # Extract dataset name from file name
  # Add dataset prefix to column names
  colnames(df)[-(1:2)] <- paste(dataset_name, colnames(df)[-(1:2)], sep = "_")
  return(df)
}

# Read in the datasets
files <- list.files(path = "R:\\Projects\\PTFI\\PTFI Publications\\Apples-to-Apples_OnePaper\\ptfi-data-4labs\\MSDial-240711_4labs-data\\cleaned-data", 
                    pattern = "*.csv", full.names = TRUE)
datasets <- lapply(files, process_dataset)

# Combine all datasets into one
combined_df <- Reduce(function(x, y) full_join(x, y, by = c("RI", "mass")), datasets)

# Create a function to merge rows based on error windows
merge_rows <- function(df, RI_error, mass_error) {
  # Create a unique identifier for each row based on RI and mass
  df <- df %>%
    mutate(ID = paste0("RI:", RI, "_mass:", mass))
  
  # Initialize an empty data frame to store the merged results
  merged_df <- data.frame()
  
  while(nrow(df) > 0) {
    # Take the first row
    current_row <- df[1, ]
    # Find rows within the error windows
    matching_rows <- df %>%
      filter(abs(RI - current_row$RI) <= RI_error & abs(mass - current_row$mass) <= mass_error)
    
    # Combine the matching rows
    combined_row <- matching_rows %>%
      summarise(across(everything(), ~ if(is.numeric(.)) sum(., na.rm = TRUE) else first(.))) %>%
      mutate(RI = current_row$RI,
             mass = current_row$mass,
             ID = paste0("RI:", current_row$RI, "_mass:", current_row$mass))
    
    # Append to the merged data frame
    merged_df <- bind_rows(merged_df, combined_row)
    
    # Remove the matching rows from the original data frame
    df <- df %>%
      filter(!(abs(RI - current_row$RI) <= RI_error & abs(mass - current_row$mass) <= mass_error))
  }
  
  return(merged_df)
} 
  

# Apply the merging function
final_df <- merge_rows(combined_df, RI_error, mass_error)

# Replace all 0 values with NA
final_df[final_df == 0] <- NA

# Print the final data table
print(final_df)

# Write the final data table to a CSV file
write.csv(final_df, "R:\\Projects\\PTFI\\PTFI Publications\\Apples-to-Apples_OnePaper\\ptfi-data-4labs\\MSDial-240711_4labs-data\\cleaned-data\\output.csv", row.names = FALSE)
