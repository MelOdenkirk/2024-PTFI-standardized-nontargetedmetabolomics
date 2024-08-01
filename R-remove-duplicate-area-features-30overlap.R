library(data.table)
library(readr)

# Function to calculate area agreement percentage
calculate_area_agreement <- function(row1, row2, area_cols) {
  areas1 <- as.data.frame(row1[, ..area_cols])
  areas2 <- as.data.frame(row2[, ..area_cols])
  
  # Remove NA values for comparison
  valid_indices <- !is.na(areas1) & !is.na(areas2)
  
  if (sum(valid_indices) == 0) return(0)
  
  areas1 <- areas1[valid_indices]
  areas2 <- areas2[valid_indices]
  
  # Calculate percentage of agreement
  agreement <- sum(areas1 == areas2) / length(areas1)
  return(agreement)
}

combine_similar_rows <- function(data, ri_tol = 3, mass_tol = 0.1, area_similarity_threshold = 0.3) {
  # Convert to data.table
  setDT(data)
  
  # Identify area columns (columns F through Q)
  area_cols <- names(data)[6:17]  # Adjust column indices if needed
  
  # Debugging: Print column names used for area check
  print(paste("Columns used for area check:", paste(area_cols, collapse = ", ")))
  
  # Remove rows with NA values in RI or mass columns
  data <- data[!is.na(RI) & !is.na(mass)]
  
  # Add a unique ID column
  data[, id := .I]
  
  # Create a list to store rows to be removed
  rows_to_remove <- vector("list", nrow(data))
  
  # Loop through each row in the data
  for (i in 1:(nrow(data) - 1)) {
    current_row <- data[i]
    comparison_rows <- data[(i + 1):nrow(data)][
      abs(RI - current_row$RI) <= ri_tol & abs(mass - current_row$mass) <= mass_tol
    ]
    
    if (nrow(comparison_rows) > 0) {
      agreements <- sapply(1:nrow(comparison_rows), function(j) {
        calculate_area_agreement(current_row, comparison_rows[j], area_cols)
      })
      
      high_agreement_indices <- which(agreements > area_similarity_threshold)
      if (length(high_agreement_indices) > 0) {
        rows_to_remove[[i]] <- comparison_rows[high_agreement_indices, id]
      }
    }
  }
  
  # Combine the rows to remove
  rows_to_remove <- unique(unlist(rows_to_remove))
  
  # Filter out the rows to remove
  combined_data <- data[!id %in% rows_to_remove]
  filtered_data <- data[id %in% rows_to_remove]
  
  # Remove the ID column
  combined_data[, id := NULL]
  filtered_data[, id := NULL]
  
  # Debugging: Print combined and filtered data summaries
  print(paste("Number of rows after combining similar rows:", nrow(combined_data)))
  print(head(combined_data))
  print(paste("Number of rows filtered out:", nrow(filtered_data)))
  print(head(filtered_data))
  
  list(combined_data = combined_data, filtered_data = filtered_data)
}

# Function to process a single file
process_file <- function(input_file, output_combined_file, output_filtered_file, ri_tol = 3, mass_tol = 0.1, area_similarity_threshold = 0.3) {
  # Read the CSV file
  data <- fread(input_file)
  
  # Debugging: Print file name and initial data summary
  print(paste("Processing file:", input_file))
  print(head(data))  # Print the first few rows of the data
  
  # Combine similar rows
  results <- combine_similar_rows(data, ri_tol, mass_tol, area_similarity_threshold)
  combined_data <- results$combined_data
  filtered_data <- results$filtered_data
  
  # Write the combined data to a new CSV file if it's not empty
  if (nrow(combined_data) > 0) {
    fwrite(combined_data, output_combined_file)
    print(paste("Saved combined file:", output_combined_file))
  } else {
    print(paste("No combined rows to save for:", output_combined_file))
  }
  
  # Write the filtered out inconsistent data to a separate CSV file if it's not empty
  if (nrow(filtered_data) > 0) {
    fwrite(filtered_data, output_filtered_file)
    print(paste("Saved filtered file:", output_filtered_file))
  } else {
    print(paste("No inconsistent rows to save for:", output_filtered_file))
  }
}

# List of input and output file paths
input_files <- c(
  "R:/Projects/PTFI/PTFI Publications/Apples-to-Apples_OnePaper/ptfi-data-4labs/MSDial-240711_4labs-data/240711_CSU-mzri_6foods_intensity_threshold.csv", 
  "R:/Projects/PTFI/PTFI Publications/Apples-to-Apples_OnePaper/ptfi-data-4labs/MSDial-240711_4labs-data/240711_JAV-mzri_6foods_intensity_threshold.csv",
  "R:/Projects/PTFI/PTFI Publications/Apples-to-Apples_OnePaper/ptfi-data-4labs/MSDial-240711_4labs-data/240711_UCA-mzri_6foods_intensity_threshold.csv",
  "R:/Projects/PTFI/PTFI Publications/Apples-to-Apples_OnePaper/ptfi-data-4labs/MSDial-240711_4labs-data/240711_VAN-mzri_6foods_intensity_threshold.csv"
)
output_combined_files <- c("cleaned_CSU_combined.csv", "cleaned_JAV_combined.csv", "cleaned_UCA_combined.csv", "cleaned_VAN_combined.csv")
output_filtered_files <- c("cleaned_CSU_filtered.csv", "cleaned_JAV_filtered.csv", "cleaned_UCA_filtered.csv", "cleaned_VAN_filtered.csv")

# Process each file
for (i in seq_along(input_files)) {
  process_file(input_files[i], output_combined_files[i], output_filtered_files[i])
}
