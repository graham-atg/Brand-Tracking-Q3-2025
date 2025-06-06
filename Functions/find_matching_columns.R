#Find_matching_columns function takes a dataframe and a pattern (as a string) as 
#input. It uses the grep function to find column names that match the pattern and 
#then returns two vectors: one with matching column names separated by commas and 
#another with matching column names enclosed in quotes and separated by commas. 
#The example usage demonstrates how to use this function with a sample dataframe.

find_matching_columns <- function(dataframe, pattern) {
  # Get column names of the dataframe
  col_names <- colnames(dataframe)
  
  # Use regular expression to find matching column names
  matching_cols <- grep(pattern, col_names, value = TRUE)
  
  # Create a vector of matching column names separated by commas
  matching_cols_csv <- paste(matching_cols, collapse = ', ')
  
  # Create a vector of matching column names enclosed in quotes and separated by commas
  matching_cols_quoted <- paste0('"', matching_cols, '"', collapse = ', ')
  
  # Return both vectors
  return(list(matching_cols_csv, matching_cols_quoted))
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  Name_First = c("Alice", "Bob", "Charlie"),
  Name_Last = c("Smith", "Johnson", "Brown"),
  Age = c(25, 30, 22),
  Score_Math = c(95, 88, 76),
  Score_English = c(92, 89, 85)
)

# Find matching columns with "Name" in their names
matching_columns <- find_matching_columns(sample_data, "Name")

# Print the results
cat("Matching Columns (CSV):", matching_columns[[1]], "\n")
cat("Matching Columns (Quoted):", matching_columns[[2]], "\n")
