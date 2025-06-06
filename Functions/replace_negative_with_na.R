#Arguments:
#data: The input dataframe that contains the columns to be processed.
#col_names: A character vector specifying the names of the columns in the dataframe where negative values should be replaced with NA.
#Output:
#The function returns a modified dataframe where all negative values in the 
#specified columns have been replaced with NA. If a specified column is not found 
#in the dataframe, a warning message is displayed.


replace_negative_with_na <- function(data, col_names) {
  for (col_name in col_names) {
    # Check if the column exists in the dataframe
    if (col_name %in% names(data)) {
      # Replace negative values with NA
      data[[col_name]][data[[col_name]] < 0] <- NA
    } else {
      cat(paste("Warning: Column", col_name, "not found in the dataframe.\n"))
    }
  }
  
  return(data)
}

# Example usage:
# Assuming df is your dataframe
# Replace "your_column1" and "your_column2" with the actual column names you want to process
# Create a test dataframe
set.seed(123)
test_data <- data.frame(
  A = c(1, -2, 3, -4, 5),
  B = c(-6, 7, -8, 9, -10),
  C = c(11, -12, 13, -14, 15)
)

# Print the original dataframe
print("Original Dataframe:")
print(test_data)

# Function call to replace negative values with NA in columns 'A' and 'B'
test_data_modified <- replace_negative_with_na(test_data, c("A", "B"))

# Print the modified dataframe
print("Modified Dataframe:")
print(test_data_modified)
