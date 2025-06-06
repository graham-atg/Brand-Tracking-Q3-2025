# Function: exclude_string_for_word
# Description: Subset a dataframe to include rows where none of the specified search strings are partially matched in a column.
# Parameters:
#   - dataframe: The input dataframe.
#   - id_col: The name of the ID column in the dataframe.
#   - text_col: The name of the column containing text strings to search.
#   - partial_strings: A list of strings for partial matching in the specified column.
# Returns: A dataframe containing the ID column and the column with non-matching partial strings.

exclude_string_for_word <- function(dataframe, id_col, text_col, partial_strings) {
  # Check if the specified text column exists in the dataframe
  if (!(text_col %in% names(dataframe))) {
    stop("The specified text column does not exist in the dataframe.")
  }
  
  # Create an empty list to store subset dataframes
  subset_data_list <- list()
  
  # Iterate over each partial string
  for (partial_string in partial_strings) {
    subset_data <- dataframe[!grepl(partial_string, dataframe[[text_col]], fixed = TRUE), c(id_col, text_col), drop = FALSE]
    subset_data_list[[partial_string]] <- subset_data
  }
  
  # Return the resulting list of subset dataframes
  return(subset_data_list)
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  ID = 1:5,
  Description = c("This is a sample text for employees.", "Sample description here for employers.", "Another example.", "Text for testing.", "Final text sample.")
)

# Specify the partial strings as a list
partial_strings <- c("employee", "test")

# Exclude rows where any of the partial strings are found
result <- exclude_string_for_word(sample_data, "ID", "Description", partial_strings)
print(result)
