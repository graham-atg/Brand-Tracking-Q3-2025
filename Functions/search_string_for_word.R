# Function: search_string_for_word
# Description: Subset a dataframe to include rows where any of the specified search strings are found in a column.
# Parameters:
#   - dataframe: The input dataframe.
#   - id_col: The name of the ID column in the dataframe.
#   - text_col: The name of the column containing text strings to search.
#   - search_strings: A list of strings to search for in the specified column.
# Returns: A dataframe containing the ID column and the column with matching strings.

search_string_for_word <- function(dataframe, id_col, text_col, search_strings) {
  # Check if the specified text column exists in the dataframe
  if (!(text_col %in% names(dataframe))) {
    stop("The specified text column does not exist in the dataframe.")
  }
  
  # Create an empty list to store subset dataframes
  subset_data_list <- list()
  
  # Iterate over each search string
  for (search_string in search_strings) {
    subset_data <- dataframe[grep(search_string, dataframe[[text_col]], ignore.case = TRUE), c(id_col, text_col), drop = FALSE]
    subset_data_list[[search_string]] <- subset_data
  }
  
  # Return the resulting list of subset dataframes
  return(subset_data_list)
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  ID = 1:5,
  Description = c("This is a sample text.", "Sample description here.", "Another example.", "Text for testing.", "Final text sample.")
)

# Specify the search strings as a list
search_strings <- c("sample", "text")

# Perform the search
result <- search_string_for_word(sample_data, "ID", "Description", search_strings)
print(result)
