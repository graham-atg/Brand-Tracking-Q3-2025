
#The remove_duplicate_rows_retain_original function takes a dataframe and an optional 
#list of columns as inputs. It identifies duplicate rows based on the specified columns, 
#retains the original occurrences of duplicate rows, and returns a cleaned dataframe with duplicate rows removed.

remove_duplicate_rows_retain_original <- function(dataframe, columns = NULL) {
  # If columns are not specified, use all columns
  if (is.null(columns)) {
    columns <- colnames(dataframe)
  }
  
  # Find duplicate rows based on the specified columns
  duplicate_rows <- duplicated(dataframe[, columns]) | duplicated(dataframe[, columns], fromLast = TRUE)
  
  # Keep only the rows that are not duplicated
  cleaned_dataframe <- dataframe[!duplicate_rows, ]
  
  return(cleaned_dataframe)
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  ID = 1:7,
  Name = c("A", "B", "A", "C", "B", "D", "A"),
  Value = c(10, 20, 10, 30, 20, 40, 10)
)

# Remove duplicate rows based on specified columns (Name and Value)
cleaned_data <- remove_duplicate_rows_retain_original(sample_data, columns = c("Name", "Value"))

# Print the cleaned dataframe
print(cleaned_data)
