#The subset_dataframe function is used to subset dataframes based on pre-specified columns.
#It takes a dataframe as the input and a list of columns_to_keep that need to be retained in the subset. 
#If the columns_to_keep list is empty, an error is thrown. 
#The function returns the subset of the original dataframe based on the specified columns.


#Note: this function is for subsetting dataframes based on perspecified columns.
subset_dataframe <- function(dataframe, columns_to_keep) {
  # Check if columns_to_keep is not empty
  if (length(columns_to_keep) == 0) {
    stop("List of columns is empty.")
  }
  
  # Subset the dataframe based on the specified columns
  subset_data <- dataframe[, columns_to_keep, drop = FALSE]
  
  return(subset_data)
}

# Example usage:
# Create a sample dataframe
df <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 22, 28, 35),
  Salary = c(50000, 60000, 45000, 55000, 70000)
)

# List of columns to keep
columns_to_keep <- c("ID", "Name", "Salary")

# Subset the dataframe
result_df <- subset_dataframe(df, columns_to_keep)

# Print the resulting dataframe
print(result_df)
