#The assign_label_to_value function accepts a dataframe, a vector of column names, 
#a specified value, and a corresponding label. It iterates through the specified 
#columns, assigning the provided label to rows with the specified value, and returns the modified dataframe.

#Note: assign label functions works to label new values in R.
assign_label_to_value <- function(dataframe, columns, value, label) {
  for (col in columns) {
    dataframe[dataframe[[col]] == value, col] <- label
  }
  return(dataframe)
}

# Example usage:
# Create a sample dataframe
df <- data.frame(
  ID = 1:5,
  Category1 = c(2, 3, 2, 1, 4),
  Category2 = c(3, 2, 4, 2, 1),
  Category3 = c(1, 2, 3, 4, 1)
)

# Assign label "New Label" to value 2 in columns "Category1" and "Category2"
result_df <- assign_label_to_value(df, columns = c("Category1", "Category2"), value = 2, label = "New Label")

# Print the result
print(result_df)
