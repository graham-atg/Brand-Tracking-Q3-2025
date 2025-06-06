#The assign_label_to_value function accepts a dataframe, a vector of column names,
#a specified value, and a corresponding label. It iterates through the specified
#columns, assigning the provided label to rows with the specified value, and returns the modified dataframe.
#Note: assign label functions works to label new values in R.

assign_label_to_value <- function(dataframe, columns, value, label) {
  if (nrow(dataframe) == 0) {
    return(dataframe)
  }
  
  df_copy <- dataframe
  for (col in columns) {
    df_copy[[col]] <- ifelse(df_copy[[col]] == value, label, df_copy[[col]])
  }
  return(df_copy)
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

# Unit tests
library(testthat)

test_that("assign_label_to_value function works as expected", {
  # Test with a basic dataframe
  df1 <- data.frame(
    ID = 1:5,
    Category1 = c(2, 3, 2, 1, 4),
    Category2 = c(3, 2, 4, 2, 1)
  )
  expected_df1 <- data.frame(
    ID = 1:5,
    Category1 = c("New Label", 3, "New Label", 1, 4),
    Category2 = c(3, "New Label", 4, "New Label", 1)
  )
  expect_equal(assign_label_to_value(df1, c("Category1", "Category2"), 2, "New Label"), expected_df1)
  
  # Test with a dataframe that doesn't have the specified value
  df2 <- data.frame(
    ID = 1:5,
    Category1 = c(3, 3, 3, 3, 3),
    Category2 = c(4, 4, 4, 4, 4)
  )
  expected_df2 <- data.frame(
    ID = 1:5,
    Category1 = c(3, 3, 3, 3, 3),
    Category2 = c(4, 4, 4, 4, 4)
  )
  expect_equal(assign_label_to_value(df2, c("Category1", "Category2"), 2, "New Label"), expected_df2)
  
  # Test with an empty dataframe
  df3 <- data.frame()
  expected_df3 <- data.frame()
  expect_equal(assign_label_to_value(df3, c("Category1", "Category2"), 2, "New Label"), expected_df3)
})