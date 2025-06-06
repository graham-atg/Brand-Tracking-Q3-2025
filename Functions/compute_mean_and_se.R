#The compute_mean_and_se function computes the mean and standard error for all columns#except the first two (ID and Name) in a dataframe, taking dataframe as an input and an #optional column_names parameter for specific columns.#The compute_mean_and_se_grouped function, which utilizes the dplyr package, computes #the mean and standard error grouped by a categorical variable, taking dataframe as an #input and optional parameters such as column_names for specific columns and groupby for the grouping variable. #This function also checks for numeric columns and skips non-numeric ones.#Compute mean and SE for all columns except the first two (ID and Name)
compute_mean_and_se <- function(dataframe, column_names = NULL) {
  # If column_names is not specified, select all columns except the first two
  if (is.null(column_names)) {
    column_names <- names(dataframe)[3:length(dataframe)]
  }
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame(Column = character(0), Mean = numeric(0), SE = numeric(0))
  
  # Loop through the specified column names
  for (col_name in column_names) {
    # Check if the column is a factor
    if (is.factor(dataframe[[col_name]])) {
      # Convert factor to numeric
      dataframe[[col_name]] <- as.numeric(dataframe[[col_name]])
    }
    
    # Subset the dataframe to exclude NA values in the current column
    subset_df <- dataframe[!is.na(dataframe[[col_name]]), ]
    
    # Compute the mean and standard error
    mean_val <- mean(subset_df[[col_name]])
    se_val <- sd(subset_df[[col_name]]) / sqrt(length(subset_df[[col_name]]))
    
    # Create a new row for the result dataframe
    result_row <- data.frame(Column = col_name, Mean = mean_val, SE = se_val)
    
    # Append the result row to the result dataframe
    result_df <- rbind(result_df, result_row)
  }
  
  return(result_df)
}

# Example usage:
# Create a sample dataframe with a factor column
sample_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
  NumericColumn = c(1, 2, NA, 4, 5)
)

# Compute mean and SE for all columns except the first two (ID and Name)
result <- compute_mean_and_se(sample_data)
print(result)

# Unit tests for compute_mean_and_se
library(testthat)

test_that("compute_mean_and_se works as expected", {
  # Test with default column_names
  expect_identical(names(compute_mean_and_se(sample_data)), c("Column", "Mean", "SE"))
  expect_equal(nrow(compute_mean_and_se(sample_data)), 2)
  
  # Test with specified column_names
  expect_identical(names(compute_mean_and_se(sample_data, "FactorColumn")), c("Column", "Mean", "SE"))
  expect_equal(nrow(compute_mean_and_se(sample_data, "FactorColumn")), 1)
})



compute_mean_and_se <- function(dataframe, column_names = NULL) {
  # If column_names is not specified, select all columns except the first two
  if (is.null(column_names)) {
    column_names <- names(dataframe)[-c(1, 2)]
  }
  
  # Initialize a list to store the results
  results_list <- lapply(column_names, function(col_name) {
    # Check if the column is numeric or factor
    if (is.numeric(dataframe[[col_name]]) || is.factor(dataframe[[col_name]])) {
      # Convert factor to numeric if necessary
      if (is.factor(dataframe[[col_name]])) {
        dataframe[[col_name]] <- as.numeric(as.character(dataframe[[col_name]]))
      }
      
      # Subset the dataframe to exclude NA values in the current column
      subset_df <- dataframe[!is.na(dataframe[[col_name]]), ]
      
      # Compute the mean and standard error
      mean_val <- mean(subset_df[[col_name]])
      se_val <- sd(subset_df[[col_name]]) / sqrt(length(subset_df[[col_name]]))
      
      # Return a data frame with the results
      return(data.frame(Column = col_name, Mean = mean_val, SE = se_val))
    } else {
      # Skip non-numeric and non-factor columns
      return(NULL)
    }
  })
  
  # Combine the results into a single data frame
  result_df <- do.call(rbind, results_list)
  
  return(result_df)
}

# Example usage:
sample_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
  NumericColumn = c(1, 2, NA, 4, 5)
)

# Compute mean and SE for all columns except the first two (ID and Name)
result <- compute_mean_and_se(sample_data)
print(result)

