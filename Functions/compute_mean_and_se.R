compute_mean_and_se <- function(dataframe, column_names = NULL) {
  library(tidyr) # Load tidyr package
  library(dplyr) # Load dplyr package
  
  # If column_names is not specified, select all numeric columns
  if (is.null(column_names)) {
    column_names <- names(dataframe)[sapply(dataframe, is.numeric)]
  }
  
  # Filter dataframe to only include the specified column_names
  dataframe_filtered <- dataframe[, column_names, drop = FALSE]
  
  result_df <- dataframe_filtered %>%
    summarise(across(everything(), list(mean = mean, se = ~ sd(.) / sqrt(sum(!is.na(.)))))) %>%
    pivot_longer(cols = everything(), names_to = "Column", names_pattern = "(.*)_(.*)", values_to = "Value")
  
  return(result_df)
}

# Test
test_that("compute_mean_and_se works as expected", {
  # Create a sample dataframe
  sample_data <- data.frame(
    ID = 1:5,
    Name = c("A", "B", "C", "D", "E"),
    FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
    NumericColumn = c(1, 2, NA, 4, 5)
  )
  
  # Test 1: Compute mean and SE for all numeric columns
  result <- compute_mean_and_se(sample_data)
  
  expect_equal(nrow(result), 2) # Expect 2 rows
  expect_true(all(result$Column %in% c("NumericColumn_mean", "NumericColumn_se"))) # Expect column names
  expect_true(all(!is.na(result$Value))) # Expect no NA values
  
  # Test 2: Compute mean and SE for a specific column
  result <- compute_mean_and_se(sample_data, column_names = "NumericColumn")
  
  expect_equal(nrow(result), 2) # Expect 2 rows
  expect_true(all(result$Column %in% c("NumericColumn_mean", "NumericColumn_se"))) # Expect column names
  expect_true(all(!is.na(result$Value))) # Expect no NA values
})