# Function: column_screening
# Description: Subset a dataframe based on column values and check conditions.
# Parameters:
#   - dataframe: The input dataframe.
#   - id_col: The name of the ID column in the dataframe.
#   - col_names: A vector of column names to check.
#   - values: A vector of values or thresholds for comparison with the specified columns. NA values are allowed.
#   - check_method: The logical operation ('AND' or 'OR') to apply when checking multiple columns.
#   - operators: A vector of operators for numeric columns ('>', '>=', '<', '<=', '=').
# Returns: A dataframe containing rows that do not pass the specified screening conditions.
library(dplyr)

column_screening <- function(dataframe, id_col, col_names, values, check_method = "AND", operators = NULL) {
  # Check if the input dataframe has at least one column
  if (ncol(dataframe) < 1) {
    stop("The input dataframe must have at least one column.")
  }
  
  # Check if the column names and values are provided
  if (length(col_names) == 0 || length(values) == 0) {
    stop("At least one column name and one value must be specified.")
  }
  
  if (length(col_names) != length(values)) {
    stop("The number of column names must match the number of specified values.")
  }
  
  if (!(check_method %in% c("AND", "OR"))) {
    stop("Invalid check_method. Use 'AND' or 'OR'.")
  }
  
  if (is.null(operators)) {
    operators <- rep("==", length(col_names))
  } else if (length(operators) != length(col_names)) {
    stop("The number of operators must match the number of specified columns.")
  }
  
  # Create a logical condition based on check_method
  condition <- create_condition(dataframe, col_names, values, operators, check_method)
  
  # Subset the dataframe to get rows that don't pass the screening
  non_passing_data <- dataframe[!condition, ]
  
  # Include the ID column and specified column names
  selected_columns <- c(id_col, col_names)
  
  # Return the resulting subset of data
  return(non_passing_data[, selected_columns, drop = FALSE])
}

create_condition <- function(dataframe, col_names, values, operators, check_method) {
  if (check_method == "AND") {
    condition <- Reduce(`&`, lapply(seq_along(col_names), function(i) {
      col_name <- col_names[i]
      value <- values[i]
      operator <- operators[i]
      
      apply_operator(dataframe[[col_name]], value, operator)
    }))
  } else if (check_method == "OR") {
    condition <- Reduce(`|`, lapply(seq_along(col_names), function(i) {
      col_name <- col_names[i]
      value <- values[i]
      operator <- operators[i]
      
      apply_operator(dataframe[[col_name]], value, operator)
    }))
  }
  condition
}

apply_operator <- function(column, value, operator) {
  if (is.factor(column)) {
    if (is.na(value)) {
      is.na(column)
    } else {
      column == as.character(value)
    }
  } else if (is.numeric(column)) {
    if (is.na(value)) {
      is.na(column)
    } else if (is.numeric(value) || (is.character(value) && all(grepl("^-?\\d+(\\.\\d+)?$", value)))) {
      value <- as.numeric(value)
      switch(operator,
             ">" = column > value,
             ">=" = column >= value,
             "<" = column < value,
             "<=" = column <= value,
             "==" = column == value,
             "%in%" = column %in% value,
             stop(paste("Operator", operator, "is not supported."))
      )
    } else {
      stop(paste("Cannot compare numeric column with non-numeric value:", value))
    }
  } else if (is.character(column)) {
    if (is.na(value)) {
      is.na(column)
    } else {
      switch(operator,
             "==" = column == value,
             "%in%" = column %in% value,
             "like" = grepl(value, column),
             stop(paste("Operator", operator, "is not supported for character columns."))
      )
    }
  } else {
    stop(paste("Column", col_name, "is not a factor, numeric, or character column."))
  }
}


# Unit tests
library(testthat)

test_that("column_screening works as expected", {
  # Test 1: Dataframe with no columns
  df1 <- data.frame()
  expect_error(column_screening(df1, "ID", "x", 3), "The input dataframe must have at least one column.")
  
  # Test 2: No column names or values specified
  df2 <- data.frame(a = 1:5, b = 1:5)
  expect_error(column_screening(df2, "a", c(), c()), "At least one column name and one value must be specified.")
  
  # Test 3: Dataframe with two columns, column names and values mismatch
  df3 <- data.frame(a = 1:5, b = 1:5)
  expect_error(column_screening(df3, "a", c("a", "b"), c(3)), "The number of column names must match the number of specified values.")
  
  # Test 4: Dataframe with three columns, check_method is invalid
  df4 <- data.frame(a = 1:5, b = 1:5, c = 1:5)
  expect_error(column_screening(df4, "a", c("a", "b"), c(3, 4), check_method = "XOR"), "Invalid check_method. Use 'AND' or 'OR'.")
  
  # Test 5: Dataframe with three columns, operators don't match columns
  df5 <- data.frame(a = 1:5, b = 1:5, c = 1:5)
  expect_error(column_screening(df5, "a", c("a", "b"), c(3, 4), operators = c(">", ">")), "The number of operators must match the number of specified columns.")
  
  # Test 6: Dataframe with numeric and factor columns, AND logic
  df6 <- data.frame(ID = 1:5, Age = c(25, 30, 22, 25, 28), Gender = factor(c("M", "F", "M", "F", "M")))
  result6 <- column_screening(df6, "ID", c("Age", "Gender"), c(25, "M"), check_method = "AND")
  expect_equal(nrow(result6), 2)
  expect_equal(result6$ID, c(3, 5))
  
  # Test 7: Dataframe with numeric and factor columns, OR logic
  df7 <- data.frame(ID = 1:5, Age = c(25, 30, 22, 25, 28), Gender = factor(c("M", "F", "M", "F", "M")))
  result7 <- column_screening(df7, "ID", c("Age", "Gender"), c(25, "F"), check_method = "OR")
  expect_equal(nrow(result7), 3)
  expect_equal(result7$ID, c(1, 3, 5))
  
  # Test 8: Dataframe with character columns, supported operators
  # Test 8: Dataframe with character columns, supported operators
  df8 <- data.frame(ID = 1:5, Name = c("Alice", "Bob", "Charlie", "David", "Eve"))
  result8_1 <- column_screening(df8, "ID", "Name", "Alice", operators = "==")
  expect_equal(nrow(result8_1), 1)
  expect_equal(result8_1$ID, 1)
  
  result8_2 <- column_screening(df8, "ID", "Name", "Bob", operators = "%in%")
  expect_equal(nrow(result8_2), 1)
  expect_equal(result8_2$ID, 2)
  
  result8_3 <- column_screening(df8, "ID", "Name", "il", operators = "like")
  expect_equal(nrow(result8_3), 2)
  expect_equal(result8_3$ID, c(3, 4))
  
  # Test 9: Dataframe with unsupported column type
  # Test 9: Dataframe with unsupported column type
  df9 <- data.frame(
    ID = 1:5,
    Data = list(
      data.frame(x = 1:3),
      data.frame(x = 4:6),
      data.frame(x = 7:9),
      data.frame(x = 10:12),
      data.frame(x = 13:15)
    )
  )
  expect_error(column_screening(df9, "ID", "Data", 1), "Column Data is not a factor, numeric, or character column.")