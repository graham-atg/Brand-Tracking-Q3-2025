
#calculate_count_and_percentage function takes a dataframe and generates the count 
#and percentage of the last column input into the function, grouped by the columns 
#specified in group_columns. If no columns are specified, it defaults to the last two columns of the dataframe.
#The inputs for this function are dataframe (the input dataframe) and an optional 
#vector group_columns specifying the columns by which the data should be grouped.

#The calculate_count_and_percentage function performs grouping, counting the occurrences, 
#and calculating the percentage of the last column, returning the results in a dataframe.


#This function takes as its input a dataframe and columns. It generates the count 
# and percentage of the last column input into the function grouped by the others
library(dplyr)

calculate_count_and_percentage <- function(dataframe, group_columns = NULL) {
  # Check if the input dataframe has at least two columns
  if (ncol(dataframe) < 2) {
    stop("The input dataframe must have at least two columns.")
  }
  
  # If group_columns is not provided, use the last column
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[ncol(dataframe)]
  }
  
  # Calculate the count and percentage
  result <- dataframe %>%
    group_by(across(all_of(group_columns))) %>%
    summarize(count = n(),
              percentage = n() / nrow(dataframe)) %>%
    ungroup() %>%
    as.data.frame()
  
  return(result)
}

# Unit tests
library(testthat)

test_that("calculate_count_and_percentage works as expected", {
  # Test 1: Dataframe with one column
  df1 <- data.frame(x = 1:5)
  expect_error(calculate_count_and_percentage(df1), "The input dataframe must have at least two columns.")
  
  # Test 2: Dataframe with two columns, group_columns not provided
  df2 <- data.frame(a = 1:5, b = 1:5)
  result2 <- calculate_count_and_percentage(df2)
  expect_equal(names(result2), c("b", "count", "percentage"))
  
  # Test 3: Dataframe with three columns, group_columns provided
  df3 <- data.frame(a = 1:5, b = 1:5, c = 1:5)
  result3 <- calculate_count_and_percentage(df3, group_columns = c("a", "b"))
  expect_equal(names(result3), c("a", "b", "count", "percentage"))
  
  # Test 4: Dataframe with one row
  df4 <- data.frame(a = 1, b = 2, c = 3)
  result4 <- calculate_count_and_percentage(df4, group_columns = c("a", "b"))
  expect_equal(nrow(result4), 1)
  expect_equal(result4$count, 1)
  expect_equal(result4$percentage, 1)
})