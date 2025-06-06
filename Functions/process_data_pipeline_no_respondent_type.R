# This processing pipeline combines three functions to transform and process data.
# 1. It starts by converting the data from wide to long format using the to_long function.
# 2. Then it replaces NAs in the resulting dataframe with a specified default value.
# 3. Next, it factors all columns in the dataframe using the factorize_column function.
# 4. Finally, it calculates counts and percentages by grouping the last two columns (variable and value)
#    using the calculate_count_and_percentage function.

library(tidyr)
library(dplyr)
library(reshape2)

# Function to transform data from wide to long format with multiple id.vars
to_long <- function(dataframe, id.vars, variable.name, value.name) {
  # Check if id.vars is a character vector
  if (!is.character(id.vars)) {
    stop("id.vars must be a character vector.")
  }
  
  # Use reshape2::melt to melt the dataframe from wide to long format
  melted_data <- reshape2::melt(dataframe, id.vars = id.vars, variable.name = variable.name, value.name = value.name)
  
  return(melted_data)
}

# Function to factorize all columns in a dataframe
factorize_column <- function(dataframe, columns = NULL) {
  if (is.null(columns)) {
    columns <- names(dataframe)
  }
  
  for (col in columns) {
    if (is.factor(dataframe[[col]])) {
      next
    }
    
    dataframe[[col]] <- as.factor(dataframe[[col]])
  }
  
  return(data.frame(dataframe))
}

# Function to calculate counts and percentages by grouping columns
# Updated calculate_count_and_percentage function with nrow argument
calculate_count_and_percentage <- function(dataframe, group_columns = NULL, total_rows = NULL) {
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[c(length(dataframe) - 1, length(dataframe))]
  }
  
  if (is.null(total_rows)) {
    total_rows <- nrow(dataframe)
  }
  
  result <- dataframe %>%
    group_by(!!!syms(group_columns)) %>%
    summarise(count = n()) %>%
    mutate(perc = count / total_rows)
  
  return(data.frame(result))
}


# Tester code
# Example dataframe
data <- data.frame(
  ID = c(1, 2, 3, 1, 2, 2, 3, 3),
  Value = c(10, 15, 5, 8, 3, 6, 7, 2)
)

# Calculate counts and percentages by grouping on "ID" column
result <- calculate_count_and_percentage(data, group_columns = "ID")

# Print the result
print(result)

# Updated process_data_pipeline function with total_rows argument
process_data_pipeline_no_respondent_type <- function(dataframe, id.var = NULL, default_value = 0, group_column = NULL, total_rows = NULL) {
  # Step 1: Transform data from wide to long format using to_long
  df_long_data <- to_long(dataframe, id.vars = id.var, variable.name = "variable", value.name = "value")
  
  # Step 2: Replace NAs with default_value
  df_long_data <- replace(df_long_data, is.na(df_long_data), default_value)
  
  # Step 3: Factorize all columns
  df_long_data <- factorize_column(df_long_data)
  
  # Step 4: Ensure the data is in a data.frame
  df_long_data <- data.frame(df_long_data)
  
  # Step 5: Calculate count and percentage with the specified group_column and total_rows
  result <- calculate_count_and_percentage(df_long_data, group_columns = group_column, total_rows = total_rows)
  
  # Step 6: Remove rows where value column equals 0
  result <- result[result$value != default_value, ]
  
  return(result)
}

# Example usage of the processing pipeline with a group_column
data <- data.frame(
  ID1 = c(1, 2, 3, 1, 2, 2, 3, 3),
  ID2 = c("A", "B", "A", "B", "A", "B", "A", "B"),
  Value = c(10, 15, 5, 8, 3, 6, 7, 2)
)

# Example: Calculate counts and percentages by grouping on "ID1" and "ID2"
result_data <- process_data_pipeline_no_respondent_type(data, id.var = c("ID1", "ID2"), default_value = 0, group_column = c("ID2"))

# Print the result
print(result_data)