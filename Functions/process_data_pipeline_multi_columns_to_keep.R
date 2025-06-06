library(tidyr)
library(dplyr)
library(reshape2)

# Function to transform data from wide to long format with multiple id.vars
#to_long takes a dataframe and transforms it from wide to long format using the reshape2 package. 
#It takes dataframe as input and outputs the transformed data melted_data.
to_long <- function(dataframe, id.vars, variable.name, value.name) {
  if (!is.character(id.vars)) {
    stop("id.vars must be a character vector.")
  }
  melted_data <- reshape2::melt(dataframe, id.vars = id.vars, variable.name = variable.name, value.name = value.name)
  return(melted_data)
}

# Function to factorize all columns in a dataframe

#factorize_column converts all columns in the dataframe into factors, ensuring
#that each column is a factor variable. It takes dataframe as input and outputs
#the dataframe with all columns converted to factors. a
# Function to factorize columns while maintaining the original levels order
factorize_column <- function(dataframe, columns = NULL, levels = NULL) {
  if (is.null(columns)) {
    columns <- names(dataframe)
  }
  for (col in columns) {
    if (is.factor(dataframe[[col]])) {
      next
    }
    if (is.null(levels)) {
      dataframe[[col]] <- as.factor(dataframe[[col]])
    } else {
      dataframe[[col]] <- factor(dataframe[[col]], levels = unique(dataframe[[col]]))
    }
  }
  return(dataframe)
}


# Function to calculate counts and percentages by grouping columns
# Updated calculate_count_and_percentage function to calculate counts and percentages within each level of group_columns

#calculate_count_and_percentage calculates counts and percentages by grouping columns 
#specified in group_columns. It groups the dataframe, calculates the count and percentage within each group, 
#and then ungroups the data. It takes dataframe as input and outputs the resulting calculated counts and percentages.

calculate_count_and_percentage <- function(dataframe, group_columns = NULL) {
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[c(length(dataframe) - 1, length(dataframe))]
  }
  
  result <- dataframe %>%
    group_by(!!!syms(group_columns)) %>%
    summarise(count = n()) %>%
    mutate(perc = count / sum(count)) %>%
    ungroup()  # Remove grouping
  
  return(result)
}

#new_percent calculates new percentages by multiplying the existing percentage (perc) 
#in the dataframe with the difference between total_columns and the length of id_vars. 
#It takes dataframe as input and outputs the dataframe with the new percentage added.

# Function to calculate new percentages
new_percent <- function(dataframe, id_vars, total_columns) {
  require(dplyr)
  difference <- total_columns - length(id_vars)
  dataframe <- dataframe %>%
    mutate(new_perc = perc * difference)
  return(dataframe)
}

# Updated process_data_pipeline function without total_rows argument
#process_data_pipeline_multiple_columns is an updated version of the data processing pipeline function. 
#It performs the following steps: transforming data from wide to long format, replacing NAs with a default value, 
#converting all columns to factors, ensuring the data is in a dataframe, calculating count and percentage, 
#removing rows where the value column equals 0, and finally calculating new percentages. 
#It takes dataframe as input and outputs the processed data.

process_data_pipeline_multiple_columns <- function(dataframe, id.var = NULL, default_value = 0, group_column = NULL) {
  # Step 1: Transform data from wide to long format using to_long
  df_long_data <- to_long(dataframe, id.vars = id.var, variable.name = "variable", value.name = "value")
  
  # Step 2: Replace NAs with default_value
  df_long_data <- replace(df_long_data, is.na(df_long_data), default_value)
  
  # Step 3: Factorize all columns
  df_long_data <- factorize_column(df_long_data)
  
  # Step 4: Ensure the data is in a data.frame
  df_long_data <- data.frame(df_long_data)
  
  # Step 5: Calculate count and percentage with the specified group_column
  result <- calculate_count_and_percentage(df_long_data, group_columns = group_column)
  
  # Step 6: Remove rows where value column equals 0
  result <- result[result$value != default_value, ]
  
  # Step 7: Calculate new percentages
  result <- new_percent(result, id_vars = id.var, total_columns = ncol(dataframe))
  
  return(result)
}

# Example usage of the processing pipeline with a group_column
data <- data.frame(
  ID1 = c(1, 2, 3, 1, 2, 2, 3, 3, 1, 2),
  ID2 = c(1,2,3,1,2,3,1,2,3,1),
  Value = c(10, 15, 5, 8, 3, 6, 7, 2, 3, 14)
)

# Example: Calculate counts, percentages, and new percentages by grouping on "ID1" and "ID2"
result_data <- process_data_pipeline_multiple_columns(data, id.var = c("ID1","ID2"), default_value = 0, group_column = c("ID2","value"))

# Print the result
print(result_data)




###Error bars incldue


library(tidyr)
library(dplyr)
library(reshape2)

# Function to transform data from wide to long format with multiple id.vars
to_long <- function(dataframe, id.vars, variable.name, value.name) {
  if (!is.character(id.vars)) {
    stop("id.vars must be a character vector.")
  }
  melted_data <- reshape2::melt(dataframe, id.vars = id.vars, variable.name = variable.name, value.name = value.name)
  return(melted_data)
}

# Function to factorize all columns in a dataframe
factorize_column <- function(dataframe, columns = NULL, levels = NULL) {
  if (is.null(columns)) {
    columns <- names(dataframe)
  }
  for (col in columns) {
    if (is.factor(dataframe[[col]])) {
      next
    }
    if (is.null(levels)) {
      dataframe[[col]] <- as.factor(dataframe[[col]])
    } else {
      dataframe[[col]] <- factor(dataframe[[col]], levels = unique(dataframe[[col]]))
    }
  }
  return(dataframe)
}

# Function to calculate counts and percentages by grouping columns
calculate_count_and_percentage <- function(dataframe, group_columns = NULL) {
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[c(length(dataframe) - 1, length(dataframe))]
  }
  
  result <- dataframe %>%
    group_by(!!!syms(group_columns)) %>%
    summarise(count = n()) %>%
    mutate(perc = count / sum(count)) %>%
    ungroup()  # Remove grouping
  
  return(result)
}

# Function to calculate new percentages
new_percent <- function(dataframe, id_vars, total_columns) {
  difference <- total_columns - length(id_vars)
  dataframe <- dataframe %>%
    mutate(new_perc = perc * difference)
  return(dataframe)
}

# Function to calculate standard error for new_perc
calculate_standard_error <- function(dataframe) {
  dataframe <- dataframe %>%
    mutate(se_perc = sqrt((new_perc * (1 - new_perc)) / count))
  return(dataframe)
}

# Updated process_data_pipeline function without total_rows argument
process_data_pipeline_multiple_columns <- function(dataframe, id.var = NULL, default_value = 0, group_column = NULL) {
  # Step 1: Transform data from wide to long format using to_long
  df_long_data <- to_long(dataframe, id.vars = id.var, variable.name = "variable", value.name = "value")
  
  # Step 2: Replace NAs with default_value
  df_long_data <- replace(df_long_data, is.na(df_long_data), default_value)
  
  # Step 3: Factorize all columns
  df_long_data <- factorize_column(df_long_data)
  
  # Step 4: Ensure the data is in a data.frame
  df_long_data <- data.frame(df_long_data)
  
  # Step 5: Calculate count and percentage with the specified group_column
  result <- calculate_count_and_percentage(df_long_data, group_columns = group_column)
  
  # Step 6: Remove rows where value column equals 0
  result <- result[result$value != default_value, ]
  
  # Step 7: Calculate new percentages
  result <- new_percent(result, id_vars = id.var, total_columns = ncol(dataframe))
  
  # Step 8: Calculate standard error for new_perc
  result <- calculate_standard_error(result)
  
  return(result)
}

# Example usage of the processing pipeline with a group_column
data <- data.frame(
  ID1 = c(1, 2, 3, 1, 2, 2, 3, 3, 1, 2),
  ID2 = c(1, 2, 3, 1, 2, 3, 1, 2, 3, 1),
  Value = c(10, 15, 5, 8, 3, 6, 7, 2, 3, 14)
)

# Example: Calculate counts, percentages, new percentages, and standard error by grouping on "ID1" and "ID2"
result_data <- process_data_pipeline_multiple_columns(data, id.var = c("ID1", "ID2"), default_value = 0, group_column = c("ID2", "value"))

# Print the result
print(result_data)
