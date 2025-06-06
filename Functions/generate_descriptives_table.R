#####Not sure if this function works

#Descriptive Statistics Table Generation Pipeline
#This pipeline is designed to generate descriptive statistics tables for continuous
#Y variables based on user-defined categorical X variables. It performs the following steps:
  
#Data Validation: It first checks if the provided X variables are factor variables. 
#If any of the X variables are not factors, it raises an error indicating that all 
#X variables must be factor variables.


#Table Generation: For each Y variable in the list of Y variables, the pipeline 
#generates a descriptive statistics table. This table includes statistics such as
#mean, median, standard deviation, maximum, minimum, and quartiles (1st and 3rd quartiles) for the Y variable, broken down by categories defined by the X variables.

#Result Storage: The results for each Y variable, including the descriptive
#statistics tables, are stored in a list, making it easy to access and analyze the results.

#Formatting (Optional): An optional step is provided to format the results in APA 
#(American Psychological Association) style using the kableExtra package.
#This step improves the presentation of the descriptive statistics tables.

#File Export: After generating and optionally formatting the tables, the pipeline 
#allows for the export of the formatted tables to XLSX files, each named after the corresponding Y variable.

#Overall, this pipeline simplifies the process of generating descriptive statistics 
#tables for multiple Y variables based on categorical X variables, making it easy to
#conduct statistical analyses and present results in a standardized format.

# Load necessary libraries
library(dplyr)
library(kableExtra)
library(writexl)

# Function to generate descriptive statistics table
generate_descriptives_table <- function(dataframe, y_variable, x_variables) {
  if (!all(sapply(x_variables, function(var) is.factor(dataframe[[var]])))) {
    stop("All x_variables must be factor variables.")
  }
  
  descriptives_table_name <- paste(y_variable, "DESCRIPTIVES_TABLE", sep = "_")
  
  # Group by combined categories and compute statistics
  descriptives_table <- dataframe %>%
    group_by(across(all_of(x_variables))) %>%
    summarize(
      mean = mean(.data[[y_variable]], na.rm = TRUE),
      median = median(.data[[y_variable]], na.rm = TRUE),
      std_dev = sd(.data[[y_variable]], na.rm = TRUE),
      max = max(.data[[y_variable]], na.rm = TRUE),
      min = min(.data[[y_variable]], na.rm = TRUE),
      q1 = quantile(.data[[y_variable]], probs = 0.25, na.rm = TRUE),
      q3 = quantile(.data[[y_variable]], probs = 0.75, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(list(name = descriptives_table_name, table = descriptives_table))
}

# Function to join count and percentage tables
join_count_percentage_tables <- function(count_table, percentage_table) {
  joined_table <- merge(count_table, percentage_table, by = "combined_categories")
  colnames(joined_table) <- c("combined_categories", "count", "percentage")
  return(joined_table)
}

# Function to perform the entire pipeline
descriptives_table_pipeline <- function(dataframe, y_variables, x_variables) {
  # Generate descriptive statistics table for each y variable
  result_list <- list()
  
  for (y_var in y_variables) {
    descriptives_table <- generate_descriptives_table(dataframe, y_var, x_variables)
    
    result_list[[descriptives_table$name]] <- list(
      descriptives_table = descriptives_table
    )
  }
  
  return(result_list)
}

# Test data
set.seed(123)
test_data <- data.frame(
  Y_Var1 = runif(100),
  Y_Var2 = runif(100),
  Gender = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
  Age_Group = factor(sample(c("Young", "Middle_Age", "Old"), 100, replace = TRUE))
)

# Define y and x variables
y_variables <- c("Y_Var1", "Y_Var2")
x_variables <- c("Age_Group", "Gender")

# Execute the descriptives table pipeline
results <- descriptives_table_pipeline(test_data, y_variables, x_variables)

# Print the results (descriptive statistics tables)
for (result_name in names(results)) {
  cat("Result for", result_name, ":\n")
  cat("Descriptive Statistics Table:\n")
  print(results[[result_name]]$descriptives_table$table)
  cat("\n")
}

# Function to format the results in APA style
format_apa_results <- function(results) {
  formatted_results <- lapply(results, function(result) {
    formatted_descriptives_table <- format_apa_table(result$descriptives_table$table)
    return(list(descriptives_table = formatted_descriptives_table))
  })
  
  return(formatted_results)
}

# Format the results in APA style
formatted_results <- format_apa_results(results)
print(formatted_results)
# Save the formatted tables to XLSX files
for (result_name in names(formatted_results)) {
  write_xlsx(as.data.frame(formatted_results[[result_name]]$descriptives_table), paste(result_name, "descriptives_table.xlsx", sep = "_"))
}
