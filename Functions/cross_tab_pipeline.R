# Load necessary libraries
library(kableExtra)
library(writexl)

#' Generate cross-tabulations for categorical variables in a dataframe.
#' 
#' @param dataframe The dataframe containing the data.
#' @param y_variable The target variable for cross-tabulation.
#' @param x_variables A vector of predictor variables for cross-tabulation.
#' @return A list containing the cross-tabulation table and proportional table.
generate_cross_tabs <- function(dataframe, y_variable, x_variables) {
  if (!all(sapply(x_variables, function(var) is.factor(dataframe[[var]])))) {
    stop("All x_variables must be factor variables.")
  }
  
  cross_tab_name_base <- paste(y_variable, "CROSSTAB", sep = "_")
  
  combined_categories <- apply(dataframe[, x_variables, drop = FALSE], 1, paste, collapse = " AND ")
  
  cross_tab <- table(combined_categories, dataframe[[y_variable]])
  prop_cross_tab <- prop.table(cross_tab, margin = 1) * 100
  
  return(list(name = cross_tab_name_base, table = cross_tab, prop_table = prop_cross_tab))
}

#' Join count and percentage tables.
#' 
#' @param count_table The count table.
#' @param percentage_table The percentage table.
#' @return The joined table.
join_count_percentage_tables <- function(count_table, percentage_table) {
  joined_table <- merge(count_table, percentage_table, by = "combined_categories")
  colnames(joined_table) <- c("combined_categories", "count", "percentage")
  return(joined_table)
}

#' Format a table in APA style.
#' 
#' @param master_table The table to be formatted.
#' @return The formatted table.
format_apa_table <- function(master_table) {
  formatted_table <- kable(master_table, "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = "hold_position", font_size = 10) %>%
    row_spec(0, bold = TRUE, background = "#f2f2f2")
  return(formatted_table)
}

#' Perform the entire cross-tab pipeline.
#' 
#' @param dataframe The dataframe containing the data.
#' @param y_variables A vector of target variables for cross-tabulation.
#' @param x_variables A vector of predictor variables for cross-tabulation.
#' @param output_file The filename for the output XLSX file.
#' @return The formatted table.
cross_tab_pipeline <- function(dataframe, y_variables, x_variables, output_file = "formatted_master_table.xlsx") {
  # Generate cross-tabulations for each y variable
  cross_tabs_list <- lapply(y_variables, function(y_var) {
    generate_cross_tabs(dataframe, y_var, x_variables)
  })
  
  # Join count and percentage tables
  joined_tables_list <- lapply(cross_tabs_list, function(cross_tab) {
    join_count_percentage_tables(cross_tab$table, cross_tab$prop_table)
  })
  
  # Combine the joined tables into a master table
  master_table <- do.call(rbind, joined_tables_list)
  
  # Format the master table in APA style
  formatted_master_table <- format_apa_table(master_table)
  
  # Save the formatted table to an XLSX file
  write_xlsx(as.data.frame(formatted_master_table), output_file)
  
  return(formatted_master_table)
}

# Test data
set.seed(123)
test_data <- data.frame(
  Y_Var1 = factor(sample(letters[1:3], 100, replace = TRUE)),
  Y_Var2 = factor(sample(letters[4:6], 100, replace = TRUE)),
  Y_Var3 = factor(sample(letters[7:9], 100, replace = TRUE)),
  Gender = factor(sample(c("Male", "Female"), 100, replace = TRUE)),
  Age_Group = factor(sample(c("Young", "Middle_Age", "Old"), 100, replace = TRUE))
)

# Define y and x variables
y_variables <- c("Y_Var1", "Y_Var2", "Y_Var3")
x_variables <- c("Age_Group", "Gender")

# Execute the cross-tab pipeline
formatted_table <- cross_tab_pipeline(test_data, y_variables, x_variables)

# Print the formatted table
print(formatted_table)
