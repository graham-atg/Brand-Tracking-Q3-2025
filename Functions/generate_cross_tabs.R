#The generate_cross_tabs function generates cross-tabulations for categorical variables
#in a dataframe, x_variables against a target variable y_variable, returning the cross-tabulation 
#table and a proportional table. The join_count_percentage_tables function combines the count 
#and percentage tables, and the format_apa_table function formats the table in APA style using the kableExtra package. 
#Finally, the cross_tab_pipeline function executes the entire cross-tabulation pipeline, 
#saving the formatted table to an XLSX file and returning the formatted table.

#  Function is meant to generate and bind together multiple cross-tabs.
# This should be retained.
#Load necessary libraries
library(kableExtra)
library(writexl)

# Function to generate cross-tabulations
generate_cross_tabs <- function(dataframe, y_variable, x_variables) {
  if (!all(sapply(x_variables, function(var) is.factor(dataframe[[var]])))) {
    stop("All x_variables must be factor variables.")
  }
  
  cross_tab_name <- paste(y_variable, "CROSSTAB", sep = "_")
  
  combined_categories <- apply(dataframe[, x_variables, drop = FALSE], 1, paste, collapse = " AND ")
  
  cross_tab <- table(combined_categories, dataframe[[y_variable]])
  prop_cross_tab <- prop.table(cross_tab, margin = 1) * 100
  
  return(list(name = cross_tab_name, table = cross_tab, prop_table = prop_cross_tab))
}

# Function to join count and percentage tables
join_count_percentage_tables <- function(count_table, percentage_table) {
  joined_table <- merge(count_table, percentage_table, by = "combined_categories")
  colnames(joined_table) <- c("combined_categories", "count", "percentage")
  return(joined_table)
}

# Function to format a table in APA style
format_apa_table <- function(master_table) {
  formatted_table <- kable(master_table, "latex", booktabs = TRUE) %>%
    kable_styling(latex_options = "hold_position", font_size = 10) %>%
    row_spec(0, bold = TRUE, background = "#f2f2f2")
  return(formatted_table)
}

# Function to perform the entire cross-tab pipeline
cross_tab_pipeline <- function(dataframe, y_variables, x_variables) {
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
  write_xlsx(as.data.frame(formatted_master_table), "formatted_master_table.xlsx")
  
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





# Create a larger test dataset with more variables and observations
set.seed(123)
larger_test_data <- data.frame(
  Y_Var1 = factor(sample(letters[1:3], 1000, replace = TRUE)),
  Y_Var2 = factor(sample(letters[4:6], 1000, replace = TRUE)),
  Y_Var3 = factor(sample(letters[7:9], 1000, replace = TRUE)),
  Y_Var4 = factor(sample(letters[10:12], 1000, replace = TRUE)),
  Y_Var5 = factor(sample(letters[13:15], 1000, replace = TRUE)),
  Gender = factor(sample(c("Male", "Female", "Other"), 1000, replace = TRUE)),
  Age_Group = factor(sample(c("Young", "Middle_Age", "Old", NA), 1000, replace = TRUE))
)

# Introduce missing values in some variables
larger_test_data$Y_Var2[sample(1:1000, 100, replace = FALSE)] <- NA
larger_test_data$Y_Var4[sample(1:1000, 50, replace = FALSE)] <- NA

# Define y and x variables including the newly added variables
y_variables <- c("Y_Var1", "Y_Var2", "Y_Var3", "Y_Var4", "Y_Var5")
x_variables <- c("Age_Group", "Gender")

# Execute the cross-tab pipeline with the larger test dataset
formatted_table <- cross_tab_pipeline(larger_test_data, y_variables, x_variables)

# Print the formatted table
print(formatted_table)







