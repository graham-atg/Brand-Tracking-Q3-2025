# Load necessary libraries
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

# Function to perform the entire cross-tab pipeline and return a dataframe
updated_cross_tab_pipeline <- function(dataframe, y_variables, x_variables, output_csv_filename) {
  # Generate cross-tabulations for each y variable
  cross_tabs_list <- lapply(y_variables, function(y_var) {
    generate_cross_tabs(dataframe, y_var, x_variables)
  })
  
  # Join count and percentage tables
  joined_tables_list <- lapply(cross_tabs_list, function(cross_tab) {
    join_count_percentage_tables(cross_tab$table, cross_tab$prop_table)
  })
  
  # Combine the joined tables into a master dataframe
  master_dataframe <- do.call(rbind, joined_tables_list)
  
  # Save the dataframe as a CSV
  write.csv(master_dataframe, output_csv_filename, row.names = FALSE)
  
  return(master_dataframe)
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

# Define the output CSV filename
output_csv_filename <- "ga_test_table.csv"

p<-updated_cross_tab_pipeline(test_data, y_variables, x_variables, output_csv_filename)
p