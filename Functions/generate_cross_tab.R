#The generate_cross_tab function generates a cross-tabulation table with count and percentage, 
#taking as input a dataframe containing the variables, y_var as the name of the factor variable 
#for the rows, and x_var as the name of the factor variable for the columns. 
#It returns a table containing count and percentage with the specified column headings.

#The function generate_cross_tab performs the cross-tabulation of factors in the
#provided dataframe, calculates the count and percentage, and returns a combined 
#table with row and column names, as demonstrated in the example usage and testing provided.



# note that this is a good cross-tab function and should be retained
#generate_cross_tab: Generates a cross-tabulation table with count and percentage.
# Input:
#   - data: Dataframe containing the variables.
#   - y_var: Name of the factor variable for the rows.
#   - x_var: Name of the factor variable for the columns.
# Output:
#   - Returns a table containing count and percentage with specified column headings.
generate_cross_tab <- function(data, y_var, x_var) {
  # Check if y_var is a factor
  if (!is.factor(data[[y_var]])) {
    stop("The 'y' variable is not a factor. Please provide a factor variable.")
  }
  
  # Check if x_var is a factor
  if (!is.factor(data[[x_var]])) {
    stop("The 'x' variable is not a factor. Please provide a factor variable.")
  }
  
  # Generate a cross-tabulation with counts and percentages
  cross_tab <- table(data[[y_var]], data[[x_var]])
  prop_cross_tab <- prop.table(cross_tab, margin = 1) * 100
  
  # Create row names for the count and percentage tables
  row_names <- rownames(cross_tab)
  
  # Create column names for the percentage table with "%" symbol
  col_names_perc <- colnames(prop_cross_tab)
  col_names_perc <- paste0(col_names_perc, "%")
  
  # Create the combined table with row and column names
  combined_table <- cbind(
    Row_Label = row_names,
    Count = as.matrix(cross_tab),
    prop_cross_tab
  )
  
  colnames(combined_table)[(ncol(combined_table) - ncol(prop_cross_tab) + 1):ncol(combined_table)] <- col_names_perc
  return(combined_table)
}

# Example usage and testing
data <- data.frame(
  y_var = factor(c("A", "B", "A", "B", "A")),
  x_var = factor(c("X", "Y", "X", "Y", "Z"))
)

# Generate a cross-tabulation
cross_tab_data <- generate_cross_tab(data, "y_var", "x_var")

# Print the resulting cross-tabulation
print("Cross-Tabulation:")
print(cross_tab_data)
