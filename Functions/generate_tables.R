# The generate_tables function takes three arguments: data (the input dataframe), variables (a list of variable names to generate tables for), and group_var (the factor variable for grouping).
# Inside the function, a for loop iterates over each variable in the variables list.
#	For each variable, it checks if it is numeric using is.numeric(). If true, it generates descriptive statistics using the st function from the vtable package.
#	If the variable is a factor (checked using is.factor()), it generates percentages using the pt function from the vtable package.
# If the variable is neither numeric nor factor, it raises an error using stop() with an appropriate message.
#	The resulting table is displayed using kable() for a simple formatted output.
#	The table is also saved to a CSV file using write.csv(), with the variable name as the filename prefix.
#	The function is then called with a sample dataframe (sample_data) and a list of variables (variables) to generate tables for, along with the grouping variable (group_var).


library(vtable)

generate_tables <- function(data, variables, group_var) {
  for (var in variables) {
    if (is.numeric(data[[var]])) {
      # Generate descriptive statistics for numeric variables
      result <- st(data, vars = var, group = group_var, out = "return")
    } else if (is.factor(data[[var]])) {
      # Generate percentages for factor variables
      result <- st(data, vars = var, group = group_var, out = "return")
    } else {
      stop(paste("Variable", var, "is not supported. Please provide numeric or factor variables."))
    }
    
    # Display the table
    cat("\nTable for", var, "by", group_var, "\n")
    print(kable(result, format = "simple"))
    
    # Save the table to a CSV file with variable name and group variable name
    write.csv(result, file = paste0(var, "_by_", group_var, "_table.csv"), row.names = FALSE)
  }
}

# Sample dataframe for testing
set.seed(123)
sample_data <- data.frame(
  group = factor(rep(c("A", "B"), each = 50)),
  var1 = rnorm(100),
  var2 = rpois(100, lambda = 5),
  var3 = factor(sample(c("Cat1", "Cat2", "Cat3"), 100, replace = TRUE))
)

# Function call for testing
variables <- c("var1", "var2", "var3")
group_var <- "group"
generate_tables(sample_data, variables, group_var)