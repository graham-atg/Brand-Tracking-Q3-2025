# Description: Create descriptive statistics tables for continuous variables in R using the gtsummary package.
# This function takes a dataframe, a vector of column names, an optional grouping variable(s), and an optional output file name.
# It produces summary tables with statistics like mean and standard deviation, and formats them with specified styles.
# Load the summarytools package
library(summarytools)
 
# Create a function to generate descriptive statistics tables
create_summary_table <- function(dataframe, columns, group_vars_by = NULL, output_file = NULL) {
  # Subset the data to include only the specified columns
  data_subset <- dataframe[, c(columns, group_vars_by)]
  
  # Check if grouping variable(s) are specified
  if (is.null(group_vars_by)) {
    # If not, summarize the provided columns without grouping
    table <- freq(data_subset,
                  style = "grid",
                  row.label = NULL,
                  row.display = NULL,
                  stats = "common")
  } else {
    # If grouping variable(s) are specified, group and summarize the columns accordingly
    table <- crosstab(data_subset,
                      cell.stats = "col",
                      test = FALSE,
                      prop.r = FALSE,
                      prop.c = FALSE,
                      prop.t = FALSE,
                      digits = c(0, 2),
                      table.title = "Descriptive Statistics by Group",
                      table.theme = 5)
  }
  
  # Set table dimensions to 4.5" in height and 10" in width
  table <- st_options(table,
                      table.theme = 5,
                      tstyle = "striped",
                      tfoot = NULL,
                      caption = NULL,
                      width = 10,
                      ffooter = NULL,
                      padding = 1)
  
  # Save the table as an HTML file if an output file name is provided
  if (!is.null(output_file)) {
    st_export(table, output_file)
  }
  
  return(table)
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  Group1 = rep(c("A", "B", "C"), each = 4),
  Group2 = rep(c("X", "Y", "Z"), times = 4),
  Variable1 = rnorm(12),
  Variable2 = rnorm(12),
  Variable3 = rnorm(12)
)

# Create a summary table for specific columns with grouping
summary_table1 <- create_summary_table(sample_data, c("Variable1", "Variable2", "Variable3"), group_vars_by = c("Group1", "Group2"), output_file = "summary_table1.html")

# Create a summary table for specific columns without grouping
summary_table2 <- create_summary_table(sample_data, c("Variable1", "Variable2", "Variable3"))

# View the summary tables
print(summary_table1)
print(summary_table2)
