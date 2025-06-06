#The compute_percentage function takes a dataframe, an identifier variable or variables, 
#and variable names for 'variable' and 'value'. It calculates the count and percentage based 
#on the provided variables, and returns the resulting dataframe.

#Note computes percentage from long dataframes.
library(dplyr)

compute_percentage <- function(data, id_vars, variable.name = "variable", value.name = "value") {
  result <- data %>%
    group_by({{ id_vars }}, {{ variable.name }}) %>%
    summarise(
      count = n(),
      percent = (sum({{ value.name }}) / sum(data[["value"]])) * 100
    ) %>%
    ungroup()
  
  return(result)
}

# Example usage:
#percentage_data <- compute_percentage(long_data, id_vars = c("id_var1", "id_var2"), variable.name = "new_variable_name", value.name = "new_value_name")
