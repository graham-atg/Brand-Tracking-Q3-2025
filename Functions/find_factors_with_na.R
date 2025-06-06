#The find_factors_with_na function identifies factor variables with <NA> values in a dataframe.
#It takes a dataframe as input and initializes an empty character vector. 
#It then loops through the columns in the dataframe, checking if each column is a
#factor and if it contains any <NA> values. If a factor variable with <NA> values is found,
#it adds the column name to the vector. 
#Finally, the function returns the vector containing the names of factor variables with <NA> values.



# Function to identify factor variables with <NA> values in a dataframe
find_factors_with_na <- function(dataframe) {
  factor_vars_with_na <- character(0)  # Initialize an empty character vector
  
  # Loop through columns in the dataframe
  for (col in names(dataframe)) {
    if (is.factor(dataframe[[col]]) && any(is.na(dataframe[[col]]))) {
      factor_vars_with_na <- c(factor_vars_with_na, col)  # Add column name to the vector
    }
  }
  
  return(factor_vars_with_na)
}

# Test the function with a sample dataframe
dataframe <- data.frame(
  "Gender" = factor(c("Male", "Female", NA, "Female")),
  "Age" = c(25, 30, 22, 40),
  "Income" = factor(c(1, 2, 3, 2), labels = c("Low", "Medium", "High")),
  "Education" = factor(c("Bachelor's", "Master's", NA, "Ph.D"))
)

# Get a vector of factor variable names with <NA> values
factor_vars_with_na <- find_factors_with_na(dataframe)

# Print the factor variable names with <NA>
cat("Factor Variables with <NA>:\n")
cat(factor_vars_with_na, "\n")
