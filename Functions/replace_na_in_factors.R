#The replace_na_in_factors function takes a dataframe, a user-specified replacement value, 
#and an optional list of factor columns as inputs. It identifies factor columns with 
#missing values and replaces the missing values with the specified replacement value, then returns the updated dataframe.

# Function to replace <NA> with a user-specified value in identified factor columns
replace_na_in_factors <- function(dataframe, replace_value, factor_columns = NULL) {
  if (is.null(factor_columns)) {
    factor_columns <- find_factors_with_na(dataframe)
  }
  
  for (col in factor_columns) {
    dataframe[[col]] <- replace(dataframe[[col]], is.na(dataframe[[col]]), replace_value)
  }
  
  return(dataframe)
}

# Test the function with a sample dataframe
dataframe <- data.frame(
  "Gender" = factor(c("Male", "Female", NA, "Female")),
  "Age" = c(25, 30, 22, 40),
  "Income" = factor(c(1, 2, 3, 2), labels = c("Low", "Medium", "High")),
  "Education" = factor(c("Bachelor's", "Master's", NA, "Ph.D"))
)

# Replace <NA> with a specified value (e.g., "Missing") in identified factor columns
replace_value <- "Missing"
factor_columns <- find_factors_with_na(dataframe)
dataframe <- replace_na_in_factors(dataframe, replace_value, factor_columns)

# Print the updated dataframe
print(dataframe)
 