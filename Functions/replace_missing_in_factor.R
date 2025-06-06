#The replace_missing_in_factor function takes a dataframe, an optional list of columns,
#and a replacement value as inputs. It loops through the specified factor columns and 
#replaces any missing values (NA) with the specified replacement value.
#The function then returns the modified dataframe.



#Loops through factor variables and replace NA with a value
replace_missing_in_factor <- function(dataframe, columns = NULL, replacement_value = -99999) {
  
  # If 'columns' argument is not provided, use all columns in the dataframe
  if (is.null(columns)) {
    columns <- names(dataframe)
  } 
  
  # Loop through the specified columns
  for (col in columns) {
    
    # Check if the column is a factor
    if (is.factor(dataframe[[col]])) {
      
      # Replace <NA> with the specified 'replacement_value'
      dataframe[[col]][is.na(dataframe[[col]])] <- replacement_value
    }
  }
  
  # Return the modified dataframe
  return(dataframe)
}

# Test the function with a sample dataframe
dataframe <- data.frame(
  Gender = factor(c("Male", "Female", NA, "Male", NA)),
  Age = c(25, 30, NA, 22, 40),
  Income = factor(c(1, 2, NA, 3, NA), labels = c("Low", "Medium", "High"))
)

# Print the original dataframe
cat("Original Dataframe:\n")
print(dataframe)

# Replace <NA> with a specified value for factor columns
dataframe_updated <- replace_missing_in_factor(dataframe, replacement_value = -99999)

# Print the updated dataframe
cat("\nUpdated Dataframe:\n")
print(dataframe_updated)

