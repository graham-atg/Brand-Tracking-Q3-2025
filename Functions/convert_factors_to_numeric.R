#The convert_numeric_to_factors function converts specific numeric columns in a 
#dataframe to factor variables. It checks if the input is a dataframe and if the 
#specified columns are present in the dataframe. It then converts the specified 
#numeric columns to factors. 
#If any column is not numeric, a warning is issued, and if a specified column is not 
#found in the dataframe, it is skipped. The function returns the modified dataframe.

#Converts factors to numeric variables 
convert_numeric_to_factors <- function(dataframe, numeric_columns) {
  if (!is.data.frame(dataframe)) {
    stop("Input is not a dataframe.")
  }
  
  for (col_name in numeric_columns) {
    if (col_name %in% colnames(dataframe)) {
      if (is.numeric(dataframe[[col_name]])) {
        dataframe[[col_name]] <- as.factor(dataframe[[col_name]] - 1)
      } else {
        warning(paste("Column", col_name, "is not numeric. Skipping."))
      }
    } else {
      warning(paste("Column", col_name, "not found in the dataframe. Skipping."))
    }
  }
  
  return(dataframe)
}
# Create a sample dataframe with factor columns
data <- data.frame(
  Factor1 = factor(c("A", "B", "A")),
  Factor2 = factor(c("X", "Y", "Z")),
  Numeric1 = c(1, 2, 3)
)

# List of factor columns to convert
factor_columns_to_convert <- c("Factor1", "Factor2")

# Convert the specified factor columns to numeric (subtracting 1)
data <- convert_numeric_to_factors(data, factor_columns_to_convert)

# Check the result
print(data)

