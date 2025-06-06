#The convert_to_numeric function converts specified columns in a data frame to numeric data type. It accepts the following inputs:

#dataframe: The data frame to be modified.
#columns_to_convert: A vector containing the names of the columns that need to be converted to numeric type.
#The function first selects the columns specified by columns_to_convert from the data frame.
#It then converts these selected columns to numeric type using the mutate_if function.
#Finally, it updates the original columns in the data frame with the converted versions and returns the modified data frame.



#Function converts factor and string variables to numeric variables.
library(dplyr)

convert_to_numeric <- function(dataframe, columns_to_convert) {
  # Select only the columns that need to be converted
  selected_columns <- dataframe %>%
    select(all_of(columns_to_convert))
  
  # Convert selected columns to numeric
  converted_columns <- selected_columns %>%
    mutate_if(~ !is.numeric(.), as.numeric)
  
  # Replace the original columns in the dataframe with the converted ones
  dataframe[, columns_to_convert] <- converted_columns
  
  return(dataframe)
}

# Example usage:
# Create a sample dataframe with different data types
sample_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  NumericColumn1 = c(1, 2, 3, 4, 5),
  NumericColumn2 = c(1.1, 2.2, 3.3, 4.4, 5.5),
  FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
  CharacterColumn = c("One", "Two", "Three", "Four", "Five")
)

# Convert non-numeric columns to numeric
converted_data <- convert_to_numeric(sample_data, columns_to_convert = c("Name", "FactorColumn", "CharacterColumn"))

# Print the converted dataframe
print(converted_data)
