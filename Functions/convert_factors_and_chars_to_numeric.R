# Explanation of the Updated Function:
# Factor and Character Conversion: The function now checks if a column is either a factor or a character and converts it to numeric.
# Handling Non-Numeric Characters: If a character column contains non-numeric values (e.g., "X", "Y", "Z"), attempting to convert it to numeric will result in NA values.

convert_factors_and_chars_to_numeric <- function(data, columns) {
  
  # Iterate over each column name provided in the 'columns' vector
  for (col in columns) {
    
    # Check if the column is a factor or character
    if (is.factor(data[[col]]) || is.character(data[[col]])) {
      
      # Convert the factor or character to numeric by first converting to character (if factor), then to numeric
      data[[col]] <- as.numeric(as.character(data[[col]]))
      
    } else {
      # Issue a warning if the column is neither a factor nor a character
      warning(paste("Column", col, "is not a factor or character and was not converted."))
    }
  }
  
  # Return the modified data frame
  return(data)
}

# Load necessary libraries
library(dplyr)

# Sample data frame
df <- data.frame(
  a = factor(c("1", "2", "3")),
  b = factor(c("4", "5", "6")),
  c = c("7", "8", "9"),           # Character column with numeric values
  d = factor(c("10", "11", "12")), # Another factor column
  e = c("X", "Y", "Z"),            # Character column with non-numeric values
  stringsAsFactors = TRUE           # Ensure factors are created
)

# View the data frame before conversion
print("Data frame before conversion:")
print(df)
str(df)

# Test the function by converting columns 'a', 'b', 'c', and 'd' to numeric
df_converted <- convert_factors_and_chars_to_numeric(df, c("a", "b", "c", "d", "e"))

# View the data frame after conversion
print("Data frame after conversion:")
print(df_converted)
str(df_converted)
