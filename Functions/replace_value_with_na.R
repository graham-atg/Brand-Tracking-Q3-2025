#replace_value_with_na function takes a dataframe and replaces predefined values with NA.
#If columns and target_value are not specified, it replaces all values with NA in the entire dataframe. 
#If only columns are specified, it replaces all values with NA in the specified columns. 
#If both columns and target_value are provided, it replaces the specified target_value with NA in the specified columns. 
#The inputs for this function are dataframe (the input dataframe), columns 
#(an optional vector specifying the columns where values need to be replaced with NA), 
#and target_value (the value to be replaced with NA).

#The replace_value_with_na function provides flexibility in replacing specific values 
#with NA in the dataframe, either in the entire dataframe, specific columns, or for specific target values.


#Note this function replaces prespecified values with NA.
replace_value_with_na <- function(dataframe, columns = NULL, target_value = NULL) {
  if (is.null(columns)) {
    if (is.null(target_value)) {
      # Replace all values with NA in the entire dataframe
      dataframe[] <- NA
    } else {
      # Replace the target value with NA in the entire dataframe
      dataframe[dataframe == target_value] <- NA
    }
  } else {
    for (col in columns) {
      if (is.null(target_value)) {
        # Replace all values with NA in the specified column
        dataframe[, col] <- NA
      } else {
        # Replace the target value with NA in the specified column
        dataframe[dataframe[, col] == target_value, col] <- NA
      }
    }
  }
  return(dataframe)
}

# Example usage:
# Create a sample dataframe
df <- data.frame(
  column1 = c(1, 2, 99, 4, 99, 6, 7, 8),
  column2 = c(99, 2, 3, 4, 99, 6, 99, 8)
)

# Replace all values with NA in the entire dataframe
result_df1 <- replace_value_with_na(df)

# Replace all occurrences of 99 with NA in the entire dataframe
result_df2 <- replace_value_with_na(df, target_value = 99)

# Replace all values with NA in column1
result_df3 <- replace_value_with_na(df, columns = "column1")

# Print the results
print(result_df1)
print(result_df2)
print(result_df3)
  