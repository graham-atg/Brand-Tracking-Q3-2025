#The replace_NA_with_value function replaces NA values in a dataframe with a specified value,
#allowing the option to target specific columns. It takes a dataframe, optional column names, 
#and a replacement value as inputs. If no columns are specified, it replaces NAs in the entire dataframe. 
#If the column is a factor, it converts it to character type, replaces NAs, and re-factors it, and for non-factor columns, 
#it replaces NAs directly.

# Function to replace NA values in a dataframe with a specified value
replace_NA_with_value <- function(dataframe, columns = NULL, replace_value = 0) {
  if (is.null(columns)) {
    # If no columns are specified, replace NA values in the entire dataframe
    dataframe[is.na(dataframe)] <- replace_value
  } else {
    # Replace NA values in the specified columns
    for (col in columns) {
      if (col %in% names(dataframe)) {
        if (is.factor(dataframe[[col]])) {
          # If the column is a factor, convert it to character, replace NAs, and re-factor
          dataframe[[col]] <- as.character(dataframe[[col]])       
          dataframe[is.na(dataframe[[col]]), col] <- as.character(replace_value)
          dataframe[[col]] <- factor(dataframe[[col]])
        } else {
          # For non-factor columns, replace NAs directly
          dataframe[is.na(dataframe[[col]]), col] <- replace_value
        }
      } else {
        warning(paste("Column", col, "not found in the dataframe. Skipping..."))
      }
    }
  }
  return(dataframe)
}

# Tester code
# Create a sample dataframe
sample_data <- data.frame(
  A = c(1, 2, NA, 4, 5),
  B = c(NA, 2, 3, 4, NA),
  C = c(NA, NA, NA, NA, NA)
)

# Replace NA values in columns A and B with 999
result <- replace_NA_with_value(sample_data, columns = c("A", "B"), replace_value = 999)

# Print the result dataframe
print(result)
