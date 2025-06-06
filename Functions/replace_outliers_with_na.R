#The function returns a modified dataframe where outliers in the specified columns
#have been replaced with NA values. If a specified column is
#not found in the dataframe or is not numeric, a warning message is displayed.
replace_outliers_with_na <- function(data, col_names) {
  for (col_name in col_names) {
    # Check if the column exists in the dataframe
    if (col_name %in% names(data)) {
      # Check if the column is numeric
      if (is.numeric(data[[col_name]])) {
        # Calculate the lower and upper bounds based on the interquartile range (IQR)
        Q1 <- quantile(data[[col_name]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[col_name]], 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR
        upper_bound <- Q3 + 1.5 * IQR
        
        # Identify values outside the interquartile range and replace with NA
        outliers <- which(data[[col_name]] < lower_bound | data[[col_name]] > upper_bound)
        data[[col_name]][outliers] <- NA
      } else {
        cat(paste("Warning: Column", col_name, "is not numeric.\n"))
      }
    } else {
      cat(paste("Warning: Column", col_name, "not found in the dataframe.\n"))
    }
  }
  
  return(data)
}

# Example usage:
# Assuming df is your dataframe
# Replace "your_column1" and "your_column2" with the actual column names you want to process
df <- replace_outliers_with_na(df, c("your_column1", "your_column2"))


# Example usage:
# Assuming df is your dataframe
# Replace "your_column1" and "your_column2" with the actual column names you want to process

# Create a test dataframe
set.seed(123)
test_data <- data.frame(
  A = rnorm(100),
  B = rnorm(100),
  C = rnorm(100),
  D = rnorm(100)
)

# Add outliers to column 'A'
test_data$A[c(5, 20, 30)] <- c(10, -8, 12)

# Print the original dataframe
print("Original Dataframe:")
print(test_data)

# Function call to replace outliers with NA in columns 'A' and 'B'
test_data_modified <- replace_outliers_with_na(test_data, c("A", "B"))

# Print the modified dataframe
print("Modified Dataframe:")
print(test_data_modified)

