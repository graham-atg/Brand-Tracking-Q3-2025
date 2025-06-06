#Detailed Explanation
#Function Arguments:
    
  #data: The data frame containing the data to analyze.
  #subset_var: A string specifying the name of the factor variable used for subsetting and grouping the data. It must be a factor variable.
  #group_var: A string specifying the name of another factor variable; however, this variable is not directly used in the function but is checked for type consistency.
  
  #Additional numeric variables to include in the mean calculations, specified by their names.
  #Input Validation:
    
  #The function checks that both subset_var and group_var are factor variables. If they are not, the function stops and throws an error message, ensuring that the input is correctly formatted.
  #Selecting Numeric Variables:
    
  #The function selects all numeric columns from the data frame using select_if(is.numeric) and further narrows down to the specified variables through select(...). This subset of numeric columns is used for the mean calculations.
  #Calculating Group Means:
    
  #The function calculates the means of the selected numeric variables grouped by the subset_var. This is achieved by:
  #Binding the subset variable column to the selected numeric variables using bind_cols().
  #Grouping the data by the subset variable using group_by(!!sym(subset_var)).
  #Applying summarise() with across(everything(), mean, na.rm = TRUE) to calculate the mean for each numeric variable within each group defined by the subset variable.
  #Calculating Overall Means:
    
  #The function calculates the overall mean of the selected numeric variables for the entire data frame using summarise(across(everything(), mean, na.rm = TRUE)). This part does not consider any grouping and provides a single mean value for each numeric variable across all rows.
  #Returning Results:
    
  #The function returns a list containing two elements:
  #group_means: A data frame of means calculated for each level of the subset variable.
  #overall_means: A data frame of overall means calculated for the entire data frame.

# Load necessary package
library(dplyr)

# Define the function
compute_grouped_means <- function(data, subset_var, ...) {
  
  # Ensure the subset variable is a factor
  if (!is.factor(data[[subset_var]])) stop("subset_var must be a factor variable")
  
  # Select numeric variables for mean calculation
  numeric_vars <- data %>%
    select_if(is.numeric) %>%
    select(...)
  
  # Compute means by the subset variable, excluding NAs
  group_means <- numeric_vars %>%
    bind_cols(data[subset_var]) %>%
    group_by(!!sym(subset_var)) %>%
    summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')
  
  # Compute overall means, excluding NAs
  overall_means <- numeric_vars %>%
    summarise(across(everything(), mean, na.rm = TRUE))
  
  # Reshape the group means to a named list in the desired format
  group_means_list <- as.list(setNames(
    c(group_means[1, -1], group_means[2, -1]), 
    paste0(rep(names(numeric_vars), 2), "_", group_means[[subset_var]])
  ))
  
  # Reshape the overall means to a named list
  overall_means_list <- as.list(setNames(
    overall_means, 
    paste0(names(numeric_vars), "_overall")
  ))
  
  # Combine group means and overall means into a single list and convert to a data frame
  combined_means <- c(group_means_list, overall_means_list)
  combined_means_df <- as.data.frame(combined_means)
  
  return(combined_means_df)
}


# Create a sample data frame
df <- data.frame(
  id = 1:10,
  group = factor(c("A", "A", "B", "B", "A", "B", "A", "A", "B", "B")),
  subset_factor = factor(c("X", "X", "Y", "Y", "X", "Y", "X", "X", "Y", "Y")),
  value1 = c(5.5, 6.7, 4.2, 7.8, 5.0, 6.3, 4.9, 6.1, 5.8, 7.2),
  value2 = c(2.1, 3.4, 1.8, 2.6, 3.0, 2.4, 1.9, 3.5, 2.2, 2.7)
)

# Call the function with the sample data
result <- compute_grouped_means(df, subset_var = 'subset_factor', 'value1', 'value2')

# Print the result
print(result)

