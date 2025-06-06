#Arguments/Inputs:

# data: This is the input dataframe that you want to filter.
# column_names: This is a vector of column names based on which the filtering will be applied.
# values: This is a vector of values that each corresponding column name must equal in order for the row to be retained.
#Explanation:
  #The function filters the input dataframe based on the specified conditions. 
#It creates two resulting dataframes: one containing the cases that do not meet 
#the conditions and one containing the cases that meet the conditions. 
#It appends _to_be_removed and _to_be_retained to the resulting dataframes' names for distinction. 
#The function then assigns these dataframes to the global environment and returns a list containing the two dataframes.

# Outputs:
# The function returns a list containing two dataframes:

# data_to_be_removed: This dataframe contains the cases that do not meet the specified conditions.
# data_to_be_retained: This dataframe contains the cases that meet the specified conditions.
#You can use this function to filter out specific cases from your dataframe based 
#on certain conditions and then work with the resulting dataframes accordingly.
filtered_data <- function(data, column_names, conditions) {
  if(length(column_names) != length(conditions)){
    stop("Length of column names and conditions should be the same.")
  }
  
  filter_condition <- rep(TRUE, nrow(data))
  
  for (i in 1:length(column_names)) {
    if (is.na(conditions[i])) {
      filter_condition <- filter_condition & is.na(data[[column_names[i]]])
    } else {
      filter_condition <- filter_condition & (data[[column_names[i]]] == conditions[i])
    }
  }
  
  filtered_data <- data[filter_condition, ]
  return(filtered_data)
}


# Example dataframe
df <- data.frame(
  A = c(1, 2, 3, 4),
  B = c(5, 6, 7, 8),
  C = c(9, 10, 11, 12)
)

# Filter data
result <- filter_data(df, c("A", "B"), c(2, 6))
to_be_removed <- result[[1]]
to_be_retained <- result[[2]]

# Printing the resulting dataframes
print(to_be_removed)
print(to_be_retained)

