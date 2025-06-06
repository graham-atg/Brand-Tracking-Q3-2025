
#calculate_count_and_percentage function takes a dataframe and generates the count 
#and percentage of the last column input into the function, grouped by the columns 
#specified in group_columns. If no columns are specified, it defaults to the last two columns of the dataframe.
#The inputs for this function are dataframe (the input dataframe) and an optional 
#vector group_columns specifying the columns by which the data should be grouped.

#The calculate_count_and_percentage function performs grouping, counting the occurrences, 
#and calculating the percentage of the last column, returning the results in a dataframe.


#This function takes as its input a dataframe and columns. It generates the count 
# and percentage of the last column input into the function grouped by the others


library(dplyr)

calculate_count_and_percentage <- function(dataframe, group_columns = NULL) {
  
  # Check if the input dataframe has at least two columns
  if (ncol(dataframe) < 2) {
    stop("The input dataframe must have at least two columns.")
  }
  
  # If group_columns is not provided, use the last two columns
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[-(1:(ncol(dataframe) - 1))]
  }
  
  # Calculate the count and percentage
  result <- dataframe %>%
    group_by(across(all_of(group_columns))) %>%
    summarize(count = n(), 
              percentage = n() / nrow(dataframe)) %>%
    ungroup()
  
  return(result)
}

# Sample long dataframe
data <- data.frame(
  Variable = c("A", "A", "B", "B", "C", "C"),
  Value = c("X", "Y", "X", "Z", "Y", "Z")
)

# Calculate count and percentage
result <- calculate_count_and_percentage(data, group_columns = c("Variable", "Value"))
print(result)