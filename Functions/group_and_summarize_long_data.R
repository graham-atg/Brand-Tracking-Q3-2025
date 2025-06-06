###Notes: here's a function in R that takes a long dataframe, along with a set of 
###columns to group by (defaulting to all columns if not specified), and calculates
###the count and percentage for the last variable specified in the group by columns

library(dplyr)

group_and_summarize_long_data <- function(long_df, group_cols = NULL) {
  if (is.null(group_cols)) {
    group_cols <- names(long_df) # Use all columns by default
  }
  
  # Ensure that the group_cols are valid column names in the dataframe
  valid_group_cols <- intersect(names(long_df), group_cols)
  
  # Group the dataframe by the specified columns
  grouped_df <- long_df %>%
    group_by(across(all_of(valid_group_cols)))
  
  # Calculate count and percentage for the last variable in group_cols
  result_df <- grouped_df %>%
    summarise(count = n()) %>%
    mutate(percentage = count / sum(count))
  
  return(result_df)
}

# Example usage:
# Suppose you have a long dataframe df with columns "A", "B", "C", and "Value"
# To group by columns "A" and "B" and calculate count and percentage for "Value":
# result <- group_and_summarize_long_data(df, c("A", "B"))
# This will give you a dataframe with columns "A", "B", "count", and "percentage".
 