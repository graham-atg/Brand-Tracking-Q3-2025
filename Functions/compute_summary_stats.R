#The compute_summary_stats function computes the mean, standard deviation, and standard error 
#of a continuous variable by a categorical variable in a given dataframe. 
#It groups the dataframe by the specified categorical variable, calculates the summary statistics, and returns the results.

#computes mean,sd and se from a continuous variable by a categorical variables
compute_summary_stats <- function(dataframe, categorical_var, continuous_var) {
  # Group the dataframe by the categorical variable
  grouped_data <- dataframe %>%
    group_by({{ categorical_var }})
  
  # Calculate mean, standard deviation, and standard error
  summary_stats <- grouped_data %>%
    summarise(
      Mean = mean({{ continuous_var }}, na.rm = TRUE),
      Median = median({{ continuous_var }}, na.rm = TRUE),
      SD = sd({{ continuous_var }}, na.rm = TRUE),
      SE = sd({{ continuous_var }}, na.rm = TRUE) / sqrt(n()),
      Min = min({{ continuous_var }}, na.rm = TRUE),
      Max = max({{ continuous_var }}, na.rm = TRUE)
    ) %>%
    ungroup()
  
  return(summary_stats)
}

# Sample dataframe
data_df <- data.frame(
  Category = rep(c("A", "B", "C", "D"), each = 4),
  Value = rnorm(16, mean = 50, sd = 10)
)

# Calculate summary statistics
result <- compute_summary_stats(data_df, categorical_var = Category, continuous_var = Value)

# View the result
print(result)
