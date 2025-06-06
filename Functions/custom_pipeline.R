# Explanation of the Custom Pipeline:
# This pipeline processes a given dataframe, transforms it from wide to long format, calculates count and percentage,
# and performs additional manipulations based on user-defined parameters such as variable columns, levels, labels,
# and positive/negative ratings.

# Load required libraries
# Load required libraries
# Load required libraries
library(tidyverse)
library(scales)
library(reshape2)


# Main pipeline function
custom_pipeline <- function(
    dataframe,
    variable_columns,
    Rating
) {
  dataframe %>%
    pivot_longer(cols = all_of(variable_columns), names_to = "Variable", values_to = "Value") %>%
    group_by(Variable, Value) %>%
    summarise(Count = n()) %>%
    ungroup() %>%
    mutate(
      Proportion = if_else(
        Value %in% Rating,
        Count / sum(Count),
        NA_real_
      ),
      Percentage = scales::percent(Proportion, accuracy = 1)
    ) %>%
    arrange(Variable)
}

# Example usage of the custom_pipeline function
set.seed(123)
test_df <- data.frame(
  response_id = 1:100,
  q1 = sample(c("A", "B", "C", "D"), 100, replace = TRUE),
  q2 = sample(c("X", "Y", "Z"), 100, replace = TRUE),
  q3 = sample(c("Yes", "No"), 100, replace = TRUE)
)

custom_result <- custom_pipeline(
  dataframe = test_df,
  variable_columns = c("q1", "q2", "q3"),
  Rating = c("A", "X", "Yes")
)

print(custom_result)
