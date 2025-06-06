#The compute_mean_and_se function computes the mean and standard error for all columns
#except the first two (ID and Name) in a dataframe, taking dataframe as an input and an 
#optional column_names parameter for specific columns.

#The compute_mean_and_se_grouped function, which utilizes the dplyr package, computes 
#the mean and standard error grouped by a categorical variable, taking dataframe as an 
#input and optional parameters such as column_names for specific columns and groupby for the grouping variable. 
#This function also checks for numeric columns and skips non-numeric ones.


#Compute mean and SE for all columns except the first two (ID and Name)
compute_mean_and_se <- function(dataframe, column_names = NULL) {
  # If column_names is not specified, select all columns except the first two
  if (is.null(column_names)) {
    column_names <- names(dataframe)[3:length(dataframe)]
  }
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame(Column = character(0), Mean = numeric(0), SE = numeric(0))
  
  # Loop through the specified column names
  for (col_name in column_names) {
    # Check if the column is a factor
    if (is.factor(dataframe[[col_name]])) {
      # Convert factor to numeric
      dataframe[[col_name]] <- as.numeric(dataframe[[col_name]])
    }
    
    # Subset the dataframe to exclude NA values in the current column
    subset_df <- dataframe[!is.na(dataframe[[col_name]]), ]
    
    # Compute the mean and standard error
    mean_val <- mean(subset_df[[col_name]])
    se_val <- sd(subset_df[[col_name]]) / sqrt(length(subset_df[[col_name]]))
    
    # Create a new row for the result dataframe
    result_row <- data.frame(Column = col_name, Mean = mean_val, SE = se_val)
    
    # Append the result row to the result dataframe
    result_df <- rbind(result_df, result_row)
  }
  
  return(result_df)
}

# Example usage:
# Create a sample dataframe with a factor column
sample_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
  NumericColumn = c(1, 2, NA, 4, 5)
)

# Compute mean and SE for all columns except the first two (ID and Name)
result <- compute_mean_and_se(sample_data)

# Print the result dataframe
print(result)


#Compute mean and se grouped by a categorical variab;le
library(dplyr)

compute_mean_and_se_grouped <- function(dataframe, column_names = NULL, groupby = NULL) {
  # If column_names is not specified, select all columns except the first two
  if (is.null(column_names)) {
    column_names <- names(dataframe)[3:length(dataframe)]
  }
  
  # Initialize an empty dataframe to store the results
  result_df <- data.frame(Column = character(0), Group = character(0), Mean = numeric(0), SE = numeric(0))
  
  # Loop through the specified column names
  for (col_name in column_names) {
    # Check if the column is numeric
    if (is.numeric(dataframe[[col_name]])) {
      # If groupby is specified, calculate mean and SE within each group using dplyr
      if (!is.null(groupby)) {
        grouped_data <- dataframe %>%
          group_by_at(vars(all_of(groupby))) %>%
          summarise(Mean = mean(.data[[col_name]], na.rm = TRUE),
                    SE = sd(.data[[col_name]], na.rm = TRUE) / sqrt(n())) %>%
          mutate(Column = col_name)
        
        result_df <- bind_rows(result_df, grouped_data)
      } else {
        # Subset the dataframe to exclude NA values in the current column
        subset_df <- dataframe[!is.na(dataframe[[col_name]]), ]
        
        # Compute the mean and standard error
        mean_val <- mean(subset_df[[col_name]])
        se_val <- sd(subset_df[[col_name]]) / sqrt(length(subset_df[[col_name]]))
        
        # Create a new row for the result dataframe
        result_row <- data.frame(Column = col_name, Group = "Overall", Mean = mean_val, SE = se_val)
        
        # Append the result row to the result dataframe
        result_df <- bind_rows(result_df, result_row)
      }
    } else {
      cat("Column", col_name, "is not numeric. Skipping...\n")
    }
  }
  
  return(result_df)
}

# Example usage:
# Create a sample dataframe with a factor column
sample_data <- data.frame(
  ID = 1:5,
  Name = c("A", "B", "C", "D", "E"),
  FactorColumn = factor(c("Low", "Medium", "High", "Low", "Medium")),
  NumericColumn = c(1, 2, NA, 4, 5)
)

# Compute mean and SE for all columns except the first two (ID and Name), grouped by FactorColumn
result <- compute_mean_and_se_grouped(sample_data, groupby = "FactorColumn")

# Print the result dataframe
print(result)



# Updated function to create a bar plot with percentages and bars ordered by y values
create_bar_chart_ordered <- function(dataframe, x_var, y_var, fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Count", y_limits = c(0, 1)) {
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = reorder(!!as.name(x_var), -!!as.name(y_var)), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = sprintf("%.2f", !!as.name(y_var))), vjust = 1, size = 2, colour = "white") +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(dataframe[[fill_var]]))) +
    expand_limits(y = y_limits) +  # Adjust y-axis limits
    labs(title = main_title, y = y_axis_label, x = x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 6, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Scale y-axis to display numbers rounded to two decimals
  p <- p + scale_y_continuous()
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 1800)
  return(p)
}

# Example usage:
# Create a sample dataframe
your_data_frame <- data.frame(
  x_variable = c("A", "B", "C", "D", "E"),
  fill_variable = c("X", "Y", "X", "Y", "Z"),
  count = c(10, 8, 6, 12, 5)
)

# Create a bar chart with bars organized by count values
create_bar_chart_ordered(your_data_frame, "x_variable", "count", "fill_variable", plot_name = "output_plot.png", facet_var = NULL, main_title = "Main Title", x_axis_label = "X-Axis Label", y_axis_label = "Count")

