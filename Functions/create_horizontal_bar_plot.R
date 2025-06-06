# Explanation of the Custom Horizontal Bar Plot Function:
# This function takes a dataframe and several customizable parameters to create a horizontal bar plot using ggplot2.
# Users can specify the x-axis variable, y-axis variable, fill variable, plot name, facet variable for faceted plots,
# main title, x-axis label, y-axis label, and y-axis limits.
# Load required libraries
library(tidyverse)
library(scales)

# Custom color palette
library(ggplot2)

# Function to create a customizable horizontal bar plot
create_horizontal_bar_plot <- function(dataframe, x_var, y_var = "perc", fill_var, plot_name = "default_plot.png", main_title = NULL, x_axis_label = NULL, y_axis_label = NULL, y_limits = c(0, 1), text_color = "white", text_size = 2.5, theme = theme_minimal()) {
  # Check for missing values
  if (any(sapply(dataframe, function(x) any(is.na(x))))) {
    stop("The input dataframe contains missing values. Please handle them before using this function.")
  }
  
  # Check if the input arguments are valid
  stopifnot(
    x_var %in% names(dataframe),
    y_var %in% names(dataframe),
    fill_var %in% names(dataframe)
  )
  
  p <- ggplot(dataframe, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var), fill = !!rlang::sym(fill_var))) +
    geom_col() +
    geom_text(aes(label = scales::percent(!!rlang::sym(y_var))),
              position = position_stack(vjust = 0.5),
              color = text_color,
              size = text_size) +
    coord_flip() +
    scale_x_discrete() +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = ifelse(!is.null(main_title), main_title, NULL),
      x = ifelse(!is.null(x_axis_label), x_axis_label, NULL),
      y = ifelse(!is.null(y_axis_label), y_axis_label, y_var)
    ) +
    theme +
    ylim(y_limits)
  
  ggsave(plot_name, plot = p, width = 10, height = 6, dpi = 300)
  
  return(p)
}

# Example usage:
set.seed(123)
test_df <- data.frame(
  Category = rep(c("A", "B", "C"), each = 3),
  Value = runif(9),
  Group = rep(c("Group1", "Group2", "Group3"), times = 3)
)

custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")

plot_object <- create_horizontal_bar_plot(
  dataframe = test_df,
  x_var = "Category",
  y_var = "Value",
  fill_var = "Group",
  plot_name = "custom_plot.png",
  main_title = "Custom Plot",
  x_axis_label = "X-Axis Label",
  y_axis_label = "Custom Y-Axis Label",
  y_limits = c(0, 1),
  text_color = "white",
  text_size = 2.5,
  theme = theme_minimal()
)
print(plot_object)