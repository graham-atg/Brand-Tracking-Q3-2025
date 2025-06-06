# create_regression_plot function takes a dataframe and creates a regression plot
# with options to display data points. It plots the relationship between the
# variables specified by x_var and y_var with a linear regression line.
# Additional customization options include setting the main_title, y_axis_label,
# x_axis_label, line_color, and whether to display data points using geom_point.
# The inputs for this function are dataframe (the input dataframe), x_var and y_var (the variables to be plotted).

# The function create_regression_plot generates a regression plot, and the example
# provided demonstrates how to use the function with sample data.

# Note this graph creates a regression plot
create_regression_plot <- function(dataframe, x_var, y_var, main_title = NULL, y_axis_label = NULL, x_axis_label = NULL, line_color = "#002A4E", plot_name = NULL) {
  # Check for missing values
  if (any(sapply(dataframe, function(x) any(is.na(x))))) {
    stop("The input dataframe contains missing values. Please handle them before using this function.")
  }
  
  # Check if the input arguments are valid
  stopifnot(
    x_var %in% names(dataframe),
    y_var %in% names(dataframe)
  )
  
  # Create a base plot with a linear regression line
  plot <- ggplot(dataframe, aes(x = !!rlang::sym(x_var), y = !!rlang::sym(y_var))) +
    geom_smooth(method = "lm", se = TRUE, color = line_color) +
    geom_point(size = 2, color = "#36749D") +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 6, family = 'sans'),
      plot.title = element_text(hjust = 0.5), # Center the title
      plot.margin = margin(20, 20, 20, 20) # Add margin to the plot
    ) +
    labs(
      title = ifelse(!is.null(main_title), main_title, NULL),
      x = ifelse(!is.null(x_axis_label), x_axis_label, x_var),
      y = ifelse(!is.null(y_axis_label), y_axis_label, y_var)
    )
  
  # Save the plot as a PNG file if plot_name is provided
  if (!is.null(plot_name)) {
    ggsave(filename = plot_name, plot = plot, width = 10, height = 4.5, dpi = 1800)
  }
  
  return(plot)
}

# Example usage for a regression plot with points
set.seed(123)
data_df <- data.frame(
  X = rnorm(100),
  Y = rnorm(100)
)

plot_object <- create_regression_plot(
  data_df, x_var = "X", y_var = "Y",
  main_title = "Regression Plot",
  y_axis_label = "Y-Axis Label",
  x_axis_label = "X-Axis Label",
  line_color = "#002A4E",
  plot_name = "regression_plot.png"
)
print(plot_object)