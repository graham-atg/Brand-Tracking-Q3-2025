#This function takes x,y and fill variables and generates a bar plot using theme
# and color pallette for presentations at Acrisure.



#The create_bar_chart function generates a bar plot using themes and a color palette 
#for presentations at Acrisure, taking as inputs data (the dataframe), x_var (the x-axis variable), 
#fill_var (the variable for fill), and facet_var (optional variable for facets).
#The function also has parameters for plot_name (output plot filename), percentage
#(whether to display percentages or counts), and main_title (title for the plot)

create_bar_chart <- function(data, x_var, fill_var = x_var, facet_var = NULL, plot_name = "defaultplot.png", percentage = FALSE, main_title = NULL) {
  # Load required packages
  library(extrafont)
  library(magick)
  library(jtools)
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#313131")
  
  # Define the common theme
  apatheme <- theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 4, family = 'sans'),
      legend.title = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none",
      axis.text.x = element_text(angle = 55, vjust = 1, hjust = 1)
    )
  
  # Create a ggplot
  p <- ggplot(data, aes(x = !!as.name(x_var), fill = !!as.name(fill_var))) +
    geom_bar(stat = ifelse(percentage, "identity", "count")) +
    apatheme +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(data[[fill_var]]))) +
    labs(title = ifelse(!is.null(main_title), main_title, plot_name), y = ifelse(percentage, "Pct. Selected", "Count"), x = NULL)
  
  # Add facet_wrap if facet_var is not NULL
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Calculate and add percentage or count labels to y-axis based on the 'percentage' argument
  if (percentage) {
    p <- p + geom_text(stat = 'identity', aes(label = paste0(round(100 * count / sum(count)), "%")), vjust = 1, size = 3, colour = "white")
  } else {
    p <- p + geom_text(stat = 'count', aes(label = after_stat(count)), vjust = 1, size = 3, colour = "white") # Set label text color to white
  }
  
  # Add support for x-axis labels
  if (!is.null(x_labels)) {
    p <- p + scale_x_discrete(labels = x_labels)
  }
  
  # Display the plot
  print(p)
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 1800)
}

# Sample data
data <- data.frame(
  category = c("A", "B", "C", "D", "E"),
  count = c(50, 30, 20, 15, 10)
)

# Example function call
create_bar_chart(
  data = data,
  x_var = "category",
  fill_var = "category",
  percentage = FALSE,
  main_title = "My Bar Chart"
)
#The function create_bar_chart_ordered creates a bar plot with bars ordered by y
#values, considering inputs such as dataframe (the dataframe), x_var (the x-axis variable), 
#y_var (the y-axis variable), fill_var (the variable for fill), plot_name (output plot filename), 
#facet_var (optional variable for facets), main_title (title for the plot), x_axis_label
#(label for the x-axis), and y_axis_label (label for the y-axis).



# Updated function to create a bar plot with percentages and bars ordered by y values
create_bar_chart_ordered <- function(dataframe, x_var, y_var, fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = NULL, x_axis_label_angle = 45, x_axis_label_vjust = 1, x_axis_label_hjust = 1, y_limits = c(0, 1)) {
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
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = reorder(!!as.name(x_var), -!!as.name(y_var)), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = sprintf("%.2f", !!as.name(y_var))), vjust = 1, size = 2, colour = "white") +
    scale_fill_manual(values = custom_palette[1:n_distinct(dataframe[[fill_var]])]) +
    expand_limits(y = y_limits) + # Adjust y-axis limits
    labs(title = main_title, y = if (is.null(y_axis_label)) y_var else y_axis_label, x = if (is.null(x_axis_label)) x_var else x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 6, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = x_axis_label_angle, vjust = x_axis_label_vjust, hjust = x_axis_label_hjust)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 1800)
  
  return(p)
}

# Example usage:
your_data_frame <- data.frame(
  x_variable = c("A", "B", "C", "D", "E"),
  fill_variable = c("X", "Y", "X", "Y", "Z"),
  count = c(10, 8, 6, 12, 5)
)

plot_object <- create_bar_chart_ordered(your_data_frame, "x_variable", "count", "fill_variable", plot_name = "output_plot.png",
                                        facet_var = NULL, main_title = "Main Title", x_axis_label = "X-Axis Label", y_axis_label = "Y-Axis Label")
print(plot_object)

#The bar_chart_count function generates a bar chart with counts, considering inputs such as dataframe (the dataframe),
#x_var (the x-axis variable), fill_var (the variable for fill), plot_name (output plot filename), main_title (title for the plot), 
#x_axis_label (label for the x-axis), and y_axis_label (label for the y-axis).

# Updated function to create a bar chart with counts
bar_chart_count <- function(dataframe, x_var, fill_var, facet_var = NULL, plot_name = "default_plot.png", main_title = NULL, x_axis_label = NULL, y_axis_label = "Count") {
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = factor(!!as.name(x_var)), fill = factor(!!as.name(fill_var)))) +
    geom_bar() +
    scale_fill_manual(values = custom_palette) +
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

# Create a bar chart with counts
bar_chart_count(your_data_frame, "x_variable", "fill_variable", plot_name = "output_plot.png", main_title = "Main Title", x_axis_label = "X-Axis Label", y_axis_label = "Count")


#Lastly, the bar_chart_count_with_error function creates a bar chart with counts
#and error bars, taking inputs such as dataframe (the dataframe), x_var (the x-axis variable),
#y_var (the y-axis variable), fill_var (the variable for fill), plot_name (output plot filename),
#main_title (title for the plot), x_axis_label (label for the x-axis), and y_axis_label (label for the y-axis).

library(ggplot2)
bar_chart_count_with_error <- function(dataframe, x_var, y_var, fill_var, facet_var = NULL, plot_name = "default_plot.png", main_title = NULL, x_axis_label = NULL, y_axis_label = NULL, x_axis_label_angle = 45, x_axis_label_vjust = 1, x_axis_label_hjust = 1) {
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
  
  # Check if facet_var is valid if provided
  if (!is.null(facet_var)) {
    if (!(facet_var %in% names(dataframe))) {
      stop(paste0("The facet_var '", facet_var, "' is not a valid column name in the input dataframe."))
    }
  }
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = factor(!!as.name(x_var)), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity") +
    geom_errorbar(
      aes(x = factor(!!as.name(x_var)), ymin = !!as.name(y_var) - sd, ymax = !!as.name(y_var) + sd),
      width = 0.2, colour = "black"
    ) +
    scale_fill_manual(values = custom_palette) +
    labs(title = main_title, y = if (is.null(y_axis_label)) y_var else y_axis_label, x = if (is.null(x_axis_label)) x_var else x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 6, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = x_axis_label_angle, vjust = x_axis_label_vjust, hjust = x_axis_label_hjust)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 1800)
  
  return(p)
}

# Example usage:
your_data_frame <- data.frame(
  x_variable = c("A", "B", "C", "D", "E"),
  fill_variable = c("X", "Y", "X", "Y", "Z"),
  y_variable = c(10, 8, 6, 12, 5), 
  sd = c(1, 1.5, 0.8, 1.2, 0.7) # Standard deviation for error bars
)

plot_object <- bar_chart_count_with_error(your_data_frame, "x_variable", "y_variable", 
                                          "fill_variable", facet_var = NULL, plot_name = "output_plot.png",
                                          main_title = "Main Title", x_axis_label = "X-Axis Label", y_axis_label = "Y-Axis Label")
print(plot_object)