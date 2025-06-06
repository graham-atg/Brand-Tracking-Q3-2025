library(extrafont)
library(magick)
library(jtools)
library(ggplot2)
library(reshape2)
library(scales)

#The bar_plot_percent function creates a bar plot with percentages. It takes a dataframe,
#dataframe, and the names of the x-axis variable, x_var, the y-axis variable, y_var, and 
#the fill variable, fill_var. Additional optional arguments include plot_name, facet_var, main_title
#, x_axis_label, y_axis_label, and y_limits. The function saves the plot as a PNG file and returns the plot object.

# Function to create a bar plot with percentages
bar_plot_percent <- function(dataframe, x_var, y_var = "perc", fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected", y_limits = c(0, 1)) {
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = reorder(stringr::str_wrap(!!as.name(x_var), 10), desc(!!as.name(y_var))), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = round(!!as.name(y_var) * 100)),   vjust = 1, size =3, colour = "white") +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(dataframe[[fill_var]]))) +
    expand_limits(y = y_limits) +  # Adjust y-axis limits
    labs(title = main_title, y = y_axis_label, x = x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 8, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Scale y-axis to display percentages
  p <- p + scale_y_continuous(labels = scales::percent)
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 12, height = 4.5, dpi = 1800)
  return(p)
  print(p)
}



#The function bar_plot_percent_ordered creates a bar plot with percentages, where 
#the bars are ordered by the values of the y-variable. It takes a dataframe, dataframe, 
#and the names of the x-axis variable, x_var, the y-axis variable, y_var, and the fill variable, 
#fill_var. It also accepts additional optional arguments such as plot_name, facet_var, main_title, x_axis_label,
#y_axis_label, and y_limits. The function saves the plot as a PNG file and returns the plot object.


# Updated function to create a bar plot with percentages, bars ordered by y values
bar_plot_percent_ordered <- function(dataframe, x_var, y_var = "Percentage", fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected", y_limits = c(0, 1)) {
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = reorder(stringr::str_wrap(!!as.name(x_var), 10), -!!as.name(y_var)), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = round(!!as.name(y_var) * 100)),   vjust = 1, size = 3, colour = "white") +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(dataframe[[fill_var]]))) +
    expand_limits(y = y_limits) +  # Adjust y-axis limits
    labs(title = main_title, y = y_axis_label, x = x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 8, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Scale y-axis to display percentages
  p <- p + scale_y_continuous(labels = scales::percent)
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 12, height = 4.5, dpi = 1800)
  return(p) 
  print(p)
}

# Example usage:
data_df <- data.frame(
  Category = rep(c("A", "B", "C", "D"), each = 4),
  Percentage = runif(16, min = 0, max = 100)
  
)

bar_plot_percent(
  dataframe = data_df,
  x_var = "Category",        # Use quotes for variable names
  y_var = "Percentage",      # Use quotes for variable names
  fill_var = "Category",  # Use quotes for variable names
  plot_name = "bar_plot_percentage.png",
  main_title = "Bar Plot Example",
  x_axis_label = "Categories",
  y_axis_label = "Percentage"
)


library(ggplot2)
library(scales)

# Sample dataframe for testing
data_df <- data.frame(
  Category = as.factor(c(1, 2, 3, 4, 5, 4, 1, 3, 2, 5)),
  Percentage = runif(10, min = 0, max = 100),
  Group = rep(c("A", "B"), each = 5)
)

# Function definition
bar_plot_percent <- function(dataframe, x_var, y_var = "Percentage", fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected", y_limits = c(0, 1)) {
  # Convert x_var to factor with ordered levels
  dataframe[[x_var]] <- factor(dataframe[[x_var]], levels = unique(dataframe[[x_var]]))
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(stringr::str_wrap(!!as.name(x_var), 10), y = !!as.name(y_var), fill = factor(!!as.name(fill_var)))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = round(!!as.name(y_var) * 100)),   vjust = 1, size = 0, colour = "white") +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(dataframe[[fill_var]]))) +
    expand_limits(y = y_limits) +  # Adjust y-axis limits
    labs(title = main_title, y = y_axis_label, x = x_axis_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 8, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 1, hjust=0.5)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Scale y-axis to display percentages
  p <- p + scale_y_continuous(labels = scales::percent)
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 12, height = 4.5, dpi = 1800)
  return(p)
}

# Test the function
bar_plot_percent(data_df, "Category", "Percentage", "Group", "bar_plot_ordered.png", main_title = "Bar Plot Example", x_axis_label = "Categories", y_axis_label = "Percentage")
