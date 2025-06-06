#The create_bar_chart_exclude_NAs function takes in a data frame along with specified 
#variables and generates a bar plot, excluding rows with NA values, and saves the 
#plot as a PNG file. The function allows customization of various plot elements like titles, labels, and colors.


# This function takes x, y, and fill variables, excluding NA values, and generates a bar plot using a theme
create_bar_chart_exclude_NAs <- function(data, x_var, fill_var = x_var, facet_var = NULL, plot_name = "defaultplot.png", percentage = FALSE, main_title = NULL, x_label = NULL, x_ticks = NULL) {
  # Load required packages
  library(extrafont)
  library(magick)
  library(jtools)
  library(ggplot2)
  library(reshape2)
  library(scales)
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#313131")
  
  # Remove rows with NA in x_var, y_var, and fill_var
  data <- data[complete.cases(data[, c(x_var, fill_var)]), ]
  
  # Define the common theme
  apatheme <- theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 8, family = 'sans'),
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
    labs(title = ifelse(!is.null(main_title), main_title, plot_name), y = ifelse(percentage, "Pct. Selected", "Count"), x = x_label) +
    scale_x_discrete(labels = x_ticks)
  
  # Add facet_wrap if facet_var is not NULL
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(!!as.name(facet_var)))
  }
  
  # Calculate and add percentage or count labels to y-axis based on the 'percentage' argument
  if (percentage) {
    p <- p + geom_text(stat = 'identity', aes(label = paste0(round(100 * count / sum(count)), "%")), vjust = 1, size = 2, colour = "white")
  } else {
    p <- p + geom_text(stat = 'count', aes(label = after_stat(count)), vjust = 1, size = 2, colour = "white") # Set label text color to white
  }
  
  # Display the plot
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 1800)
  return(p)
}

# Example usage:
create_bar_chart_exclude_NAs(your_data_frame, "x_variable", "fill_variable", facet_var = NULL, "output_plot.png", percentage = FALSE, main_title = "Main Title", x_label = "X-Axis Label", x_ticks = c("Tick1", "Tick2", "Tick3"))
# create_bar_chart_exclude_NAs(your_data_frame, "x_variable", "fill_variable", "facet_variable", "output_plot.png", percentage = TRUE, main_title = "Main Title", x_label = "X-Axis Label", x_ticks = c("Tick1", "Tick2", "Tick3"))
