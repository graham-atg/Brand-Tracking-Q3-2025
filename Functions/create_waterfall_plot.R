# Load required libraries
library(extrafont)
library(magick)
library(jtools)
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)
#The create_waterfall_plot function generates a waterfall plot based on the provided dataframe. 
#It takes the inputs of the dataframe, variable, y_variable, fill_variable, facet_wrap_variable, 
#color, save_as, main_title, x_axis_label, y_axis_label, and x_ticks. 
#The function produces a waterfall plot using ggplot2 and saves it as a PNG file. It then returns the ggplot object.


# Function to create a waterfall plot
# This function generates a waterfall plot based on the provided dataframe and various customizable parameters.
create_waterfall_plot <- function(dataframe, variable, y_variable, fill_variable = NULL, facet_wrap_variable = NULL,
                                  color = NULL, save_as = "default_waterfall_plot.png",
                                  main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct. Selected", x_ticks = NULL) {
  # Define a custom color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Use the custom palette if specified in the 'color' argument
  if (is.null(color)) {
    color_palette <- "RdBu"  # Default palette
  } else if (color == "custom") {
    color_palette <- custom_palette
  } else {
    color_palette <- color
  }
  
  # Create a ggplot object for the waterfall plot
  p <- ggplot(dataframe, aes(x = reorder(dataframe[[variable]], dataframe[[y_variable]]), y = dataframe[[y_variable]], fill = dataframe[[fill_variable]])) +
    geom_bar(stat = "identity", position = "identity", width = 0.7) +
    scale_fill_manual(values = color_palette) +  # Use the selected color palette
    labs(title = main_title, x = x_axis_label, y = y_axis_label) +
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
  
  # Set x-axis ticks if specified
  if (!is.null(x_ticks)) {
    p <- p + scale_x_discrete(breaks = x_ticks)
  }
  
  # Add facet wrapping if specified
  if (!is.null(facet_wrap_variable)) {
    p <- p + facet_wrap(~get(facet_wrap_variable), ncol = 1)
  }
  
  # Save the plot using ggsave
  ggsave(filename = save_as, plot = p, width = 10, height = 4.5, dpi = 1800)
  
  # Return the ggplot object
  return(p)
}


#The create_waterfall_percentage_plot function creates a waterfall plot similar to create_waterfall_plot,
#but it also scales the y-axis to display percentages. It takes the same inputs as create_waterfall_plot 
#and produces a waterfall plot with percentage values. The function saves the plot as a PNG file and returns the ggplot object.


# Function to create a waterfall plot with percentage values
# This function creates a waterfall plot with percentage values based on the provided dataframe and various customizable parameters.
create_waterfall_percentage_plot <- function(dataframe, variable, y_variable, fill_variable = variable, facet_wrap_variable = NULL,
                                             color = NULL, save_as = "default_waterfall_plot.png",
                                             main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct. Selected", x_ticks = NULL) {
  # Define a custom color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Use the custom palette if specified in the 'color' argument
  if (is.null(color)) {
    color_palette <- "RdBu"  # Default palette
  } else if (color == "custom") {
    color_palette <- custom_palette
  } else {
    color_palette <- color
  }
  
  # Create a ggplot object for the waterfall plot
  p <- ggplot(dataframe, aes(x = reorder(dataframe[[variable]], dataframe[[y_variable]]), y = ifelse(grepl("%", dataframe[[y_variable]]), as.numeric(gsub("%", "", dataframe[[y_variable]])) / 100, dataframe[[y_variable]]), fill = dataframe[[fill_variable]])) +
    geom_bar(stat = 'identity', position = 'identity', width = 0.7) +
    geom_text(stat = 'identity', aes(label = round(!!as.name(y_variable) * 100)), vjust = 1, size = 2, colour = "white") +
    scale_fill_manual(values = color_palette) +  # Use the selected color palette
    labs(title = main_title, x = x_axis_label, y = y_axis_label) +
    scale_y_continuous(labels = scales::percent) +  # Scale y-axis to display percentages
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
  
  # Set x-axis ticks if specified
  if (!is.null(x_ticks)) {
    p <- p + scale_x_discrete(breaks = x_ticks)
  }
  
  # Add facet wrapping if specified
  if (!is.null(facet_wrap_variable)) {
    p <- p + facet_wrap(~get(facet_wrap_variable), ncol = 1)
  }
  
  # Save the plot using ggsave
  ggsave(filename = save_as, plot = p, width = 10, height = 4.5, dpi = 1800)
  
  # Return the ggplot object
  return(p)
}
