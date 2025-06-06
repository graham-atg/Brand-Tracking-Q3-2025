#Function creates a stacked bar chart with standard formatting.
library(extrafont)
library(magick)
library(jtools)
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")

#create_stacked_bar_chart function takes a dataframe and creates a stacked bar 
#chart with standard formatting. It calculates the percentage of the y_var within 
#each category specified by the x_var, and uses the fill_var for differentiating
#the segments within each bar. It can also optionally facet the plot by a variable specified in facet_var. 
#The function then saves the plot as an image and returns the plot itself.
#The inputs are dataframe, x_var, y_var, fill_var, plot_name, facet_var, main_title, x_axis_label, and y_axis_label.

create_stacked_bar_chart <- function(dataframe, x_var, y_var, fill_var = x_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected") {
  # Create a summary dataframe for stacking
  summary_df <- dataframe %>%
    group_by(.data[[x_var]], .data[[fill_var]]) %>%
    summarise(sum_y_var = sum(.data[[y_var]])) %>%
    ungroup() %>%
    mutate(Percent = sum_y_var / sum(sum_y_var) * 100)
  
  # Create the stacked bar chart
  plot <- ggplot(summary_df, aes(x = .data[[x_var]], y = Percent, fill = .data[[fill_var]])) +
    geom_bar(stat = "identity") +
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
    ) +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = ifelse(!is.null(main_title), main_title, NULL),
      x = ifelse(!is.null(x_axis_label), x_axis_label, NULL),
      y = y_axis_label
    )
  
  # Add facets if specified
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(vars(.data[[facet_var]]))
  }+
  plot <- plot + scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0, 0))
  
  
  # Save the plot as an image using ggsave
  ggsave(filename = plot_name, plot = plot, width = 10, height = 4.5, dpi = 1800)
  
  # Display the plot
  return(plot)
}

# Example usage:
# create_stacked_bar_chart(dataframe, x_var, y_var, fill_var, plot_name, facet_var, main_title, x_axis_label, y_axis_label)
# Sample data
set.seed(123)
data_df <- data.frame(
  Category = rep(c("A", "B", "C", "D"), each = 4),
  Subcategory = rep(c("X", "Y", "Z", "W"), times = 4),
  Percentage = runif(16, min = 0, max = 100)
)

# Example usage of create_stacked_bar_chart
create_stacked_bar_chart(
  dataframe = data_df,
  x_var = "Category",        # Use quotes for variable names
  y_var = "Percentage",      # Use quotes for variable names
  fill_var = "Subcategory",  # Use quotes for variable names
  plot_name = "stacked_bar_chart.png",
  main_title = "Stacked Bar Chart Example",
  x_axis_label = "Categories",
  y_axis_label = "Percentage"
)

#create_stacked_bar_chart_facet is an extension of the create_stacked_bar_chart function that 
#additionally allows faceting by a variable specified in facet_var. It performs the same operations
#as the create_stacked_bar_chart function, but considers an extra variable for faceting the plot.
#The inputs are similar to those of create_stacked_bar_chart, with an additional input facet_var.


library(extrafont)
library(magick)
library(jtools)
library(ggplot2)
library(reshape2)
library(scales)
library(dplyr)

custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")

create_stacked_bar_chart_facet <- function(dataframe, x_var, y_var, fill_var = x_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected") {
  # Create a summary dataframe for stacking
  summary_df <- dataframe %>%
    group_by(.data[[x_var]], .data[[fill_var]], .data[[facet_var]]) %>%
    summarise(sum_y_var = sum(.data[[y_var]])) %>%
    ungroup() %>%
    mutate(Percent = sum_y_var / sum(sum_y_var) * 100)
  
  # Create the stacked bar chart
  plot <- ggplot(summary_df, aes(x = .data[[x_var]], y = Percent, fill = .data[[fill_var]])) +
    geom_bar(stat = "identity") +
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
    ) +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = ifelse(!is.null(main_title), main_title, NULL),
      x = ifelse(!is.null(x_axis_label), x_axis_label, NULL),
      y = y_axis_label
    )
  
  # Add facets if specified
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(vars(.data[[facet_var]]))
  }
  
  # Save the plot as an image using ggsave
  ggsave(filename = plot_name, plot = plot, width = 10, height = 4.5, dpi = 1800)
  
  # Display the plot
  return(plot)
}

# Example usage:
# create_stacked_bar_chart_facet(dataframe, x_var, y_var, fill_var, plot_name, facet_var, main_title, x_axis_label, y_axis_label)
# Sample data
set.seed(123)
data_df <- data.frame(
  Category = rep(c("A", "B", "C", "D"), each = 4),
  Subcategory = rep(c("X", "Y", "Z", "W"), times = 4),
  FacetVar = rep(c("F1", "F2"), each = 8),  # Facet variable
  Percentage = runif(16, min = 0, max = 100)
)

# Example usage of create_stacked_bar_chart_facet with a facet plot
create_stacked_bar_chart_facet(
  dataframe = data_df,
  x_var = "Category",        # Use quotes for variable names
  y_var = "Percentage",      # Use quotes for variable names
  fill_var = "Subcategory",  # Use quotes for variable names
  plot_name = "stacked_bar_chart.png",
  main_title = "Stacked Bar Chart Example",
  x_axis_label = "Categories",
  y_axis_label = "Percentage",
  facet_var = "FacetVar"     # Specify facet variable
)


#create_stacked_bar_chart_facet_percent function creates a stacked bar chart with 
#correct percentage representation for each stacked option. 
#It calculates the percentage of each segment within each bar, taking into account
#the y_var values within each category specified by the x_var and differentiating the segments using the fill_var. 
#If a facet_var is specified, the function facets the plot accordingly. 
#The function saves the plot as an image and returns the plot itself.
#The inputs are dataframe, x_var, y_var, fill_var, plot_name, facet_var, main_title, x_axis_label, and y_axis_label.
create_stacked_bar_chart_facet_percent <- function(dataframe, x_var, y_var, fill_var = x_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected") {
  # Create a summary dataframe for stacking
  summary_df <- dataframe %>%
    group_by(.data[[x_var]], .data[[fill_var]], .data[[facet_var]]) %>%  # Added .data[] to specify the dataframe columns in group_by function
    summarise(sum_y_var = sum(.data[[y_var]])) %>%
    ungroup() %>%
    mutate(Percent = sum_y_var / sum(sum_y_var) * 100)  # Corrected calculation to represent percentage of each stacked option correctly
  
  # Create the stacked bar chart
  plot <- ggplot(summary_df, aes(x = .data[[x_var]], y = Percent, fill = .data[[fill_var]])) +  # Added .data[] to specify the dataframe columns in aes function
    geom_bar(stat = "identity") +
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
    ) +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = ifelse(!is.null(main_title), main_title, NULL),
      x = ifelse(!is.null(x_axis_label), x_axis_label, NULL),
      y = y_axis_label
    )
  
  # Add facets if specified
  if (!is.null(facet_var)) {
    plot <- plot + facet_wrap(vars(.data[[facet_var]]))  # Added .data[] to specify the dataframe columns in facet_wrap function
  }
  
  # Save the plot as an image using ggsave
  ggsave(filename = plot_name, plot = plot, width = 10, height = 4.5, dpi = 1800)
  
  # Display the plot
  return(plot)
}



