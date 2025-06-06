#Certainly! I'll create a function called create_bubble_plot that takes the inputs you specified and saves the plot as a high-resolution PNG file using ggsave. Here's the function:
#Function to Create and Save Bubble PlotClick to open code
#This function does the following:
  
#It takes six parameters:
  
#data: The dataframe containing your data
#x_variable: The name of the column for the x-axis
#y_variable: The name of the column for the y-axis
#size_variable: The name of the column for the size of the bubbles
#fill_var: The name of the column for the color fill of the bubbles
#output_filename: The name of the output file (should end with .png)


#It creates a ggplot object with the specified aesthetics.
#It uses aes_string() instead of aes() to allow variable names to be passed as strings.
#It calculates the x and y axis limits with a 10% padding to ensure all points are visible.
#It saves the plot using ggsave() with the specified parameters (dpi=1800, height=4.5, width=12).
#It returns the plot object in case you want to make further modifications.

library(ggplot2)

create_bubble_plot <- function(data, x_variable, y_variable, size_variable, fill_var, output_filename, 
                               x_label = NULL, y_label = NULL, 
                               x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL) {
  # Define custom palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C","#d6b4fc","lightgrey")
  
  # If custom labels are not provided, use variable names
  if (is.null(x_label)) x_label <- x_variable
  if (is.null(y_label)) y_label <- y_variable
  
  # Create the plot
  p <- ggplot(data, aes_string(x = x_variable, y = y_variable, size = size_variable, fill = fill_var)) +
    geom_point(shape = 21, alpha = 0.6) +
    geom_text(aes_string(label = size_variable), vjust = 0.5, hjust = 0.5, size = 3, family = "sans", color = "black") +
    scale_size_continuous(range = c(10, 25)) + 
    scale_fill_manual(values = custom_palette) +
    labs(title = "", 
         x = x_label, 
         y = y_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 10, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "right",
      legend.key = element_rect(color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5),
      legend.text = element_text(size = 8)) +
    guides(size = "none")
  
  # Set axis limits
  if (is.null(x_min) || is.null(x_max)) {
    x_range <- range(data[[x_variable]], na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1
    x_min <- ifelse(is.null(x_min), x_range[1] - x_padding, x_min)
    x_max <- ifelse(is.null(x_max), x_range[2] + x_padding, x_max)
  }
  
  if (is.null(y_min) || is.null(y_max)) {
    y_range <- range(data[[y_variable]], na.rm = TRUE)
    y_padding <- diff(y_range) * 0.1
    y_min <- ifelse(is.null(y_min), y_range[1] - y_padding, y_min)
    y_max <- ifelse(is.null(y_max), y_range[2] + y_padding, y_max)
  }
  
  # Add limits to the plot
  p <- p + 
    xlim(x_min, x_max) +
    ylim(y_min, y_max)
  
  # Save the plot
  ggsave(filename = output_filename, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  # Return the plot object in case further modifications are needed
  return(p)
}

# Sample dataframe
sample_data <- data.frame(
  Ambassador = c("A", "B", "C", "D", "E", "F", "G"),
  mean_favorability = c(3.2, 3.5, 3.8, 3.3, 3.6, 3.9, 3.7),
  mean_trust = c(3.5, 3.7, 3.9, 3.4, 3.8, 4.0, 3.6),
  mean_awareness = c(60, 75, 90, 55, 80, 95, 70)
)

# Function call to test with custom axis labels and limits
create_bubble_plot(sample_data, 
                   "mean_favorability", 
                   "mean_trust", 
                   "mean_awareness", 
                   "Ambassador", 
                   "test_bubble_plot_custom_labels_limits.png",
                   x_label = "Mean Favorability (1-5)",
                   y_label = "Mean Trust (1-5)",
                   x_min = 3, x_max = 4,
                   y_min = 3.3, y_max = 4.1)

create_bubble_plot <- function(data, x_variable, y_variable, size_variable, fill_var, label_variable, output_filename,
                               x_label = NULL, y_label = NULL,
                               x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                               label_symbol = NA, legend_title = NA, top_legend = NA) {
  library(ggplot2)
  library(ggrepel)
  
  # Define custom palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#7C6992","#B2B4B2","#313131",
                      "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831")
  
  # If custom labels are not provided, use variable names
  if (is.null(x_label)) x_label <- x_variable
  if (is.null(y_label)) y_label <- y_variable
  
  # Add symbol to the label_variable after the value if specified
  if (!is.na(label_symbol)) {
    data[[paste0(label_variable, "_with_symbol")]] <- paste0(data[[label_variable]], label_symbol)
    label_variable_for_plot <- paste0(label_variable, "_with_symbol")
  } else {
    label_variable_for_plot <- label_variable
  }
  
  # Calculate the radius of each bubble
  size_range <- range(data[[size_variable]], na.rm = TRUE)
  data$radius <- scales::rescale(data[[size_variable]], to = c(2, 30), from = size_range)
  
  # Create the plot
  p <- ggplot(data, aes_string(x = x_variable, y = y_variable, size = size_variable, fill = fill_var)) +
    geom_point(shape = 21, alpha = 0.6) +
    geom_text(aes_string(label = label_variable_for_plot), size = 3, family = "sans", color = "black", vjust = 0.5, hjust = 0.5) +
    scale_size_binned(range = c(2,20), guide = guide_legend(title = legend_title, order = 2)) +
    scale_fill_manual(values = custom_palette, guide = guide_legend(title = "", order = 1)) +
    labs(title = "",
         x = x_label,
         y = y_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 10, family = 'sans'),
      legend.position = "right",
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 10, 0),
      legend.key = element_rect(color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.5, "lines"),
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Add second legend for top_legend if specified
  if (!is.na(top_legend) && top_legend != fill_var) {
    p <- p + 
      geom_point(aes_string(color = top_legend), alpha = 0) +
      scale_color_manual(values = custom_palette, 
                         guide = guide_legend(title = NULL, order = 0, 
                                              override.aes = list(size = 3, alpha = 1)))
  }
  
  # Set axis limits
  if (is.null(x_min) || is.null(x_max)) {
    x_range <- range(data[[x_variable]], na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1
    x_min <- ifelse(is.null(x_min), x_range[1] - x_padding, x_min)
    x_max <- ifelse(is.null(x_max), x_range[2] + x_padding, x_max)
  }
  
  if (is.null(y_min) || is.null(y_max)) {
    y_range <- range(data[[y_variable]], na.rm = TRUE)
    y_padding <- diff(y_range) * 0.1
    y_min <- ifelse(is.null(y_min), y_range[1] - y_padding, y_min)
    y_max <- ifelse(is.null(y_max), y_range[2] + y_padding, y_max)
  }
  
  # Add limits to the plot
  p <- p + 
    xlim(x_min, x_max) +
    ylim(y_min, y_max)
  
  # Save the plot
  ggsave(filename = output_filename, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  # Return the plot object in case further modifications are needed
  return(p)
}

# Sample dataframe
sample_data <- data.frame(
  Ambassador = c("A", "B", "C", "D", "E", "F", "G"),
  mean_favorability = c(3.2, 3.5, 3.8, 3.3, 3.6, 3.9, 3.7),
  mean_trust = c(3.5, 3.7, 3.9, 3.4, 3.8, 4.0, 3.6),
  mean_awareness = c(60, 75, 90, 55, 80, 95, 70)
)


# Test with custom options: top_label lowered, Ambassador in the center
create_bubble_plot(sample_data, 
                   "mean_favorability", 
                   "mean_trust", 
                   "mean_awareness", 
                   "Ambassador",    # Fill based on Ambassador
                   "Ambassador",    # Label bubbles with Ambassador
                   "test_bubble_plot_custom_labels_limits.png",
                   x_label = "Mean Favorability (1-5)",
                   y_label = "Mean Trust (1-5)",
                   x_min = 3, x_max = 4.5,
                   y_min = 3.3, y_max = 4.5,
                   label_symbol = "%",  # Symbol comes after the value
                   legend_title = "Bubble Size", 
                   top_legend  = "Ambassador")  # Top label for awareness, lowered above the bubble




create_bubble_plot_y_inverted <- function(data, x_variable, y_variable, size_variable, fill_var, label_variable, output_filename,
                                          x_label = NULL, y_label = NULL,
                                          x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                                          label_symbol = NA, legend_title = NA, top_legend = NA) {
  library(ggplot2)
  library(ggrepel)
  
  # Define custom palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#7C6992","#B2B4B2","#313131",
                      "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831")
  
  # If custom labels are not provided, use variable names
  if (is.null(x_label)) x_label <- x_variable
  if (is.null(y_label)) y_label <- y_variable
  
  # Add symbol to the label_variable after the value if specified
  if (!is.na(label_symbol)) {
    data[[paste0(label_variable, "_with_symbol")]] <- paste0(data[[label_variable]], label_symbol)
    label_variable_for_plot <- paste0(label_variable, "_with_symbol")
  } else {
    label_variable_for_plot <- label_variable
  }
  
  # Calculate the radius of each bubble
  size_range <- range(data[[size_variable]], na.rm = TRUE)
  data$radius <- scales::rescale(data[[size_variable]], to = c(2, 30), from = size_range)
  
  # Create the plot with y-axis inverted
  p <- ggplot(data, aes_string(x = x_variable, y = y_variable, size = size_variable, fill = fill_var)) +
    geom_point(shape = 21, alpha = 0.6) +
    geom_text(aes_string(label = label_variable_for_plot), size = 3, family = "sans", color = "black", vjust = 0.5, hjust = 0.5) +
    scale_size_binned(range = c(2, 20), guide = guide_legend(title = legend_title, order = 2)) +
    scale_fill_manual(values = custom_palette, guide = guide_legend(title = "", order = 1)) +
    labs(title = "", x = x_label, y = y_label) +
    scale_y_reverse() +  # Invert the y-axis
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 10, family = 'sans'),
      legend.position = "right",
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 10, 0),
      legend.key = element_rect(color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.5, "lines"),
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Add second legend for top_legend if specified
  if (!is.na(top_legend) && top_legend != fill_var) {
    p <- p + 
      geom_point(aes_string(color = top_legend), alpha = 0) +
      scale_color_manual(values = custom_palette, 
                         guide = guide_legend(title = NULL, order = 0, 
                                              override.aes = list(size = 3, alpha = 1)))
  }
  
  # Set axis limits for x, but for y use reversed limits if provided
  if (is.null(x_min) || is.null(x_max)) {
    x_range <- range(data[[x_variable]], na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1
    x_min <- ifelse(is.null(x_min), x_range[1] - x_padding, x_min)
    x_max <- ifelse(is.null(x_max), x_range[2] + x_padding, x_max)
  }
  
  # Reverse y-axis by inverting y_min and y_max, if provided
  if (!is.null(y_min) && !is.null(y_max)) {
    p <- p + ylim(y_max, y_min)  # Flip y_min and y_max for correct reversal
  }
  
  # Add x limits
  p <- p + xlim(x_min, x_max)
  
  # Save the plot
  ggsave(filename = output_filename, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  # Return the plot object in case further modifications are needed
  return(p)
}


# Test with inverted y-axis
create_bubble_plot_y_inverted(sample_data, 
                              "mean_favorability", 
                              "mean_trust", 
                              "mean_awareness", 
                              "Ambassador",    # Fill based on Ambassador
                              "Ambassador",    # Label bubbles with Ambassador
                              "test_bubble_plot_inverted_y.png",
                              x_label = "Mean Favorability (1-5)",
                              y_label = "Mean Trust (1-5)",
                              x_min = 3, x_max = 4.5,  # Customize as needed
                              y_min = 3.3, y_max = 4.5,  # Customize as needed
                              legend_title = "Bubble Size")





custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#7C6992","#B2B4B2","#313131",
                    "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831")

create_bubble_plot <- function(data, x_variable, y_variable, size_variable, fill_var, label_variable, output_filename,
                               x_label = NULL, y_label = NULL,
                               x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                               label_symbol = NA, legend_title = NA) {
  library(ggplot2)
  library(ggrepel)
  
  # If custom labels are not provided, use variable names
  if (is.null(x_label)) x_label <- x_variable
  if (is.null(y_label)) y_label <- y_variable
  
  # Add symbol to the label_variable after the value if specified
  if (!is.na(label_symbol)) {
    data[[paste0(label_variable, "_with_symbol")]] <- paste0(data[[label_variable]], label_symbol)
    label_variable_for_plot <- paste0(label_variable, "_with_symbol")
  } else {
    label_variable_for_plot <- label_variable
  }
  
  # Calculate the radius of each bubble
  size_range <- range(data[[size_variable]], na.rm = TRUE)
  data$radius <- scales::rescale(data[[size_variable]], to = c(2, 30), from = size_range)
  
  # Create the plot with fill_var for bubble color
  p <- ggplot(data, aes_string(x = x_variable, y = y_variable, size = size_variable, fill = fill_var)) +
    geom_point(shape = 21, alpha = 0.6) +  # Color is controlled by fill_var
    geom_text(aes_string(label = label_variable_for_plot), size = 3, family = "sans", color = "black", vjust = 0.5, hjust = 0.5) +
    scale_size_binned(range = c(2, 20), guide = guide_legend(title = legend_title, order = 1)) +  # Size legend included
    scale_fill_manual(values = custom_palette, guide = "none") +  # Remove the fill_var legend
    labs(title = "",
         x = x_label,
         y = y_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 10, family = 'sans'),
      legend.position = "right",  # Keep only the size legend on the right
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 10, 0),
      legend.key = element_rect(color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.5, "lines"),
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Set axis limits
  if (is.null(x_min) || is.null(x_max)) {
    x_range <- range(data[[x_variable]], na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1
    x_min <- ifelse(is.null(x_min), x_range[1] - x_padding, x_min)
    x_max <- ifelse(is.null(x_max), x_range[2] + x_padding, x_max)
  }
  
  if (is.null(y_min) || is.null(y_max)) {
    y_range <- range(data[[y_variable]], na.rm = TRUE)
    y_padding <- diff(y_range) * 0.1
    y_min <- ifelse(is.null(y_min), y_range[1] - y_padding, y_min)
    y_max <- ifelse(is.null(y_max), y_range[2] + y_padding, y_max)
  }
  
  # Add limits to the plot
  p <- p + 
    xlim(x_min, x_max) +
    ylim(y_min, y_max)
  
  # Save the plot
  ggsave(filename = output_filename, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  # Return the plot object in case further modifications are needed
  return(p)
}



custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#7C6992","#B2B4B2","#313131",
                    "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831")

create_bubble_plot_jitter <- function(data, x_variable, y_variable, size_variable, fill_var, label_variable, output_filename,
                                      x_label = NULL, y_label = NULL,
                                      x_min = NULL, x_max = NULL, y_min = NULL, y_max = NULL,
                                      label_symbol = NA, legend_title = NA, jitter_amount = NA) {
  library(ggplot2)
  library(ggrepel)
  
  # If custom labels are not provided, use variable names
  if (is.null(x_label)) x_label <- x_variable
  if (is.null(y_label)) y_label <- y_variable
  
  # Add symbol to the label_variable after the value if specified
  if (!is.na(label_symbol)) {
    data[[paste0(label_variable, "_with_symbol")]] <- paste0(data[[label_variable]], label_symbol)
    label_variable_for_plot <- paste0(label_variable, "_with_symbol")
  } else {
    label_variable_for_plot <- label_variable
  }
  
  # Calculate the radius of each bubble
  size_range <- range(data[[size_variable]], na.rm = TRUE)
  data$radius <- scales::rescale(data[[size_variable]], to = c(2, 30), from = size_range)
  
  # Create the plot with fill_var for bubble color
  p <- ggplot(data, aes_string(x = x_variable, y = y_variable, size = size_variable, fill = fill_var)) +
    if (!is.na(jitter_amount)) {
      geom_jitter(shape = 21, alpha = 0.6, width = jitter_amount, height = jitter_amount)
    } else {
      geom_point(shape = 21, alpha = 0.6)
    } +  # Apply jitter if specified
    geom_text(aes_string(label = label_variable_for_plot), size = 3, family = "sans", color = "black", vjust = 0.5, hjust = 0.5) +
    scale_size_binned(range = c(2, 20), guide = guide_legend(title = legend_title, order = 1)) +  # Size legend included
    scale_fill_manual(values = custom_palette, guide = "none") +  # Remove the fill_var legend
    labs(title = "", x = x_label, y = y_label) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 10, family = 'sans'),
      legend.position = "right",  # Keep only the size legend on the right
      legend.box = "vertical",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.margin = margin(0, 0, 10, 0),
      legend.key = element_rect(color = NA),
      legend.background = element_rect(fill = "white", color = NA),
      legend.text = element_text(size = 6),
      legend.key.size = unit(0.5, "lines"),
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Set axis limits
  if (is.null(x_min) || is.null(x_max)) {
    x_range <- range(data[[x_variable]], na.rm = TRUE)
    x_padding <- diff(x_range) * 0.1
    x_min <- ifelse(is.null(x_min), x_range[1] - x_padding, x_min)
    x_max <- ifelse(is.null(x_max), x_range[2] + x_padding, x_max)
  }
  
  if (is.null(y_min) || is.null(y_max)) {
    y_range <- range(data[[y_variable]], na.rm = TRUE)
    y_padding <- diff(y_range) * 0.1
    y_min <- ifelse(is.null(y_min), y_range[1] - y_padding, y_min)
    y_max <- ifelse(is.null(y_max), y_range[2] + y_padding, y_max)
  }
  
  # Add limits to the plot
  p <- p + 
    xlim(x_min, x_max) +
    ylim(y_min, y_max)
  
  # Save the plot
  ggsave(filename = output_filename, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  # Return the plot object in case further modifications are needed
  return(p)
}

                          legend_title = "Purchase Likelihood",
                          jitter_amount = 1)