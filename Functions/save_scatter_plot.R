# Load required library
library(ggplot2)

# Function: save_scatter_plot
# This function generates a scatter plot visualizing the relationship between two continuous variables.
# Each point is colored by a categorical variable, labeled with its corresponding value, and slightly enlarged for visibility.
# The function saves the plot as a high-resolution PNG file.
#
# Parameters:
#   - data: A data frame containing the variables.
#   - x: The name of the continuous variable for the x-axis.
#   - y: The name of the continuous variable for the y-axis.
#   - fill: The categorical variable that determines the point color.
#   - file_name: The name of the output PNG file.
#
# Example usage:
#   save_scatter_plot(data = sample_data, x = "awareness", y = "NPS", fill = "brand", file_name = "scatter_plot.png")

save_scatter_plot <- function(data, x, y, fill, file_name) {
  
  # Define a custom color palette (extended to handle multiple brands)
  custom_palette <- c(
    "#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", 
    "#004F51", "#95174C", "#7C6992", "#B2B4B2", "#313131",
    "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", 
    "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831", "#222830"
  )
  
  # Create scatter plot
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]], label = .data[[fill]])) +
    geom_point(shape = 21, size = 8, color = "black", alpha = 0.8) +  # Slightly larger points with black outline
    geom_text(aes(label = .data[[fill]]),size = 0, vjust = -2.5) +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = "",
      x = "Awareness (%)",
      y = "Mean CSAT (1-5)"
    ) +
    theme_minimal() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 12, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 2.5, hjust = 1)
    )
  
  # Save the plot
  ggsave(filename = file_name, plot = p, dpi = 1800, height = 8, width = 16, units = "in")
  
  message("Plot saved as ", file_name)
}

# Sample dataset for testing
set.seed(123)
sample_data <- data.frame(
  awareness = runif(20, 0, 100),  # Awareness score between 0 and 100
  NPS = runif(20, -100, 100),     # NPS score between -100 and 100
  brand = sample(LETTERS[1:10], 20, replace = TRUE)  # Random brand labels (A-J)
)

# Test the function
save_scatter_plot(data = sample_data, x = "awareness", y = "NPS", fill = "brand", file_name = "scatter_plot.png")




# Load required library
library(ggplot2)

save_scatter_plot <- function(data, x, y, fill, file_name) {
  
  # Define a custom color palette
  custom_palette <- c(
    "#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", 
    "#004F51", "#95174C", "#7C6992", "#B2B4B2", "#313131",
    "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", 
    "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831", "#222830"
  )
  
  # Create scatter plot with jittered points and labels
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]], label = .data[[fill]])) +
    geom_jitter(shape = 21, size = 10, color = "black", alpha = 0.4, width = 1, height = 1) +  # Jittered points
    geom_text(position = position_jitter(width =0, height = 0),  # Jitter labels separately
              size = 0, fontface = "bold") +  
    scale_fill_manual(values = custom_palette) +
    labs(
      title = "Awareness vs NPS by Brand",
      x = "Awareness",
      y = "Net Promoter Score (NPS)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 12, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Save the plot
  ggsave(filename = file_name, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  message("Plot saved as ", file_name)
}

# Test the function
set.seed(123)
sample_data <- data.frame(
  awareness = runif(10, 0, 100),
  NPS = runif(20, -100, 100),
  brand = sample(LETTERS[1:10], 20, replace = TRUE)
)

save_scatter_plot(data = sample_data, x = "awareness", y = "NPS", fill = "brand", file_name = "scatter_plot.png")



library(ggplot2)

save_scatter_plot <- function(data, x, y, fill, file_name) {
  
  # Define a custom color palette
  custom_palette <- c(
    "#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", 
    "#004F51", "#95174C", "#7C6992", "#B2B4B2", "#313131",
    "#1F6F8B", "#2C3E50", "#8C4646", "#607D8B", "#6D4C41", 
    "#A1C4D1", "#475C72", "#B88C73", "#A9A9A9", "#222831", "#222830"
  )
  
  # Create scatter plot with jittered points and labels
  p <- ggplot(data, aes(x = .data[[x]], y = .data[[y]], fill = .data[[fill]], label = .data[[fill]])) +
    geom_jitter(shape = 21, size = 6, color = "black", alpha = 0.4, width = 0, height = 0) +
    geom_text(
      position = position_jitter(width = 0, height = 0),
      size = 0,
      fontface = "bold"
    ) +
    scale_fill_manual(values = custom_palette, name = fill) +  # add legend title
    labs(
      title = "Awareness vs NPS by Brand",
      x = "Awareness",
      y = "Net Promoter Score (NPS)"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 12, family = 'sans'),
      legend.position = "right",          # show legend
      legend.title = element_text(face = "bold"),  # optional
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Save the plot
  ggsave(filename = file_name, plot = p, dpi = 1800, height = 4.5, width = 12, units = "in")
  
  message("Plot saved as ", file_name)
}
