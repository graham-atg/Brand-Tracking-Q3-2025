library(ggplot2)
library(gridExtra)
library(grid)
library(png)  # Load the png package for reading PNG files
#The combine_and_save_plots function combines multiple PNG plots into a single
#plot and saves the combined plot as a PNG file. It takes the following inputs:
  
  #input_files: A character vector containing the file paths of the input plot files (PNGs).
#output_file: A string specifying the output file name for the combined plot (default is "default_plot.png").
#The function reads each input plot file, converts it to a raster graphical object, 
#and stores it in a list.
#It then arranges the plots in a single pane using the grid.arrange function. 
#Finally, it saves the combined plot as a PNG file with the specified parameters, such as width, height, and resolution.
combine_and_save_plots <- function(input_files, output_file = "default_plot.png") {
  # Create a list to store the individual plots
  plots <- lapply(input_files, function(file) {
    img <- readPNG(file)
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
  })
  
  # Arrange the plots in a single pane
  combined_plot <- do.call(grid.arrange, plots)
  
  # Save the combined plot as a PNG with the specified parameters
  ggsave(output_file, combined_plot, width = 15.5, height = 7, dpi = 1800)
}

# Example usage:
# Provide a list of input plot file paths (PNGs)
input_files <- c("plot1.png", "plot2.png", "plot3.png")

# Call the function to combine and save the plots
combine_and_save_plots(input_files, "combined_plot.png")


library(ggplot2)

# Function to generate a sample plot and save it as a PNG
generate_and_save_plot <- function(file_name) {
  p <- ggplot(data = data.frame(x = 1:10, y = 1:10), aes(x, y)) +
    geom_point() +
    ggtitle("Sample Plot")
  ggsave(file_name, p)
}

# Generate and save some sample plots
generate_and_save_plot("plot1.png")
generate_and_save_plot("plot2.png")
generate_and_save_plot("plot3.png")

# List of input plot file paths
input_files <- c("plot1.png", "plot2.png", "plot3.png")

# Call the function to combine and save the plots
combine_and_save_plots(input_files, "combined_plot.png")



library(ggplot2)
library(gridExtra)
library(grid)
library(png)

combine_and_save_plots_revised <- function(input_files, output_file = "default_plot.png") {
  # Precompute combined plot size
  width <- 15.5
  height <- 7
  dpi <- 1800

  # Create a list to store the individual plots
  plots <- lapply(input_files, function(file) {
    img <- readPNG(file)
    rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)
  })

  # Arrange the plots in a single pane
  combined_plot <- do.call(grid.arrange, plots)

  # Save the combined plot as a PNG
  ggsave(output_file, combined_plot, width = width, height = height, dpi = dpi)
}

# Example usage:
input_files <- c("plot1.png", "plot2.png", "plot3.png")
combine_and_save_plots_revised(input_files, "combined_plot.png")
