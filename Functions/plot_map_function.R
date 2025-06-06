
#The plot_usa_map function creates a map of the United States using the sf and ggplot2 libraries. 
#It takes a dataframe, longitude column name, and latitude column name as inputs.
#It optionally filters the dataframe based on a specified column and plots the resulting map with location points.
#If provided, it saves the map as a PNG file with the specified filename.

library(sf)
library(ggplot2)

plot_usa_map <- function(dataframe, lon_col, lat_col, filter_col = NULL, save_filename = NULL) {
  # Get world geometries
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  
  # Filter the United States
  usa <- world[world$name == "United States", ]
  
  # Create a ggplot object for the US map
  usa_plot <- ggplot() +
    geom_sf(data = usa)
  
  if (!is.null(filter_col)) {
    # Filter the dataframe based on the filter column
    dataframe <- dataframe[dataframe[[filter_col]] == 1, ]
  }
  
  # Add location points to the map
  usa_plot <- usa_plot +
    geom_point(data = dataframe, aes(x = !!sym(lon_col), y = !!sym(lat_col)), color = "red", size = 3)
  
  # Customize the plot
  usa_plot <- usa_plot +
    labs(title = "Locations in the United States",
         subtitle = "Including Hawaii and Alaska") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  # Print the plot
  print(usa_plot)
  
  # Save the plot as a PNG if save_filename is provided
  if (!is.null(save_filename)) {
    ggsave(save_filename, plot = usa_plot, dpi = 1800, width = 10, height = 4.5)
  }
}

# Example dataframe with longitude, latitude, and filter columns
locations_df <- data.frame(
  name = c("New York", "Los Angeles", "Chicago", "Miami"),
  lon = c(-74.006, -118.243, -87.629, -80.1918),
  lat = c(40.7128, 34.0522, 41.8781, 25.7617),
  filter_col = c(1, 0, 1, 1)  # 1 to display, 0 to hide
)

# Call the function to plot the map with filtered points and save it as a PNG
plot_usa_map(dataframe = locations_df, lon_col = "lon", lat_col = "lat", filter_col = "filter_col", save_filename = "usa_map.png")
