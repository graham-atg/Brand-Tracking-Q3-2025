library(ggplot2)
library(dplyr)

# Create a sample custom color palette
custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C", "#FAF9F6")

# Function to plot the frequency of the top 10 phrases
plot_top_phrases <- function(dataframe, column_name, plot_name = "top_phrases_plot.png") {
  # Check if the input dataframe and column_name are valid
  if (!is.data.frame(dataframe) || !is.character(column_name) || !column_name %in% names(dataframe)) {
    stop("Invalid input dataframe or column_name.")
  }
  
  # Group and count the phrases in the specified column
  phrase_counts <- dataframe %>%
    group_by(!!sym(column_name)) %>%
    summarize(Count = n()) %>%
    arrange(desc(Count)) %>%
    slice_head(n = 10)
  
  # Determine the number of unique phrases
  num_unique_phrases <- nrow(phrase_counts)
  
  # Create a fill column to assign unique colors from the custom palette
  phrase_counts$Fill <- rep(custom_palette, length.out = num_unique_phrases)
  
  # Create a ggplot bar chart
  p <- ggplot(phrase_counts, aes(x = reorder(!!sym(column_name), -Count), y = Count, fill = Fill)) +
    geom_bar(stat = "identity") +
    labs(title = paste("Top 10 Phrases in", column_name), y = "Frequency") +
    scale_fill_identity() + # Use the assigned colors
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
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, width = 10, height = 4.5, dpi = 300)
  
  # Display the plot
  print(p)
}

generate_multiple_phrase_barcharts <- function(dataframe) {
  for (column_name in names(dataframe)) {
    if (is.character(dataframe[[column_name]])) {
      plot_name <- paste0(column_name, "_phrases_barchart.png")
      plot_top_phrases(dataframe, column_name, plot_name)
    } else {
      message("Column '", column_name, "' is not a text column. Skipping this column.")
    }
  }
}

# Create a sample dataframe
sample_data <- data.frame(
  ID = 1:10,
  question1 = c("apple", "banana", "apple", "cherry", "banana", "apple", "banana", "cherry", "date", "apple"),
  question2 = c("orange", "pear", "orange", "kiwi", "pear", "orange", "pear", "kiwi", "mango", "orange"),
  numeric_column = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
)

# Generate multiple phrase barcharts
generate_multiple_phrase_barcharts(sample_data)