# Install the necessary packages if not already installed
# install.packages("ggplot2")
# install.packages("reshape2")

# Load the required libraries
library(ggplot2)
library(reshape2)

# Function to generate a correlation heatmap
generate_correlation_heatmap <- function(dataframe) {
  # Compute the correlation matrix
  cor_matrix <- cor(dataframe)
  
  # Reshape the correlation matrix
  cor_melted <- melt(cor_matrix)
  
  # Generate the heatmap
  ggplot(data = cor_melted, aes(Var1, Var2, fill = value)) +
    geom_tile() +
    scale_fill_gradient(low = "skyblue", high = "steelblue") +
    labs(title = "Correlation Heatmap", x = "Variables", y = "Variables")
}

# Example usage with a sample dataframe
# Replace this with your actual dataframe
set.seed(123)
dataframe <- data.frame(
  var1 = rnorm(100),
  var2 = rnorm(100),
  var3 = rnorm(100)
)

# Generate the correlation heatmap
heatmap_plot <- generate_correlation_heatmap(dataframe)

# Display the correlation heatmap
print(heatmap_plot)
