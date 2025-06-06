#Prints a out the str output neately.
print_dataframe <- function(dataframe) {
  # Capture the output of str
  str_output <- capture.output(str(dataframe))
  
  # Print each line of the captured output
  for (line in str_output) {
    cat(line, "\n")
  }
}

# Example usage:
# Create a sample dataframe
sample_data <- data.frame(
  ID = 1:5,
  Name = c("Alice", "Bob", "Charlie", "David", "Eve"),
  Age = c(25, 30, 22, 35, 28),
  Score = c(95, 88, 76, 92, 89)
)

# Print the dataframe as a nicely formatted table
print_dataframe(sample_data)

