#1.data: This is the input dataframe where the columns are to be moved.
#2,columns_to_move: This is a vector of column names that you want to move within the dataframe.
#3.position: This is the position where you want to place the selected columns in the dataframe.

#The function first checks if the number of columns to move does not exceed the 
#total number of columns in the dataframe. If it does, an error message is returned. 
#It also checks if the desired position is within a valid range.

#The function then rearranges the columns in the dataframe according to the specified inputs. 
#It places the selected columns at the desired position while maintaining the order of the remaining columns.
#The function finally returns the updated dataframe with the columns moved to the specified position.

move_columns <- function(data, columns_to_move, position) {
  if (length(columns_to_move) > ncol(data)) {
    stop("Number of columns to move exceeds the number of columns in the dataframe.")
  }
  
  if (position > ncol(data) + 1) {
    stop("Position to place the columns is out of range.")
  }
  
  existing_columns <- colnames(data)
  other_columns <- existing_columns[!existing_columns %in% columns_to_move]
  
  new_order <- c(other_columns[1:position - 1], columns_to_move, other_columns[position:length(other_columns)])
  data <- data[, new_order, drop = FALSE]
  
  return(data)
}

# Example dataframe
df <- data.frame(
  A = c(1, 2, 3),
  B = c(4, 5, 6),
  C = c(7, 8, 9)
)

# Move columns B and C to the first position
new_df <- move_columns(df, c("B", "C"), 1)
print(new_df)
