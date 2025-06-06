#This function scans through the columns of a dataframe and removes columns
# that the use does not want to use based on a substring
# it allows for the user to specify columns that have a similar string variable name that 
# they want to retain.



#The delete_columns_with_substring_and_retain function is designed to remove columns 
#containing a specified substring while retaining columns that contain another specified substring.
#It first initializes an empty list to store the deleted column names.
#Then, it loops through the columns in the dataframe, checking if each column contains
#the substring to be deleted and does not contain the substring to be retained. 
#If these conditions are met, the function deletes the column from the dataframe 
#and adds the name of the deleted column to the list. Finally, the function prints the 
#list of deleted columns and returns the modified dataframe.

delete_columns_with_substring_and_retain <- function(df, substring_to_delete, substring_to_retain) {
  # Initialize an empty list to store the deleted column names
  deleted_columns <- vector("character", length = 0)
  
  # Loop through columns in the dataframe
  for (col in names(df)) {
    # Check if the column contains the substring to delete
    if (grepl(substring_to_delete, col) && !grepl(substring_to_retain, col)) {
      # Delete the column from the dataframe
      df <- df[, !(names(df) %in% col)]
      # Add the deleted column name to the list
      deleted_columns <- c(deleted_columns, col)
    }
  }
  
  # Print the list of deleted columns
  cat("Deleted columns:", paste(deleted_columns, collapse = ", "), "\n")
  
  # Return the modified dataframe
  return(df)
}

# Example usage:
# df <- delete_columns_with_substring_and_retain(your_data_frame, "_DE", "_DEMO")