#factorize_columns function iterates through a dataframe and factorizes specified columns,
#ensuring that the specified columns are converted to factors.
#If no columns are specified, it factorizes all columns. 
#The inputs for this function are dataframe (the input dataframe) and an optional
#vector columns specifying the columns to be factorized.

#The function factorize_columns takes a dataframe as input and factorizes the specified
#columns or the entire dataframe if no columns are specified.
#The example provided demonstrates the factorization of specific columns and the entire dataframe.

#Iterates through a dataframes and factorizes specified columsn
factorize_columns <- function(dataframe, columns = NULL) {
  if (is.null(columns)) {
    columns <- names(dataframe)
  }
  
  for (col in columns) {
    if (is.factor(dataframe[[col]])) {
      next
    }
    
    dataframe[[col]] <- as.factor(dataframe[[col]])
  }
  
  return(data.frame(dataframe))
}


# Factorize specific columns in a dataframe
data <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(25, 30, 22),
  Gender = c("Female", "Male", "Male")
)

data <- factorize_columns(data, columns = c("Name", "Gender"))

# Factorize the entire dataframe
data <- factorize_columns(data)
