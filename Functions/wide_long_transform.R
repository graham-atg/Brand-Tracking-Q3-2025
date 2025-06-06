library(tidyr)
library(reshape2)
#The to_long function uses the melt function from the reshape2 package to transform data from wide to long format. 
#It takes a dataframe as input along with id.vars as the identifying variable, variable.name as the name for the variable,
#and value.name as the name for the corresponding values. The function returns the melted dataframe in long format.
#The to_long function uses the melt function from the reshape2 package to transform data from wide to long format. 
#It takes a dataframe as input along with id.vars as the identifying variable, 
#variable.name as the name for the variable, and value.name as the name for the corresponding values. 
#The function returns the melted dataframe in long format.


# Function to transform data from wide to long format
to_long <- function(dataframe, id.vars, variable.name, value.name) {
  # Use reshape2 to melt the dataframe from wide to long format
  melted_data <- melt(dataframe, id.vars = id.vars, variable.name = variable.name, value.name = value.name)
  return(melted_data)
}


#The to_wide function employs the spread function from the tidyr package to transform data
#from long to wide format. It takes a dataframe as input along with id.vars as the identifying variable,
#variable.name as the name for the variable, and value.name as the name for the corresponding values.
#The function returns the spread dataframe in wide format.

# Function to transform data from long to wide format
to_wide <- function(dataframe, id.vars, variable.name, value.name) {
  # Use tidyr to spread the dataframe from long to wide format
  spread_data <- spread(dataframe, key = variable.name, value = value.name)
  return(spread_data)
}

# Example usage:
# Create a sample dataframe in wide format
wide_data <- data.frame(
  ID = 1:3,
  Var1_A = c(10, 20, 30),
  Var1_B = c(15, 25, 35),
  Var2_A = c(40, 50, 60),
  Var2_B = c(45, 55, 65)
)

# Transform from wide to long format
long_data <- to_long(wide_data, id.vars = "ID", variable.name = "Variable", value.name = "Value")
print(long_data)

# Transform from long to wide format
wide_data_again <- to_wide(long_data, id.vars = "ID", variable.name = "Variable", value.name = "Value")
print(wide_data_again)
