# Function to count the highest combination and calculate percentages
# This function takes a dataframe and a list of variables as inputs.
# It counts the highest combination of filled cells for each row and calculates the percentage of each combination.
# The combinations are sorted from 6 variables at the top to 2 variables at the bottom.
require(dplyr)
count_combinations <- function(df, variables) {
  # Select only the relevant variables
  df <- dplyr::select(df, dplyr::all_of(variables))
  
  combinations <- list()
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, , drop = FALSE]
    non_na_values <- unlist(row[!is.na(row)])
    
    if (length(non_na_values) >= 2) {
      comb <- paste(sort(non_na_values), collapse = ", ")
      combinations[[comb]] <- combinations[[comb]] %||% 0
      combinations[[comb]] <- combinations[[comb]] + 1
    }
  }
  
  return(combinations)
}

# Sample dataframe
BTQ4_CL <- data.frame(
  response_id = 1:10,
  q233_cl_1 = c("A", NA, "B", "C", NA, "D", "E", "F", NA, "G"),
  q233_cl_2 = c("H", "I", NA, "J", "K", NA, "L", "M", "N", NA),
  q233_cl_3 = c("O", "P", "Q", NA, "R", "S", NA, "T", "U", "V"),
  q233_cl_4 = c("W", "X", "Y", "Z", NA, "AA", "BB", NA, "CC", "DD"),
  q233_cl_5 = c("EE", "FF", "GG", "HH", "II", NA, "JJ", "KK", "LL", NA),
  q233_cl_6 = c("MM", "NN", "OO", "PP", "QQ", "RR", NA, "SS", "TT", "UU")
)

# List of variables
variables <- c("q233_cl_1", "q233_cl_2", "q233_cl_3", "q233_cl_4", "q233_cl_5", "q233_cl_6")

# Apply the function to the dataframe
combinations_count <- count_combinations(BTQ4_CL, variables)

# Convert the list to a dataframe for better readability
combinations_df <- as.data.frame(do.call(rbind, lapply(names(combinations_count), function(x) cbind(x, combinations_count[[x]]))))
colnames(combinations_df) <- c("Combination","Count")

# Print the result
print(combinations_df)

