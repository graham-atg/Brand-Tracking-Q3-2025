#The generate_mutliple_tables_and_plots function is the main entry point of this data analysis pipeline. 
#Its purpose is to generate multiple tables and plots for a given set of questions in a large dataset.
#Here's a breakdown of what the function does:
	#1.	It takes two arguments: df_large (the large dataset to be analyzed) and questions (a vector of question names to be processed).
	#2.	It iterates through each question in the questions vector and performs the following steps: 
    #a. Selects the relevant columns from the df_large dataset, including the response_id column and the column corresponding to the current question. 
    #b. Converts the question variable to a factor to ensure it is treated as a categorical variable. 
    #c. Processes the data using the process_data_pipeline_multiple_columns function (which is not defined in the provided code). 
    #d. Generates a bar plot using the bar_plot_percent_ordered function, passing in the processed data, labels, and other parameters. 
    #e. Saves the processed dataframe to a CSV file using the current question as the filename. 
    #f. Generates and displays the descriptive statistics for the current question using the st() function from the vtable package and the generate_question_descriptives function.
#The generate_question_descriptives function is a helper function that is called within the generate_mutliple_tables_and_plots function. 
#Its purpose is to generate and display the descriptive statistics for a given question in the dataset.
#This function takes two arguments: data (the dataset to be analyzed) and question (the name of the question to be analyzed). 
#It then uses the st() function from the vtable package to generate the descriptive statistics and saves the results to a CSV file.
#Finally, it displays the descriptive statistics in a formatted table using the kable() function.
#Overall, this pipeline is designed to automate the process of generating multiple tables and plots for a set of questions in a large dataset,
#as well as providing descriptive statistics for each question. This can be useful for quickly exploring and summarizing the data, as well as preparing materials for reporting or further analysis.


# Load process_data_pipeline_multiple_columns into the environment.
library(tidyr)
library(dplyr)
library(reshape2)

# Function to transform data from wide to long format with multiple id.vars
#to_long takes a dataframe and transforms it from wide to long format using the reshape2 package. 
#It takes dataframe as input and outputs the transformed data melted_data.
to_long <- function(dataframe, id.vars, variable.name, value.name) {
  if (!is.character(id.vars)) {
    stop("id.vars must be a character vector.")
  }
  melted_data <- reshape2::melt(dataframe, id.vars = id.vars, variable.name = variable.name, value.name = value.name)
  return(melted_data)
}

# Function to factorize all columns in a dataframe

#factorize_column converts all columns in the dataframe into factors, ensuring
#that each column is a factor variable. It takes dataframe as input and outputs
#the dataframe with all columns converted to factors.
# Function to factorize columns while maintaining the order of the levels
# Function to factorize columns while maintaining the original levels order
factorize_column <- function(dataframe, columns = NULL, levels = NULL) {
  if (is.null(columns)) {
    columns <- names(dataframe)
  }
  for (col in columns) {
    if (is.factor(dataframe[[col]])) {
      next
    }
    if (is.null(levels)) {
      dataframe[[col]] <- as.factor(dataframe[[col]])
    } else {
      dataframe[[col]] <- factor(dataframe[[col]], levels = unique(dataframe[[col]]))
    }
  }
  return(dataframe)
}


# Function to calculate counts and percentages by grouping columns
# Updated calculate_count_and_percentage function to calculate counts and percentages within each level of group_columns

#calculate_count_and_percentage calculates counts and percentages by grouping columns 
#specified in group_columns. It groups the dataframe, calculates the count and percentage within each group, 
#and then ungroups the data. It takes dataframe as input and outputs the resulting calculated counts and percentages.

calculate_count_and_percentage <- function(dataframe, group_columns = NULL) {
  if (is.null(group_columns)) {
    group_columns <- names(dataframe)[c(length(dataframe) - 1, length(dataframe))]
  }
  
  result <- dataframe %>%
    group_by(!!!syms(group_columns)) %>%
    summarise(count = n()) %>%
    mutate(perc = count / sum(count)) %>%
    ungroup()  # Remove grouping
  
  return(result)
}

#new_percent calculates new percentages by multiplying the existing percentage (perc) 
#in the dataframe with the difference between total_columns and the length of id_vars. 
#It takes dataframe as input and outputs the dataframe with the new percentage added.

# Function to calculate new percentages
new_percent <- function(dataframe, id_vars, total_columns) {
  require(dplyr)
  difference <- total_columns - length(id_vars)
  dataframe <- dataframe %>%
    mutate(new_perc = perc * difference)
  return(dataframe)
}

# Updated process_data_pipeline function without total_rows argument
#process_data_pipeline_multiple_columns is an updated version of the data processing pipeline function. 
#It performs the following steps: transforming data from wide to long format, replacing NAs with a default value, 
#converting all columns to factors, ensuring the data is in a dataframe, calculating count and percentage, 
#removing rows where the value column equals 0, and finally calculating new percentages. 
#It takes dataframe as input and outputs the processed data.

process_data_pipeline_multiple_columns <- function(dataframe, id.var = NULL, default_value = 0, group_column = NULL) {
  # Step 1: Transform data from wide to long format using to_long
  df_long_data <- to_long(dataframe, id.vars = id.var, variable.name = "variable", value.name = "value")
  
  # Step 2: Replace NAs with default_value
  df_long_data <- replace(df_long_data, is.na(df_long_data), default_value)
  
  # Step 3: Factorize all columns
  df_long_data <- factorize_column(df_long_data)
  
  # Step 4: Ensure the data is in a data.frame
  df_long_data <- data.frame(df_long_data)
  
  # Step 5: Calculate count and percentage with the specified group_column
  result <- calculate_count_and_percentage(df_long_data, group_columns = group_column)
  
  # Step 6: Remove rows where value column equals 0
  result <- result[result$value != default_value, ]
  
  # Step 7: Calculate new percentages
  result <- new_percent(result, id_vars = id.var, total_columns = ncol(dataframe))
  
  return(result)
}

#Bar plot percent ordered function,


library(extrafont)
library(magick)
library(jtools)
library(ggplot2)
library(reshape2)
library(scales)
library(glue)


# Function to create a bar plot with percentages, bars ordered by y values
bar_plot_percent_ordered <- function(dataframe, x_var, y_var = "Percentage", fill_var, plot_name = "default_plot.png", facet_var = NULL, main_title = NULL, x_axis_label = NULL, y_axis_label = "Pct Selected", y_limits = c(0, 1)) {
  
  # Define the color palette
  custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
  
  # Precompute the `reorder()` expression
  x_var_ordered <- reorder(stringr::str_wrap(dataframe[[x_var]], 10), -dataframe[[y_var]])
  
  # Create a ggplot
  p <- ggplot(dataframe, aes(x = x_var_ordered, y = .data[[y_var]], fill = factor(.data[[fill_var]]))) +
    geom_bar(stat = "identity", width = 0.4) +
    geom_text(stat = 'identity', aes(label = glue::glue("{round(.data[[y_var]] * 100)}%")), vjust = 1, size = 3, colour = "white") +
    scale_fill_manual(values = rep(custom_palette, length.out = n_distinct(dataframe[[fill_var]]))) +
    expand_limits(y = y_limits) + # Adjust y-axis limits
    labs(title = glue::glue("{main_title}"), y = y_axis_label, x = glue::glue("{x_axis_label}")) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      text = element_text(size = 8, family = 'sans'),
      legend.title = element_blank(),
      legend.position = "none",
      axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5)
    )
  
  # Apply facet_wrap if facet_var is specified
  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars(.data[[facet_var]]))
  }
  
  # Scale y-axis to display percentages
  p <- p + scale_y_continuous(labels = scales::label_percent())
  
  # Save the plot as a PNG file
  ggsave(filename = plot_name, plot = p, device = "png")
  
  return(p)
}

require(vtable)

QUESTIONS <- c("q1_shared", "q4_shared", "q11_shared", "q16_shared", "q17_shared", "q18_shared", "q19_shared", "q96_shared", "q97_shared", "q21_shared", "q30_shared", "q31_shared")
DF_LARGE <- BTQ1

require(vtable)

generate_mutliple_tables_and_plots <- function(df_large, questions) {
  for (question in questions) {
    # Select the relevant columns from the large dataframe
    df <- dplyr::select(df_large, response_id, !! sym(question))
    
    # Convert the question variable to a factor
    df[[question]] <- as.factor(df[[question]])
    
    # Process the data
    question_df_perc <- process_data_pipeline_multiple_columns(df, id.var = c("response_id"), default_value = 0, group_column = c("value"))
    
    # Generate the bar plot
    bar_plot_percent_ordered(question_df_perc, "value", "new_perc", "value", paste0(question, "_DF_PERC.png"), main_title = "", x_axis_label = " ", y_axis_label = "Pct. Selected", y_limits = c(0, 0.25))
    
    # Save the processed dataframe
    write.csv(question_df_perc, paste0(question, "_QUESTION_DF_PERC.csv"), row.names = FALSE)
    
    # Generate and display the descriptive statistics
    st(df, vars = c(question), out = 'csv', file = paste0(question, "_descriptives.csv"))
    generate_question_descriptives(df, question)
  }
}


# Example usage
generate_mutliple_tables_and_plots(DF_LARGE, QUESTIONS)
