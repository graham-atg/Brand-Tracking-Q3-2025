#' Generate Horizontal Bar Chart
#'
#' This function generates a horizontal bar chart from a given dataframe. It allows the user to specify the variable names, value labels, and a custom color palette.
#'
#'@param df The input dataframe.
#'@param file_name The name of the output file to be saved.
#'@param question_names A vector of column names to be used for the chart.
#'@param var_labels A vector of variable labels to be used for the x-axis.
#'@param value_labels A vector of value labels to be used for the legend.
#'@param custom_palette A vector of custom colors to be used for the bars.
#'
#'@return The function saves the generated chart as a PNG file.
require(dplyr)
require(ggplot2)
generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette) {
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, !!!syms(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count), 
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.6) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete() +
    scale_fill_manual(values = setNames(custom_palette[1:length(value_labels)], value_labels)) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 12, family = 'sans'),
          legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
}

# Sample dataframe
set.seed(123)
sample_df <- data.frame(
  response_id = 1:100,
  q66_shared_1 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_2 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_3 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_4 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_5 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_6 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_7 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE)
)

# Test the function
custom_palette <- c("#002A4E", "#36749D","#85714D", "#004F51", "#000000",  "#95174C","#DDE9F0")

generate_horizontal_bar_chart(
  sample_df,
  "sample_output.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette
)






library(ggplot2)
library(dplyr)

#' Generate Horizontal Bar Chart
#'
#' This function generates a horizontal bar chart from a given dataframe. It allows the user to specify the variable names, variable labels, value labels, and a custom color palette.
#'
#' @param df The input dataframe.
#' @param file_name The name of the output file to be saved.
#' @param question_names A vector of column names to be used for the chart.
#' @param var_labels A vector of variable labels to be used for the x-axis.
#' @param value_labels A vector of value labels to be used for the legend.
#' @param custom_palette A vector of custom colors to be used for the bars.
#'
#' @return The generated horizontal bar chart plot object.


generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette) {
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, !!!syms(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Reorder the Value column to match the desired order
  reduced_df_long$Value <- factor(reduced_df_long$Value, levels = rev(value_labels))
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count),
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(values = rev(custom_palette[1:length(value_labels)])) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 12, family = 'sans'),
          legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
  
  # Return the plot object
  return(plot)
}


# Sample dataframe
set.seed(123)
sample_df <- data.frame(
  response_id = 1:100,
  q66_shared_1 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_2 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_3 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_4 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_5 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_6 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_7 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE)
)

# Test the function
custom_palette <- c("#002A4E", "#36749D", "#85714D","#004F51", "#000000",  "#95174C","#DDE9F0")

plot_object <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3","q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 1", "Variable 2", "Variable 3", "Variable 4", "Variable 5", "Variable 6", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette
)






generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette) {
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, !!!syms(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Reorder the Value column to match the desired order
  reduced_df_long$Value <- factor(reduced_df_long$Value, levels = rev(value_labels))
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count),
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(values = rev(custom_palette[1:length(value_labels)])) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 12, family = 'sans'),
          legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
  
  # Return the plot object and the reduced_df_long_perc dataframe
  return(list(plot = plot, data = reduced_df_long_perc))
}

# Sample dataframe
set.seed(123)
sample_df <- data.frame(
  response_id = 1:100,
  q66_shared_1 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_2 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_3 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_4 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_5 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_6 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_7 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE)
)

# Test the function
custom_palette <- c("#002A4E", "#36749D", "#DDE9F0", "#85714D", "#000000", "#004F51", "#95174C")
result <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output2.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 1", "Variable 2", "Variable 3", "Variable 4", "Variable 5", "Variable 6", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette
)

# Access the plot object and the data frame
plot_object <- result$plot
reduced_df_long_perc <- result$data




generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette, bar_order) {
  
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, !!!syms(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Reorder the Variable column to match the desired order
  reduced_df_long$Variable <- factor(reduced_df_long$Variable, levels = bar_order)
  
  # Reorder the Value column to match the desired order
  reduced_df_long$Value <- factor(reduced_df_long$Value, levels = rev(value_labels))
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count),
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(values = rev(custom_palette[1:length(value_labels)])) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 12, family = 'sans'),
          legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
  
  # Return the plot object and the reduced_df_long_perc dataframe
  return(list(plot = plot, data = reduced_df_long_perc))
}


result <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output3.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 2", "Variable 4", "Variable 1", "Variable 6", "Variable 3", "Variable 5", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette,
  bar_order = c("q66_shared_2", "q66_shared_4", "q66_shared_1", "q66_shared_6", "q66_shared_3", "q66_shared_5", "q66_shared_7")
)



generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette, bar_order) {
  reduced_df <- dplyr::select(df, response_id, all_of(question_names))
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Reorder the Variable column to match the desired order using forcats::fct_relevel()
  reduced_df_long$Variable <- forcats::fct_relevel(reduced_df_long$Variable, bar_order)
  
  reduced_df_long$Value <- factor(reduced_df_long$Value, levels = rev(value_labels))
  
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count),
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(values = rev(custom_palette[1:length(value_labels)])) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      text = element_text(size = 12, family = 'sans'),
      legend.position = "top"
    )
  
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
  
  return(list(plot = plot, data = reduced_df_long_perc))
}


result <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output3.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 2", "Variable 4", "Variable 1", "Variable 6", "Variable 3", "Variable 5", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette,
  bar_order = c("q66_shared_2", "q66_shared_4", "q66_shared_1", "q66_shared_6", "q66_shared_3", "q66_shared_5", "q66_shared_7")
)


##Newest version 20240516

#' Generate Horizontal Bar Chart
#'
#' This function generates a horizontal bar chart from a given dataframe. It allows the user to specify the variable names, variable labels, value labels, and a custom color palette.
#'
#' @param df The input dataframe.
#' @param file_name The name of the output file to be saved.
#' @param question_names A vector of column names to be used for the chart.
#' @param var_labels A vector of variable labels to be used for the x-axis.
#' @param value_labels A vector of value labels to be used for the legend.
#' @param custom_palette A vector of custom colors to be used for the bars.
#'
#' @return The generated horizontal bar chart plot object.

generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette, bar_order) {
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, !!!syms(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(reduced_df, cols = -response_id, names_to = "Variable", values_to = "Value")
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Reorder the Variable column to match the desired order
  reduced_df_long$Variable <- factor(reduced_df_long$Variable, levels = bar_order)
  
  # Reorder the Value column to match the desired order
  reduced_df_long$Value <- factor(reduced_df_long$Value, levels = value_labels)
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    group_by(Variable, Value) %>%
    summarize(Count = n()) %>%
    mutate(Proportion = Count / sum(Count),
           Percentage = scales::percent(Proportion, accuracy = 1)) %>%
    ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(aes(label = Percentage), position = position_fill(vjust = 0.5), size = 4, color = "white") +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(values = custom_palette[1:length(value_labels)], 
                      breaks = rev(levels(reduced_df_long_perc$Value))) +  # Reverse the order of labels in the legend
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          text = element_text(size = 12, family = 'sans'),
          legend.position = "top")
  
  # Save the plot as a PNG file
  ggsave(file_name, plot, height = 4.5, width = 12, dpi = 1800)
  
  # Return the plot object and the reduced_df_long_perc dataframe
  return(list(plot = plot, data = reduced_df_long_perc))
}

# Sample dataframe
set.seed(123)
sample_df <- data.frame(
  response_id = 1:100,
  q66_shared_1 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_2 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_3 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_4 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_5 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_6 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_7 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE)
)

result <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output3.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 2", "Variable 4", "Variable 1", "Variable 6", "Variable 3", "Variable 5", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette,
  bar_order = c("q66_shared_2", "q66_shared_4", "q66_shared_1", "q66_shared_6", "q66_shared_3", "q66_shared_5", "q66_shared_7")
)



generate_horizontal_bar_chart <- function(df, file_name, question_names, var_labels, value_labels, custom_palette, bar_order) {
  # Require necessary libraries
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(scales)
  
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, dplyr::all_of(question_names))
  
  # Reshape the data to long format
  reduced_df_long <- tidyr::pivot_longer(
    reduced_df, 
    cols = -response_id, 
    names_to = "Variable", 
    values_to = "Value"
  )
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Ensure Variable and Value are factors with specified orders
  reduced_df_long$Variable <- factor(
    reduced_df_long$Variable, 
    levels = bar_order
  )
  
  reduced_df_long$Value <- factor(
    reduced_df_long$Value, 
    levels = value_labels
  )
  
  # Calculate the percentage of respondents for each value
  reduced_df_long_perc <- reduced_df_long %>%
    dplyr::count(Variable, Value) %>%
    dplyr::group_by(Variable) %>%
    dplyr::mutate(
      Proportion = n / sum(n),
      Percentage = scales::percent(Proportion, accuracy = 1)
    ) %>%
    dplyr::ungroup()
  
  # Create the horizontal bar chart
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(
      aes(label = Percentage), 
      position = position_fill(vjust = 0.5), 
      size = 4, 
      color = "white"
    ) +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(
      values = custom_palette[1:length(value_labels)], 
      breaks = rev(levels(reduced_df_long_perc$Value))
    ) +  # Reverse the order of labels in the legend
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      text = element_text(size = 12, family = 'sans'),
      legend.position = "top"
    )
  
  # Save the plot as a PNG file
  ggsave(
    file_name, 
    plot, 
    height = 4.5, 
    width = 12, 
    dpi = 1800
  )
  
  # Return the plot object and the reduced_df_long_perc dataframe
  return(list(plot = plot, data = reduced_df_long_perc))
}

# Sample dataframe
set.seed(123)
sample_df <- data.frame(
  response_id = 1:100,
  q66_shared_1 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_2 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_3 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_4 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_5 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_6 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE),
  q66_shared_7 = sample(c("Yes", "No", "Maybe", "Unsure"), 100, replace = TRUE)
)

result <- generate_horizontal_bar_chart(
  sample_df,
  "sample_output3.png",
  question_names = c("q66_shared_1", "q66_shared_2", "q66_shared_3", "q66_shared_4", "q66_shared_5", "q66_shared_6", "q66_shared_7"),
  var_labels = c("Variable 2", "Variable 4", "Variable 1", "Variable 6", "Variable 3", "Variable 5", "Variable 7"),
  value_labels = c("Yes", "No", "Maybe", "Unsure"),
  custom_palette = custom_palette,
  bar_order = c("q66_shared_2", "q66_shared_4", "q66_shared_1", "q66_shared_6", "q66_shared_3", "q66_shared_5", "q66_shared_7")
)



generate_horizontal_bar_chart <- function(
    df,
    file_name,
    question_names,
    var_labels,
    value_labels,
    custom_palette,
    bar_order
) {
  # Load required libraries
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)  # For str_wrap
  
  # Select relevant columns
  reduced_df <- dplyr::select(df, response_id, dplyr::all_of(question_names))
  
  # Reshape to long format
  reduced_df_long <- tidyr::pivot_longer(
    reduced_df,
    cols = -response_id,
    names_to = "Variable",
    values_to = "Value"
  )
  
  # Remove NAs
  reduced_df_long <- na.omit(reduced_df_long)
  
  # Factor levels for bar and value order
  reduced_df_long$Variable <- factor(
    reduced_df_long$Variable,
    levels = bar_order
  )
  
  reduced_df_long$Value <- factor(
    reduced_df_long$Value,
    levels = value_labels
  )
  
  # Summarize counts and percentages
  reduced_df_long_perc <- reduced_df_long |>
    count(Variable, Value) |>
    group_by(Variable) |>
    mutate(
      Proportion = n / sum(n),
      Percentage = percent(Proportion, accuracy = 1)
    ) |>
    ungroup()
  
  # Wrap long legend labels to max 15 characters per line
  legend_labels <- stringr::str_wrap(levels(reduced_df_long$Value), width = 15)
  names(legend_labels) <- levels(reduced_df_long$Value)
  
  # Plot
  plot <- ggplot(reduced_df_long_perc, aes(x = Variable, y = Proportion, fill = Value)) +
    geom_col(position = "fill", width = 0.9) +
    geom_text(
      aes(label = Percentage),
      position = position_fill(vjust = 0.5),
      size = 4,
      color = "white"
    ) +
    coord_flip() +
    scale_x_discrete(labels = var_labels) +
    scale_fill_manual(
      values = custom_palette[1:length(value_labels)],
      breaks = rev(levels(reduced_df_long$Value)),
      labels = rev(legend_labels)
    ) +
    labs(title = "", x = NULL, fill = NULL) +
    theme_minimal() +
    theme(
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      panel.grid = element_blank(),
      text = element_text(size = 12, family = "sans"),
      legend.position = "top"
    )
  
  # Save plot
  ggsave(
    file_name,
    plot,
    height = 4.5,
    width = 12,
    dpi = 1800
  )
  
  # Return output
  return(list(plot = plot, data = reduced_df_long_perc))
}
