
#' Plot a single Y-variable line chart over time with error bars
#'
#' This function creates a minimalist line plot of a Y variable against an X-axis
#' factor (e.g., time), with standard error bars and group-colored lines.
#' Thin X and Y axis lines are preserved, but all gridlines and background
#' are removed for a clean look.
#'
#' @param dataframe A data frame containing the input data.
#' @param x_var Unquoted name of the factor X-axis variable (e.g., wave or year).
#' @param y_var Unquoted name of the numeric Y-axis variable.
#' @param color_var Unquoted name of the grouping/coloring variable.
#' @param se_var Unquoted name of the standard error variable for Y.
#' @param plot_title Optional title for the plot.
#' @param x_label Optional x-axis label.
#' @param y_label Optional y-axis label.
#' @param plot_name Optional file name to save the plot as PNG.
#'
#' @return A ggplot object.
#'
plot_single_line_with_error <- function(
    dataframe,
    x_var,
    y_var,
    color_var,
    se_var,
    plot_title = "Line Plot",
    x_label = NULL,
    y_label = NULL,
    plot_name = "line_plot_single_x.png"
) {
  custom_palette <- c("#002A4E", "#36749D", "#85714D", "#004F51", "#000000", "#95174C", "#DDE9F0")
  
  p <- ggplot(
    dataframe,
    aes(
      x = {{ x_var }},
      y = {{ y_var }},
      group = factor({{ color_var }}),
      color = factor({{ color_var }})
    )
  ) +
    geom_line(size = 0.8) +
    geom_point(size = 2) +
    geom_errorbar(
      aes(
        ymin = {{ y_var }} - {{ se_var }},
        ymax = {{ y_var }} + {{ se_var }}
      ),
      width = 0.1
    ) +
    scale_color_manual(values = custom_palette) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      color = NULL
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(color = "black", size = 0.3),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      text = element_text(family = "sans", size = 10)
    )
  
  # Save the plot as a high-resolution PNG
  ggsave(
    filename = plot_name,
    plot = p,
    width = 12,
    height = 6,
    dpi = 1800
  )
  
  return(p)
}

example_data <- data.frame(
  wave = factor(
    c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2"),
    levels = c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2"),
    ordered = TRUE
  ),
  response = c(0.2, 0.3, 0.4, 0.5, 0.45, 0.6, 0.65),
  se = rep(0.02, 7),
  gender = rep(c("M", "F"), length.out = 7)
)

plot_single_line_with_error(
  dataframe = example_data,
  x_var = wave,
  y_var = response,
  color_var = gender,
  se_var = se,
  plot_title = "Response by Survey Wave",
  x_label = "Survey Wave",
  y_label = "Response Rate",
  plot_name = "response_by_wave.png"
)
require(ggplot2)


plot_single_line_with_smooth <- function(
    dataframe,
    x_var,
    y_var,
    color_var,
    plot_title = "Line Plot",
    x_label = NULL,
    y_label = NULL,
    plot_name = "line_plot_smooth.png",
    alpha_ribbon = 0.3
) {
  custom_palette <- c("#002A4E", "#36749D", "#85714D", "#004F51", "#000000", "#95174C", "#DDE9F0")
  
  p <- ggplot(
    dataframe,
    aes(
      x = {{ x_var }},
      y = {{ y_var }},
      group = factor({{ color_var }}),
      color = factor({{ color_var }}),
      fill = factor({{ color_var }})
    )
  ) +
    geom_smooth(
      method = "loess",
      se = TRUE,
      alpha = alpha_ribbon,
      size = 0.8
    ) +
    geom_point(size = 2) +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette) +
    labs(
      title = plot_title,
      x = x_label,
      y = y_label,
      color = NULL,
      fill = NULL
    ) +
    theme(
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.3, color = "black"),
      axis.ticks = element_line(color = "black", size = 0.3),
      axis.text = element_text(color = "black", size = 10),
      axis.title = element_text(color = "black", size = 11),
      plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = 10),
      text = element_text(family = "sans", size = 10)
    ) +
    guides(fill = "none")  # Hide fill legend since it's the same as color
  
  # Save the plot as a high-resolution PNG
  ggsave(
    filename = plot_name,
    plot = p,
    width = 12,
    height = 6,
    dpi = 1800
  )
  
  return(p)
}

# Example usage with the same data
example_data <- data.frame(
  wave = factor(
    c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2"),
    levels = c("2023 Q4", "2024 Q1", "2024 Q2", "2024 Q3", "2024 Q4", "2025 Q1", "2025 Q2"),
    ordered = TRUE
  ),
  response = c(0.2, 0.3, 0.4, 0.5, 0.45, 0.6, 0.65),
  gender = rep(c("M", "F"), length.out = 7)
)

plot_single_line_with_smooth(
  dataframe = example_data,
  x_var = wave,
  y_var = response,
  color_var = gender,
  plot_title = "Response by Survey Wave (with Smooth Confidence Band)",
  x_label = "Survey Wave",
  y_label = "Response Rate",
  plot_name = "response_by_wave_smooth.png"
)