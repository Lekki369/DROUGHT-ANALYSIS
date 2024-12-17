# Function to perform trend analysis
# This function performs a trend analysis on selected variables in a dataset using the Mann-Kendall trend test and Sen's slope estimate. It summarizes the trend results and returns them in a tidy format.
# Arguments:
#   data: The dataset containing the variables to be analyzed.
#   variables: A vector of variable names to analyze for trends.
perform_trend_analysis <- function(data, variables) {
  # Summarize the trend results for each variable
  trend_results <- data %>%
    summarise(across(
      all_of(variables),
      list(
        Mann_Tau = ~ MannKendall(.)$tau,           # Mann-Kendall Tau
        Mann_pvalue = ~ MannKendall(.)$sl,         # Mann-Kendall p-value
        Sen_slope = ~ sens.slope(.)$estimate       # Sen's slope estimate
      ),
      .names = "{.col}_{.fn}"
    ))
  
  # Transform the summarized results into a tidy format
  tidy_results <- trend_results %>%
    pivot_longer(
      cols = everything(),
      names_to = c("Variable", "Type", "Statistic"),
      names_sep = "_",
      values_to = "Value"
    )
  
  return(tidy_results)
}

# Function to plot trends
# This function creates a scatter plot with a trend line for a given dataset. It uses the ggplot2 package to create the plot and adds a smooth trend line using LOESS.
# Arguments:
#   data: The dataset to be plotted.
#   x_var: The name of the variable to be used for the x-axis.
#   y_var: The name of the variable to be used for the y-axis.
#   color: The color of the points in the plot.
#   title: The title of the plot.
#   caption: The caption for the plot.
plot_trend_analysis <- function(data, x_var, y_var, color, title, caption) {
  # Create the plot
  plot <- ggplot(data, aes_string(x = x_var)) +
    geom_point(aes_string(y = y_var), color = color, size = 2) +
    geom_smooth(aes_string(y = y_var), method = "loess", se = FALSE, color = "red") +
    labs(
      title = title,
      x = x_var,
      y = y_var,
      caption = caption
    ) +
    theme_minimal()
  
  print(plot)
  return(plot)
}

