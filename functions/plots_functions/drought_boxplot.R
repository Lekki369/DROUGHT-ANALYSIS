# Purpose:
# This function creates a boxplot to visualize the distribution of drought event counts 
# across different drought categories. It shows the spread and outliers for each category.

# Function: plot_drought_boxplot
# This function generates a boxplot to compare the distribution of drought event counts 
# across different categories. The categories can be related to drought severity, such as 
# mild, moderate, severe, and extreme drought.

# Parameters:
# - data: A data frame containing the drought event data.
# - category: A string representing the column name for the drought category (e.g., "spi_category").
# - count_col: A string representing the column name for the count of drought events (e.g., "count").
# - title: The title of the plot (default is "Boxplot of Drought Categories").

# Returns:
# - A ggplot object representing the boxplot showing the count of drought events by category.

# Example:
# plot_drought_boxplot(data = drought_data, category = "spi_category", count_col = "count", title = "Drought Categories Boxplot")
# This will generate a boxplot showing the distribution of drought event counts for each category.

plot_drought_boxplot <- function(data, category, count_col, title = "Boxplot of Drought Categories") {
  plot <- ggplot(data, aes_string(x = category, y = count_col)) +
    geom_boxplot() +
    labs(
      title = title,
      x = "Drought Category",
      y = "Count of Drought Events"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better readability
  print(plot)
  return(plot)
}
