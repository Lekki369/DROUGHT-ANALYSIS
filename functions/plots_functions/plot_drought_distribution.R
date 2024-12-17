# Purpose:
# This function creates a bar plot to visualize the distribution of drought categories across the data, 
# showing the total count of occurrences for each drought category. The plot is color-filled based on the drought category.
#
# The function uses the `ggplot2` package to create a clean and visually appealing bar chart, 
# which can be used to analyze the frequency of different drought categories in the dataset.

# Function: plot_drought_distribution
# This function generates a bar plot showing the distribution of drought categories 
# based on the total count of occurrences. The categories are color-coded, and the x-axis labels are rotated for better readability.

# Parameters:
# - data: A data frame containing the drought data, including the category and counts.
# - category: The name of the column representing the drought category (e.g., "spi3_cat" or "spei6_cat").
# - count_col: The name of the column representing the count of events in each drought category (e.g., "count").
# - title_prefix: A string to prefix the plot's title to indicate the subject of the plot (e.g., "Yearly").

# Returns:
# - A ggplot object representing the bar plot visualizing the drought category distribution.

# Example:
# plot_drought_distribution(data = drought_data, category = "spi3_cat", count_col = "count", title_prefix = "Yearly")
# This will generate a bar plot showing the total count of drought events for each SPI3 category over the years.

plot_drought_distribution <- function(data, category, count_col, title_prefix) {
  # Create the plot using ggplot2
  plot <- ggplot(data, aes(x = !!sym(category), y = !!sym(count_col), fill = !!sym(category))) +
    geom_bar(stat = "identity") +  # Create the bar plot
    labs(
      title = paste0(title_prefix, " Drought Category Distribution"),  # Title with prefix
      x = "Drought Category",  # Label for the x-axis
      y = "Total Count"  # Label for the y-axis
    ) +
    theme_minimal() +  # Apply a minimalist theme for a clean look
    scale_fill_viridis_d() +  # Use a color palette (viridis) for the drought categories
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability
  
  # Print and return the plot
  print(plot)
  return(plot)  # Return the ggplot object for further use
}
