# Purpose:
# This function generates a bar plot showing the total count of drought events across different categories (e.g., mild, moderate, severe, etc.)
# for each year or another index, based on the data provided. The plot allows comparison of drought categories over time or different indices.

# Function: plot_drought_categories
# This function creates a bar plot displaying the total count of drought events categorized by a specified drought category 
# (e.g., mild, moderate, severe, etc.). It supports faceting the plot by a specified index (such as year or region), 
# with a color fill for better visualization.

# Parameters:
# - data: The data frame containing the drought data, including the categories and counts.
# - index_col: The column name representing the index for faceting (e.g., Year, Region).
# - category: The column name representing the drought category (e.g., "spi3_cat" or "spei6_cat").
# - count_col: The column name representing the count of events in each category (e.g., "count").
# - title_prefix: A string to prefix the plot's title to indicate the subject of the plot (e.g., "Yearly").

# Returns:
# - A ggplot object representing the bar plot visualizing drought categories across years or indices.
#
# Example:
# plot_drought_categories(data = drought_data, index_col = "Year", category = "spi3_cat", count_col = "count", title_prefix = "Yearly")
# This will generate a bar plot showing the total count of drought events for each SPI3 category over the years.

plot_drought_categories <- function(data, index_col, category, count_col, title_prefix) {
  # Create the bar plot using ggplot2
  plot <- ggplot(data, aes_string(x = category, y = count_col, fill = category)) +
    geom_bar(stat = "identity", position = "dodge") +  # Bar plot with dodge position for multiple bars
    labs(
      title = paste0(title_prefix, " Drought Categories Over the Years"),  # Title with prefix
      x = "Drought Category",  # Label for x-axis
      y = "Total Count of Drought Events"  # Label for y-axis
    ) +
    theme_minimal() +  # Minimalist theme for the plot
    theme(legend.title = element_blank()) +  # Hide legend title
    scale_fill_viridis_d() +  # Use a color palette (viridis)
    facet_wrap(as.formula(paste("~", index_col)), ncol = 3)  # Faceting by the specified index column (e.g., Year)
  
  # Print and return the plot
  print(plot)
  return(plot)  # Return the ggplot object for further use
}
