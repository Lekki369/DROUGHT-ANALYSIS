# Purpose:
# This function creates a side-by-side bar plot comparing two drought indices (SPI and SPEI) over time (years). 
# The plot displays both indices in different colors for each year, showing their values side by side for comparison.

# Function: indices_bar_plot
# This function generates a bar plot that shows the values of SPI and SPEI for each year. 
# The two indices are displayed as side-by-side bars for each year.

# Parameters:
# - data: A data frame containing the year-wise values of SPI and SPEI.
# - y_vars: A vector of two strings, each representing the column names for SPI and SPEI.
# - title: The title of the plot.
# - y_label: The label for the y-axis.

# Returns:
# - A ggplot object representing the side-by-side bar plot comparing the two indices over time.

# Example:
# indices_bar_plot(data = drought_data, y_vars = c("spi", "spei"), title = "SPI and SPEI Over Time", y_label = "Index Value")
# This will generate a bar plot showing SPI and SPEI values for each year.

indices_bar_plot <- function(data, y_vars, title, y_label) {
  plot <- ggplot(data, aes(x = factor(Year))) +  # Use factor for Year to treat it as discrete values
    geom_col(aes(y = !!sym(y_vars[1]), fill = "SPI"), width = 0.1, position = position_dodge(width = 0.2)) +  # Side-by-side dodge for SPI
    geom_col(aes(y = !!sym(y_vars[2]), fill = "SPEI"), width = 0.1, position = position_dodge(width = 0.3)) +  # Same dodge for SPEI
    scale_x_discrete(breaks = seq(min(data$Year), max(data$Year), by = 2)) +  # Year intervals set to 2
    labs(title = title, x = "Year", y = y_label) +
    scale_fill_manual(values = c("SPI" = "blue", "SPEI" = "red")) +  # Custom colors for SPI and SPEI
    theme_minimal() +
    theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))  # Remove legend title and rotate x-axis labels
  print(plot)
  return(plot)
}

# Purpose:
# This function generates a line plot comparing two drought indices (e.g., SPI and SPEI) over time (years).
# The indices are reshaped into a long format, and the plot is created with different colors for each index.

# Function: indices_line_plot
# This function creates a line plot that compares two drought indices over time. It reshapes the data from wide to long format and plots the values as lines for each index.

# Parameters:
# - data: A data frame containing the year-wise values of the indices.
# - y_vars: A vector of two strings representing the column names of the indices to be plotted.
# - title: The title of the plot.
# - y_label: The label for the y-axis.

# Returns:
# - A ggplot object representing the line plot comparing the indices over time.

# Example:
# indices_line_plot(data = drought_data, y_vars = c("spi", "spei"), title = "SPI vs SPEI Over Time", y_label = "Index Value")
# This will generate a line plot showing SPI and SPEI values over the years.

indices_line_plot <- function(data, y_vars, title, y_label) {
  # Reshape data for ggplot2 from wide to long format
  data <- data %>%
    gather(key = "Index", value = "Value", all_of(y_vars))
  
  # Dynamically create a color vector for each index
  colors <- c("red", "blue")[seq_along(y_vars)]
  names(colors) <- y_vars
  
  # Check the levels of the Index variable
  print("Levels of Index after gathering:")
  print(unique(data$Index))
  
  # Plot with dynamically specified colors for each index
  plot <- ggplot(data, aes(x = Year, y = Value, color = Index)) + 
    geom_line(size = 1) +  # Plot the indices as lines
    scale_x_continuous(breaks = seq(min(data$Year), max(data$Year), by = 2)) +  # Set year intervals
    labs(title = title, x = "Year", y = y_label) +
    scale_color_manual(values = colors) +  # Apply custom colors to the indices
    theme_minimal() +
    theme(
      legend.title = element_blank(),  # Remove the legend title
      axis.text.x = element_text(angle = 90, hjust = 1)  # Rotate x-axis labels for readability
    ) +
    geom_hline(yintercept = 0, color = "black", size = 1.5)  # Add a bold black horizontal line at y = 0
  
  print(plot)
  return(plot)
}
