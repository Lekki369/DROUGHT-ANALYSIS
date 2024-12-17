# Purpose:
# This function creates a scatter plot to visualize the relationship between two drought indices (such as SPI and SPEI)
# by plotting them against each other. The plot includes a linear regression line to show the trend, and it annotates the 
# plot with the equation of the linear regression line.

# Function: plot_drought_index_relationship
# This function generates a scatter plot with a regression line between two specified drought indices. It filters out 
# invalid data (e.g., NA, NaN, or infinite values) and annotates the plot with the linear regression equation.
# 
# Parameters:
# - df: The data frame containing the drought indices to be plotted.
# - x_var: The name of the column (variable) representing the drought index to be plotted on the x-axis.
# - y_var: The name of the column (variable) representing the drought index to be plotted on the y-axis.
# - title: The title for the plot.
# 
# Returns:
# - A ggplot object representing the scatter plot with the regression line.
# 
# Example:
# plot_drought_index_relationship(df = data, x_var = "spi3", y_var = "spei3", title = "SPI3 vs SPEI3")
# This will plot SPI3 against SPEI3, with a regression line and the equation.

plot_drought_index_relationship <- function(df, x_var, y_var, title) {
  # Clean the data to remove invalid (NA, infinite, or NaN) values
  df_clean <- df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]), 
           !is.infinite(.data[[x_var]]), !is.infinite(.data[[y_var]]), 
           !is.nan(.data[[x_var]]), !is.nan(.data[[y_var]]))
  
  # Create the scatter plot with a linear regression line
  plot <-  ggplot(df_clean, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") +  # Scatter plot points
    geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
    labs(title = title, x = x_var, y = y_var) +  # Add title and labels
    annotate("text", x = min(df_clean[[x_var]], na.rm = TRUE), y = max(df_clean[[y_var]], na.rm = TRUE), 
             label = lm_eqn(df_clean, x_var, y_var), parse = TRUE, hjust = 0)  # Annotate with regression equation
  
  print(plot)
  return(plot)  # Return the plot object
}
