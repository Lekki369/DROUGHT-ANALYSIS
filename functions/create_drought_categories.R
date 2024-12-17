# Function to categorize data into drought levels based on predefined ranges.
# 
# Purpose:
# This function assigns a drought category to each data point based on its value.
# The function splits the data into specified ranges and labels them accordingly, 
# providing an easy way to classify data into categories like "Extreme drought" or "Very wet".
#
# Parameters:
# - data: A numeric vector containing the values to be categorized (e.g., drought index or rainfall data).
# - breaks: A numeric vector defining the interval boundaries for categorizing the data. Default splits the data 
#   into ranges such as "Extreme drought" to "Extremely wet".
# - labels: A character vector with labels that correspond to the intervals defined in 'breaks'. Default labels 
#   range from "Extreme drought" to "Moderately wet".
#
# Returns:
# A factor with categories corresponding to the ranges defined by 'breaks' and labeled by 'labels'.
#
# Example:
# data <- c(-2.5, -1.2, 0.5, 1.8)
# create_drought_categories(data)
# This will return:
# "Extreme drought" "Moderate drought" "Moderately wet" "Very wet"
create_drought_categories <- function(data, breaks = c(-Inf, -2, -1.5, -1, -0.99, 0.99, 1.0, 1.49, 1.99, 2.00, Inf), 
                                      labels = c("Extreme drought", "Severe drought", "Moderate drought", "Mild drought", 
                                                 "Moderately wet", "Very wet", "Extremely wet", "Extremely wet", 
                                                 "Very wet", "Moderately wet")) {
  # Split data into categories based on 'breaks' and assign 'labels' to those categories
  cut(data, breaks = breaks, labels = labels, right = FALSE)
}
