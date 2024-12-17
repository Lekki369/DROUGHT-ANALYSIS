# Function to compute drought characteristics for each year
# 
# Purpose:
# This function calculates several key drought characteristics (such as severity, duration, frequency, and intensity) 
# for each year based on a given drought index (e.g., a drought severity index or rainfall deficit).
# It groups the data by year and computes statistics related to drought.
#
# Parameters:
# - data: A data frame containing the data to analyze. The data should include at least two columns: 
#   one for the year (e.g., 'Year') and one for the drought index (e.g., 'DroughtIndex').
# - index_name: A character string specifying the column name of the drought index in the data frame.
#
# Returns:
# - A data frame grouped by year with the following columns:
#   - **Mean**: The average value of the drought index for each year.
#   - **Lat**: The total value of the 'Lat' column for each year (assuming 'Lat' is some relevant value to sum).
#   - **Severity**: The total value of the drought index for each year.
#   - **Duration**: The number of months in which the drought index was below zero (indicating drought).
#   - **Frequency**: The percentage of months in the year where the drought index was below zero.
#   - **Intensity**: The ratio of severity to duration, indicating the intensity of the drought.
#
# Example:
# compute_drought_characteristics(data, "DroughtIndex")
# This will calculate the drought characteristics based on the "DroughtIndex" column, grouped by year.
compute_drought_characteristics <- function(data, index_name) {
  # Create a new name for the mean of the specified drought index
  index_mean_name <- paste0(index_name, "_Mean")
  
  # Group data by Year and calculate various drought characteristics
  data %>%  
    group_by(Year) %>% 
    summarise(
      # Calculate the mean of the drought index for each year
      !!index_mean_name := mean(!!sym(index_name), na.rm = TRUE),
      # Sum the 'Lat' values for each year
      Lat = sum(Lat, na.rm = TRUE),
      # Calculate the total severity (sum of the drought index values for each year)
      Severity = sum(!!sym(index_name), na.rm = TRUE),
      # Calculate the duration: the number of months where the drought index was below 0
      Duration = sum(!!sym(index_name) < 0, na.rm = TRUE),
      # Calculate the frequency: the percentage of months in the year with drought (index < 0)
      Frequency = round(sum(!!sym(index_name) < 0, na.rm = TRUE) * 100 / 12, 2),
      # Calculate the intensity: severity divided by duration
      Intensity = round(Severity / Duration, 2)
    )
}
