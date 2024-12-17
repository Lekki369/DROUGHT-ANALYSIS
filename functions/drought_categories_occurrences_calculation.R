# Function to calculate total counts of drought categories
# 
# Purpose:
# This function calculates the total number of occurrences of each drought category (Mild, Moderate, Severe, Extreme) 
# for different time scales (SPI3, SPI6, SPI12, SPEI3, SPEI6, and SPEI12) from a data frame. It helps in summarizing the 
# frequency of drought events based on different indices (SPI and SPEI) for short, medium, and long-term periods.
#
# Parameters:
# - data: A data frame that contains columns for categorized drought indices (e.g., spi3_cat, spi6_cat, spei3_cat, etc.) 
#         which indicate the drought category for each observation.
#
# Returns:
# - A data frame with two columns:
#   - category: The drought category (e.g., "spi3_mild_total", "spi6_severe_total").
#   - count: The number of occurrences for each drought category.
#
# Example:
#  total_drought_categories_occurrences_calculation(data)
# This will calculate the total counts for each drought category across the SPI and SPEI indices (for 3, 6, and 12 months)
# and return the results in a summarized data frame.
 total_drought_categories_occurrences_calculation <- function(data) {
  drought_totals <- data %>%
    summarise(
      # SPI3 counts
      spi3_mild_total = sum(spi3_cat == 'Mild Drought', na.rm = TRUE),
      spi3_moderate_total = sum(spi3_cat == "Moderate Drought", na.rm = TRUE),
      spi3_severe_total = sum(spi3_cat == "Severe Drought", na.rm = TRUE),
      spi3_extreme_total = sum(spi3_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPI6 counts
      spi6_mild_total = sum(spi6_cat == 'Mild Drought', na.rm = TRUE),
      spi6_moderate_total = sum(spi6_cat == "Moderate Drought", na.rm = TRUE),
      spi6_severe_total = sum(spi6_cat == "Severe Drought", na.rm = TRUE),
      spi6_extreme_total = sum(spi6_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPI12 counts
      spi12_mild_total = sum(spi12_cat == 'Mild Drought', na.rm = TRUE),
      spi12_moderate_total = sum(spi12_cat == "Moderate Drought", na.rm = TRUE),
      spi12_severe_total = sum(spi12_cat == "Severe Drought", na.rm = TRUE),
      spi12_extreme_total = sum(spi12_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI3 counts
      spei3_mild_total = sum(spei3_cat == 'Mild Drought', na.rm = TRUE),
      spei3_moderate_total = sum(spei3_cat == "Moderate Drought", na.rm = TRUE),
      spei3_severe_total = sum(spei3_cat == "Severe Drought", na.rm = TRUE),
      spei3_extreme_total = sum(spei3_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI6 counts
      spei6_mild_total = sum(spei6_cat == 'Mild Drought', na.rm = TRUE),
      spei6_moderate_total = sum(spei6_cat == "Moderate Drought", na.rm = TRUE),
      spei6_severe_total = sum(spei6_cat == "Severe Drought", na.rm = TRUE),
      spei6_extreme_total = sum(spei6_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI12 counts
      spei12_mild_total = sum(spei12_cat == 'Mild Drought', na.rm = TRUE),
      spei12_moderate_total = sum(spei12_cat == "Moderate Drought", na.rm = TRUE),
      spei12_severe_total = sum(spei12_cat == "Severe Drought", na.rm = TRUE),
      spei12_extreme_total = sum(spei12_cat == "Extreme Drought", na.rm = TRUE)
    ) %>%
    pivot_longer(cols = everything(), names_to = "category", values_to = "count") 
  
  # Drop additional columns manually by checking names and removing non-relevant ones
  drought_totals <- drought_totals[, c("category", "count")]
  
  return(drought_totals)
}


# Function to calculate drought counts
# This function calculates the number of occurrences of different drought categories for each year. It groups the data by year and counts the occurrences of each drought category for various indices (SPI3, SPEI3, SPI6, SPEI6, SPI12, SPEI12).
# Arguments:
#   data: The dataset containing the drought information with year and drought category columns.
yearly_drought_categories_occurrences_calculation <- function(data) {
  # Group data by year and calculate the occurrences of different drought categories 
  drought_counts <- data %>%
    group_by(Year) %>%
    summarise(
      # SPI3 categories
      spi3_mild = sum(spi3_cat == 'Mild Drought', na.rm = TRUE),
      spi3_moderate = sum(spi3_cat == "Moderate Drought", na.rm = TRUE),
      spi3_severe = sum(spi3_cat == "Severe Drought", na.rm = TRUE),
      spi3_extreme = sum(spi3_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI3 categories
      spei3_mild = sum(spei3_cat == "Mild Drought", na.rm = TRUE),
      spei3_moderate = sum(spei3_cat == "Moderate Drought", na.rm = TRUE),
      spei3_severe = sum(spei3_cat == "Severe Drought", na.rm = TRUE),
      spei3_extreme = sum(spei3_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPI6 categories
      spi6_mild = sum(spi6_cat == "Mild Drought", na.rm = TRUE),
      spi6_moderate = sum(spi6_cat == "Moderate Drought", na.rm = TRUE),
      spi6_severe = sum(spi6_cat == "Severe Drought", na.rm = TRUE),
      spi6_extreme = sum(spi6_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI6 categories
      spei6_mild = sum(spei6_cat == "Mild Drought", na.rm = TRUE),
      spei6_moderate = sum(spei6_cat == "Moderate Drought", na.rm = TRUE),
      spei6_severe = sum(spei6_cat == "Severe Drought", na.rm = TRUE),
      spei6_extreme = sum(spei6_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPI12 categories
      spi12_mild = sum(spi12_cat == 'Mild Drought', na.rm = TRUE),
      spi12_moderate = sum(spi12_cat == "Moderate Drought", na.rm = TRUE),
      spi12_severe = sum(spi12_cat == "Severe Drought", na.rm = TRUE),
      spi12_extreme = sum(spi12_cat == "Extreme Drought", na.rm = TRUE),
      
      # SPEI12 categories
      spei12_mild = sum(spei12_cat == "Mild Drought", na.rm = TRUE),
      spei12_moderate = sum(spei12_cat == "Moderate Drought", na.rm = TRUE),
      spei12_severe = sum(spei12_cat == "Severe Drought", na.rm = TRUE),
      spei12_extreme = sum(spei12_cat == "Extreme Drought", na.rm = TRUE)
    )
  
  return(drought_counts)
}

