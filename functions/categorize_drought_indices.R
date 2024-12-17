# Define categorization functions for SPI and SPEI

# Purpose:
# These functions categorize the Standardized Precipitation Index (SPI) and Standardized Precipitation Evapotranspiration Index (SPEI)
# into different drought categories based on their values. The categories include "Extreme Drought", "Severe Drought", 
# "Moderate Drought", "Mild Drought", and "No Drought". These indices are used to assess drought conditions based on 
# precipitation and its relationship with evapotranspiration.

# Function 1: categorize_spi
# This function categorizes the SPI values into the following drought categories:
# - "Extreme Drought" for SPI values less than or equal to -2.0
# - "Severe Drought" for SPI values between -2.0 and -1.5
# - "Moderate Drought" for SPI values between -1.5 and -1.0
# - "Mild Drought" for SPI values between -1.0 and -0.5
# - "No Drought" for SPI values greater than -0.5

# Parameters:
# - spi: A numeric value representing the Standardized Precipitation Index.
# 
# Returns:
# - A string representing the drought category corresponding to the SPI value.

categorize_spi <- function(spi) {
  case_when(
    spi <= -2.0 ~ "Extreme Drought",
    spi > -2.0 & spi <= -1.5 ~ "Severe Drought",
    spi > -1.5 & spi <= -1.0 ~ "Moderate Drought",
    spi > -1.0 & spi <= -0.5 ~ "Mild Drought",
    TRUE ~ "No Drought"
  )
}

# Function 2: categorize_spei
# This function categorizes the SPEI values into the following drought categories:
# - "Extreme Drought" for SPEI values less than or equal to -2.0
# - "Severe Drought" for SPEI values between -2.0 and -1.5
# - "Moderate Drought" for SPEI values between -1.5 and -1.0
# - "Mild Drought" for SPEI values between -1.0 and -0.5
# - "No Drought" for SPEI values greater than -0.5

# Parameters:
# - spei: A numeric value representing the Standardized Precipitation Evapotranspiration Index.
#
# Returns:
# - A string representing the drought category corresponding to the SPEI value.

categorize_spei <- function(spei) {
  case_when(
    spei <= -2.0 ~ "Extreme Drought",
    spei > -2.0 & spei <= -1.5 ~ "Severe Drought",
    spei > -1.5 & spei <= -1.0 ~ "Moderate Drought",
    spei > -1.0 & spei <= -0.5 ~ "Mild Drought",
    TRUE ~ "No Drought"
  )
}
