# Function to calculate climate indices
# 
# Purpose:
# This function calculates several important climate indices such as Potential Evapotranspiration (PET), 
# the Standardized Precipitation Index (SPI), and the Standardized Precipitation-Evapotranspiration Index (SPEI) for 
# different time periods (3, 6, and 12 months). It also calculates the Aridity Index, SHI (Shannon's Hydrological Index), 
# and the water balance (cwbal) based on precipitation and temperature data.
#
# Parameters:
# - data: A data frame containing climate data with columns for temperature (Tmin, Tmax) and precipitation (Prec.).
# - lat_col: The latitude value or column used to calculate Potential Evapotranspiration (PET) using the Hargreaves method.
#
# Returns:
# - A data frame with additional columns containing calculated indices:
#   - PET: Potential Evapotranspiration calculated using the Hargreaves method.
#   - SHI: Shannon's Hydrological Index, which is the ratio of precipitation to PET.
#   - cwbal: Water balance, which is the difference between precipitation and PET.
#   - Tmean: The mean temperature (average of Tmin and Tmax).
#   - Aridity_Index: A measure of dryness, the ratio of precipitation to PET.
#   - spi3, spi6, spi12: Standardized Precipitation Index for 3, 6, and 12 months, which measures the deviation of precipitation from the long-term average.
#   - spei3, spei6, spei12: Standardized Precipitation-Evapotranspiration Index for 3, 6, and 12 months, which accounts for both precipitation and evapotranspiration.
#
# Example:
# calculate_indices(data, lat_col = 45.0)
# This will add the calculated indices to the 'data' frame based on the given latitude value.
calculate_indices <- function(data, lat_col) {
  # Calculate PET, SHI, and other indices
  data <- data %>%
    mutate(
      PET = hargreaves(Tmin = Tmin, Tmax = Tmax, lat = lat_col),
      SHI = Prec. / PET,
      cwbal = Prec. - PET,
      Tmean = (Tmin + Tmax) / 2,
      Aridity_Index = Prec. / PET,
      spi3 = spi(Prec., 3)$fitted,
      spi6 = spi(Prec., 6)$fitted,
      spi12 = spi(Prec., 12)$fitted,
      spei3 = spei(cwbal, 3)$fitted,
      spei6 = spei(cwbal, 6)$fitted,
      spei12 = spei(cwbal, 12)$fitted
    )
  return(data)
}
