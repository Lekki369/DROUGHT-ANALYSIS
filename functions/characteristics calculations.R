### This function calculates the drought Exposure, Vulnerability & Resilience characteristics
### which is called by [add_drought_characteristics] function
### [data]

calculate_exposure <- function(data, drought_column, total_years_count) {
  total_drought_duration <- sum(data[[drought_column]] < 0, na.rm = TRUE)
  Ex <- total_drought_duration / total_years_count
  return(Ex)
}

calculate_vulnerability <- function(data, drought_column) {
  SPI_drought <- data[[drought_column]][data[[drought_column]] < 0 & !is.na(data[[drought_column]])]
  M <- length(SPI_drought)
  Vu <- if (M > 0) sum(abs(SPI_drought), na.rm = TRUE) / M else NA
  return(Vu)
}

calculate_resilience <- function(data, drought_column) {
  drought_durations <- data[[drought_column]][data[[drought_column]] < 0 & !is.na(data[[drought_column]])]
  M <- length(drought_durations)
  sum_DDi <- sum(drought_durations, na.rm = TRUE)
  Re <- if (M > 0 & sum_DDi != 0) M / sum_DDi else NA
  return(Re)
}
