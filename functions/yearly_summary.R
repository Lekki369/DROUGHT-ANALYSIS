### This function return a new dataframe for the columns below
### yearly using the summarise function
yearly_summary_function <- function(data) {
  year_summary <- data %>%
    group_by(Year) %>%
    summarise(
      Tmin = mean(Tmin, na.rm = TRUE),
      Tmax = mean(Tmax, na.rm = TRUE),
      Prec. = mean(Prec., na.rm = TRUE),
      spi3 = mean(spi3, na.rm = TRUE),
      spi6 = mean(spi6, na.rm = TRUE),
      spi12 = mean(spi12, na.rm = TRUE),
      spei3 = mean(spei3, na.rm = TRUE),
      spei6 = mean(spei6, na.rm = TRUE),
      spei12 = mean(spei12, na.rm = TRUE)
      
    )
  return(year_summary)
}

