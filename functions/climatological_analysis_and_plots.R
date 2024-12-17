# Climatological Year Analysis Function
# 
# Purpose:
# This function analyzes climate data over specified periods, categorizing each year into different periods
# (e.g., base period, climate change onset, accelerated climate change) and calculates climate trends (such as
# temperature and precipitation trends) using statistical methods like Mann-Kendall and Sen's slope.
#
# Parameters:
# - df: A data frame containing climate data, including columns for year (e.g., 'Year'), temperature (e.g., 'Tmin', 'Tmax'),
#   and precipitation (e.g., 'Prec.').
# - base_period: A numeric vector indicating the years considered the "Base Period".
# - change_period1: A numeric vector indicating the years representing the "Climate Change Onset".
# - change_period2: A numeric vector indicating the years representing the "Accelerated Climate Change".
#
# Returns:
# - A list with two elements:
#   - **Data**: The original data frame with an added 'Period' column categorizing each year into one of the periods.
#   - **Trends**: A data frame containing the Mann-Kendall trend and Sen's slope estimates for Tmin, Tmax, and Precipitation
#     for each of the defined periods.
#
# Example:
# climatological_analysis(df, base_period = 1980:2000, change_period1 = 2001:2010, change_period2 = 2011:2020)
# This will categorize the data into periods and calculate the trends for each period.
climatological_analysis <- function(df, base_period, change_period1, change_period2) {
  # Categorize years into periods
  df <- df %>%
    mutate(Period = case_when(
      Year %in% base_period ~ "Base Period",
      Year %in% change_period1 ~ "Climate Change Onset",
      Year %in% change_period2 ~ "Accelerated Climate Change",
      TRUE ~ "Other"
    ))
  
  # Perform trend analysis for Tmin, Tmax, and Precipitation
  trend_analysis <- df %>%
    group_by(Period) %>%
    summarise(
      Tmin_Trend = ifelse(n() >= 3, MannKendall(Tmin)$tau, NA),
      Tmax_Trend = ifelse(n() >= 3, MannKendall(Tmax)$tau, NA),
      Prec_Trend = ifelse(n() >= 3, MannKendall(Prec.)$tau, NA),
      Tmin_Sen_Slope = ifelse(n() >= 3, sens.slope(Tmin)$estimate, NA),
      Tmax_Sen_Slope = ifelse(n() >= 3, sens.slope(Tmax)$estimate, NA),
      Prec_Sen_Slope = ifelse(n() >= 3, sens.slope(Prec.)$estimate, NA)
    )
  
  return(list(Data = df, Trends = trend_analysis))
}

# Function for timeseries plot with trend line
# 
# Purpose:
# This function generates a time series plot for a given data frame and adds a trend line (linear model) to the plot.
# The plot will visualize how a variable changes over time and include a red trend line.
#
# Parameters:
# - data: A data frame containing the data to plot.
# - x: The name of the column to be plotted on the x-axis (e.g., "Year").
# - y: The name of the column to be plotted on the y-axis (e.g., "Tmin").
# - title: The title of the plot.
# - x_label: The label for the x-axis.
# - y_label: The label for the y-axis.
#
# Returns:
# - A ggplot object representing the time series plot with a trend line.
#
# Example:
# generate_timeseries_plot(df, "Year", "Tmin", "Temperature Trend", "Year", "Temperature")
# This will generate a time series plot of the 'Tmin' variable over 'Year' with a red trend line.
generate_timeseries_plot <- function(data, x, y, title, x_label, y_label) {
  plot <- ggplot(data, aes_string(x = x, y = y)) +
    geom_line() +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal()
  return(plot)
}

# Function to create and print plots for a specific period
# 
# Purpose:
# This function generates and prints multiple time series plots for various climate indices (e.g., SPI, SPEI, temperature)
# for a specific period. It filters the data based on the specified period and generates plots for the selected indices.
#
# Parameters:
# - data: A data frame containing climate data with a 'Period' column (e.g., "Base Period", "Climate Change Onset").
# - period_name: A character string specifying the period for which the plots should be generated (e.g., "Base Period").
# - sheetstation: A character string specifying the station name to be included in the plot titles.
#
# Returns:
# - A list of ggplot objects for the generated plots.
#
# Example:
# create_plots_for_period(df, "Base Period", "Station A")
# This will generate and print multiple time series plots for various climate indices (spi3, spei3, Tmin, Tmax, etc.)
# for the "Base Period" and the station "Station A".
create_plots_for_period <- function(data, period_name, sheetstation) {
  # Filter data for the specified period
  filtered_data <- data %>%
    filter(Period == period_name)
  
  # Generate and print plots for various climate indices
  plot_spi3 <- generate_timeseries_plot(filtered_data, "Year", "spi3",
                                        paste(period_name, "Time Series Plot of spi3 for", sheetstation),
                                        "Year", "spi3")
  print(plot_spi3)
  
  plot_spi6 <- generate_timeseries_plot(filtered_data, "Year", "spi6",
                                        paste(period_name, "Time Series Plot of spi6", sheetstation),
                                        "Year", "spi6")
  print(plot_spi6)
  
  plot_spi12 <- generate_timeseries_plot(filtered_data, "Year", "spi12",
                                         paste(period_name, "Time Series Plot of spi12", sheetstation),
                                         "Year", "spi12")
  print(plot_spi12)
  
  plot_spei3 <- generate_timeseries_plot(filtered_data, "Year", "spei3",
                                         paste(period_name, "Time Series Plot of spei3", sheetstation),
                                         "Year", "spei3")
  print(plot_spei3)
  
  plot_spei6 <- generate_timeseries_plot(filtered_data, "Year", "spei6",
                                         paste(period_name, "Time Series Plot of spei6", sheetstation),
                                         "Year", "spei6")
  print(plot_spei6)
  
  plot_spei12 <- generate_timeseries_plot(filtered_data, "Year", "spei12",
                                          paste(period_name, "Time Series Plot of spei12", sheetstation),
                                          "Year", "spei12")
  print(plot_spei12)
  
  plot_tmin <- generate_timeseries_plot(filtered_data, "Year", "Tmin",
                                        paste(period_name, "Time Series Plot of Tmin", sheetstation),
                                        "Year", "Tmin")
  print(plot_tmin)
  
  plot_tmax <- generate_timeseries_plot(filtered_data, "Year", "Tmax",
                                        paste(period_name, "Time Series Plot of Tmax", sheetstation),
                                        "Year", "Tmax")
  print(plot_tmax)
  
  plot_prec <- generate_timeseries_plot(filtered_data, "Year", "Prec.",
                                        paste(period_name, "Time Series Plot of Precipitation", sheetstation),
                                        "Year", "Precipitation")
  print(plot_prec)
  
  # Generate plots comparing SPI and SPEI indices
  plot_spi6_vs_spei6 <- indices_line_plot(filtered_data, c("spi6", "spei6"), paste0(sheetstation, " ", period_name, " SPI6 and SPEI6 Indices Over Time"), "spi6, spei6")
  plot_spi3_vs_spei3 <- indices_line_plot(filtered_data, c("spi3", "spei3"), paste0(sheetstation, " ", period_name, " SPI3 and SPEI3 Indices Over Time"), "spi3, spei3")
  plot_spi12_vs_spei12 <- indices_line_plot(filtered_data, c("spi12", "spei12"), paste0(sheetstation, " ", period_name, " SPI12 and SPEI12 Indices Over Time"), "spi12, spei12")
  
  return(list(plot_tmin, plot_tmax, plot_prec, plot_spi3, plot_spi6, plot_spi12, plot_spei3, plot_spei6, plot_spei12, plot_spi6_vs_spei6, plot_spi3_vs_spei3, plot_spi12_vs_spei12))
}
