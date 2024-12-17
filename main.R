# Load necessary libraries
pacman::p_load(readxl, ggplot2, dplyr, tidyr, SPEI, drought, Kendall, 
               lubridate, tidyverse, MASS, broom, trend, openxlsx, tseries, changepoint)

#Source All Functions
function_files <- list.files("functions", pattern = "\\.R$", full.names = TRUE)
for (file in function_files) {
  source(file)
}
plot_function_files <- list.files("functions/plots_functions", pattern = "\\.R$", full.names = TRUE)
for (file in plot_function_files) {
  source(file)
}
message("All functions and libraries initialized successfully!")

# Load Excel sheets
excelsheets <- excel_sheets("dataset/droughtdat.xlsx")
for(sheet in excelsheets){
  message(sheet)
  sheetstation <- sheet
  sample_station <- read_excel("dataset/droughtdat.xlsx", sheet = sheet)
  
  # Extract latitude
  lat <- sample_station$Lat[1]
  
  sample_station <-  calculate_indices(sample_station, lat_col = lat)
  
  sample_station <- clean_na_inf (sample_station, replace_with = 0)
  
  # Calculate total years
  total_years_count <-  as.numeric(length(sample_station$Year))  
  
  # Yearly summary
  year_summary <- yearly_summary_function(sample_station)
  
  
  # Categorize drought events# Categorize drought events# Categorize drought events
  sample_station <- sample_station %>%
    mutate(
      spi3_cat = categorize_spi(spi3),
      spi6_cat = categorize_spi(spi6),
      spi12_cat = categorize_spi(spi12),
      spei3_cat = categorize_spei(spei3),
      spei6_cat = categorize_spei(spei6),
      spei12_cat = categorize_spei(spei12)
    )
  
  # Summarize drought events by year
  drought_counts <- yearly_drought_categories_occurrences_calculation(sample_station)
  
  # Pivot SPI data
  drought_long_spi <- pivot_drought_data(drought_counts, prefix = "spi")
  
  # Pivot SPEI data
  drought_long_spei <- pivot_drought_data(drought_counts, prefix = "spei")
  
  
  # SPI distribution plot
  plot1 <- plot_drought_distribution(
    data = drought_long_spi,
    category = "Category",
    count_col = "Count",
    title_prefix = sheetstation
  )
  
  # SPEI distribution plot
  plot2 <- plot_drought_distribution(
    data = drought_long_spei,
    category = "Category",
    count_col = "Count",
    title_prefix = sheetstation
  )
  
  # Bar plot of drought categories over the years
  drought_long_spi <- pivot_drought_data_with_index(
    data = drought_counts,
    prefix = "spi"
  )
  
  plot3 <- plot_drought_categories(
    data = drought_long_spi,
    index_col = "Index",
    category = "Category",
    count_col = "Count",
    title_prefix = sheetstation
  )
  
  drought_long_spei <- pivot_drought_data_with_index(
    data = drought_counts,
    prefix = "spei"
  )
  
  plot4 <- plot_drought_categories(
    data = drought_long_spei,
    index_col = "Index",
    category = "Category",
    count_col = "Count",
    title_prefix = sheetstation
  )
  
  
  
  plot5 <- indices_bar_plot(sample_station, c("spi6", "spei6"), paste0(sheetstation, " SPI6 and SPEI6 Indices Over Time"), "spi6, spei6")
  plot6 <- indices_bar_plot(sample_station, c("spi3", "spei3"), paste0(sheetstation, " SPI3 and SPEI3 Indices Over Time"), "spi3, spei3")
  plot7 <- indices_bar_plot(sample_station, c("spi12", "spei12"), paste0(sheetstation, " SPI12 and SPEI12 Indices Over Time"), "spi12, spei12")
  
  # Boxplot of drought categories for spi
  drought_long_boxplot <-  pivot_drought_boxplot_data(
    data = drought_counts,
    prefix = "spi"
  )
  
  plot8 <- plot_drought_boxplot(
    data = drought_long_boxplot,
    category = "Category",
    count_col = "Count",
    title = "Boxplot of Drought Categories"
  )
  
  # Categorize indices
  indices <- list(spi3 = sample_station$spi3, 
                  spi6 = sample_station$spi6,
                  spi12 = sample_station$spi12,
                  spei3 = sample_station$spei3, 
                  spei6 = sample_station$spei6, 
                  spei12 = sample_station$spei12)
  
  drought_categories <-  lapply(indices, create_drought_categories)
  index_names <- c("SPI-3", "SPI-6", "SPI-12", "SPEI-3", "SPEI-6", "SPEI-12")
  sp_cat <- data.frame(do.call(rbind, lapply(drought_categories, table)))
  
  sp_cat <- cbind(Index = index_names, sp_cat)
  
  # Compute drought characteristics
  Chara_spi3 <- clean_na_inf(compute_drought_characteristics(sample_station, "spi3"))
  Chara_spi6 <- clean_na_inf(compute_drought_characteristics(sample_station, "spi6"))
  Chara_spi12 <- clean_na_inf(compute_drought_characteristics(sample_station, "spi12"))
  Chara_spei3 <- clean_na_inf(compute_drought_characteristics(sample_station, "spei3"))
  Chara_spei6 <- clean_na_inf(compute_drought_characteristics(sample_station, "spei6"))
  Chara_spei12 <- clean_na_inf(compute_drought_characteristics(sample_station, "spei12"))
  
  # Add Exposure, Vulnerability, and Resilience to characteristics
  # Add characteristics for SPEI and SPI indices

  
  Chara_spei6 <- add_drought_characteristics(Chara_spei6, sample_station, "spei6", total_years_count)
  Chara_spei3 <- add_drought_characteristics(Chara_spei3, sample_station, "spei3", total_years_count)
  Chara_spei12 <- add_drought_characteristics(Chara_spei12, sample_station, "spei12", total_years_count)
  Chara_spi3 <- add_drought_characteristics(Chara_spi3, sample_station, "spi3", total_years_count)
  Chara_spi6 <- add_drought_characteristics(Chara_spi6, sample_station, "spi6", total_years_count)
  Chara_spi12 <- add_drought_characteristics(Chara_spi12, sample_station, "spi12", total_years_count)
  
  
  
  # Plot SPI vs SPEI
  plot9 <- plot_drought_index_relationship(sample_station, "spi3", "spei3", "Linear Relationship between SPI3 and SPEI3")
  plot10 <- plot_drought_index_relationship(sample_station, "spi6", "spei6", "Linear Relationship between SPI6 and SPEI6")
  plot11 <- plot_drought_index_relationship(sample_station, "spi12", "spei12", "Linear Relationship between SPI12 and SPEI12")
  
  # Mann-Kendall trend test and Sen’s Slope
  variables <- c("Tmin", "Tmax", "Prec.")
  trend_result <-  perform_trend_analysis(sample_station, variables)
  
  
  
  # Trend plot of Tmin, Tmax, and Precipitation over time
  plot12 <- plot_trend_analysis(sample_station, "Year", "Tmin", "blue", "Trend of Tmin Over Time", "Blue: Tmin")
  plot13 <- plot_trend_analysis(sample_station, "Year", "Tmax", "blue", "Trend of Tmax Over Time", "Red: Tmax")
  plot14 <- plot_trend_analysis(sample_station, "Year", "Prec.", "green", "Trend of Precipitation Over Time", "Green: Precipitation")
  
  
  # Calculate total counts of drought categories
  #drought_columns <- grep("_cat$", colnames(sample_station), value = TRUE) 
  drought_total_counts <-  total_drought_categories_occurrences_calculation(sample_station)
  
  # Homogeneity Test (Normality Test for Precipitation)
  homogeneity_test_results <- homogeneity_test(
    data = year_summary, 
    column = "Prec.", 
    time_col = "Year"
  )
  
  # Change Point Analysis
  change_point_prec <- change_point_analysis(year_summary, "Prec.")
  change_point_tmin <- change_point_analysis(year_summary, "Tmin")
  change_point_tmax <- change_point_analysis(year_summary, "Tmax")
  # Extract changepoint summaries
  change_point_prec_summary <- extract_changepoint_info(change_point_prec)
  change_point_tmin_summary <- extract_changepoint_info(change_point_tmin)
  change_point_tmax_summary <- extract_changepoint_info(change_point_tmax)
  # Parse changepoint summaries
  clean_change_point_prec <- parse_change_point_summary(change_point_prec_summary)
  clean_change_point_tmin <- parse_change_point_summary(change_point_tmin_summary)
  clean_change_point_tmax <- parse_change_point_summary(change_point_tmax_summary)
  
  # Climatological Year Analysis
  # Define periods
  base_period <- year_summary$Year[year_summary$Year >= 1960 & year_summary$Year <= 1989]
  change_period1 <- year_summary$Year[year_summary$Year >= 1990 & year_summary$Year <= 2004]
  change_period2 <- year_summary$Year[year_summary$Year >= 2005 & year_summary$Year <= 2019]
  
  climate_analysis_results <- climatological_analysis(year_summary, base_period, change_period1, change_period2)
  
  climate_data <- climate_analysis_results$Data
  climate_trends <- climate_analysis_results$Trends
  
  # Generate plots for each defined period
  plot15 <- create_plots_for_period(climate_data, "Base Period", sheetstation)
  plot16 <- create_plots_for_period(climate_data, "Climate Change Onset", sheetstation)
  plot17 <- create_plots_for_period(climate_data, "Accelerated Climate Change", sheetstation)
  
  # Define the directory for saving plots
  output_directory <- paste0("results/stations/", sheetstation)
  
# List of plot objects
  plot_group_1 <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
                    plot11, plot12, plot13, plot14)
  
  plot_group_2 <- append(plot_group_1, plot15)

  plot_group_3 <- append(plot_group_2, plot16)

  plot_group_4 <- append(plot_group_3, plot17)


  # Save the plots
  save_plots(plot_group_4, output_directory)
  
  # Save data to Excel with multiple sheets
  # Define the file path
  output_file <- paste0("results/stations/", sheetstation, "/", sheetstation, ".xlsx")
  
  # Define the dynamic sheet name for the first item
  sheetstation_data_name <- paste0(sheetstation, " Data")
  
  # Create a list of sheet names and their corresponding data frames
  sheets_and_data <- list(
    sheetstation_data_name = sample_station,  # Dynamic name variable
    "Drought Cat" = sp_cat,
    "Chara_spi3" = Chara_spi3,
    "Chara_spi6" = Chara_spi6,
    "Chara_spi12" = Chara_spi12,
    "Chara_spei3" = Chara_spei3,
    "Chara_spei6" = Chara_spei6,
    "Chara_spei12" = Chara_spei12,
    "Trends" = trend_result,
    "Drought_Total_Count" = drought_total_counts,
    "Homogeneity Test" = homogeneity_test_results,
    "Change Points Prec" = clean_change_point_prec,
    "Change Points Tmin" = clean_change_point_tmin,
    "Change Points Tmax" = clean_change_point_tmax,
    "Climatological Analysis" = climate_analysis_results$Trends
  )
  
  # Save data to Excel
  save_to_excel(output_file, sheets_and_data)  
}




