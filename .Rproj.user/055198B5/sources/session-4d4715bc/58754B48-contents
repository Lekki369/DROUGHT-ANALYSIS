# install.packages("pacman")
pacman::p_load(readxl, ggplot2, dplyr, tidyr, SPEI, drought, Kendall, 
               lubridate, tidyverse, MASS, broom, trend, openxlsx)

excelsheets <- excel_sheets("dataset/droughtdat.xlsx")
sheetstation <- excelsheets[1]
sample_station <- read_excel("dataset/droughtdat.xlsx", sheet = sheetstation)
total_years <- length(unique(sample_station$Year))

year_summary <- sample_station %>%
  group_by(Year) %>%
  summarise(count = n()) %>%
  arrange(Year)

# View the summary


lat <- sample_station$Lat[1]

# Calculate PET, SHI, and drought indices
sample_station <- sample_station %>%
  mutate(
    PET = hargreaves(Tmin = Tmin, Tmax = Tmax, lat = lat),
    SHI = Prec. / PET,
    cwbal = Prec. - PET,
    Tmean = (Tmin + Tmax) / 2,
    Aridity_Index = Prec. / PET
  )

# Calculate SPI and SPEI indices for different time scales
sample_station$spi3 <- spi(sample_station$Prec., 3)$fitted
sample_station$spi6 <- spi(sample_station$Prec., 6)$fitted
sample_station$spi12 <- spi(sample_station$Prec., 12)$fitted

sample_station$spei3 <- spei(sample_station$cwbal, 3)$fitted
sample_station$spei6 <- spei(sample_station$cwbal, 6)$fitted
sample_station$spei12 <- spei(sample_station$cwbal, 12)$fitted

# function to Categorize Drought Events
categorize_spi <- function(spi) {
  case_when(
    spi <= -2.0 ~ "Extreme Drought",
    spi > -2.0 & spi <= -1.5 ~ "Severe Drought",
    spi > -1.5 & spi <= -1.0 ~ "Moderate Drought",
    spi > -1.0 & spi <= -0.5 ~ "Mild Drought",
    TRUE ~ "No Drought"
  )
}
categorize_spei <- function(spei) {
  case_when(
    spei <= -2.0 ~ "Extreme Drought",
    spei > -2.0 & spei <= -1.5 ~ "Severe Drought",
    spei > -1.5 & spei <= -1.0 ~ "Moderate Drought",
    spei > -1.0 & spei <= -0.5 ~ "Mild Drought",
    TRUE ~ "No Drought"
  )
}

#Categorize Drought Events
sample_station <- sample_station %>%
  mutate(
    spi3_cat = categorize_spi(spi3),
    spi6_cat = categorize_spi(spi6),
    spi12_cat = categorize_spi(spi12)
  )
sample_station <- sample_station %>%
  mutate(
    spei3_cat = categorize_spei(spei3),
    spei6_cat = categorize_spei(spei6),
    spei12_cat = categorize_spei(spei12)
  )


#Summarize Drought Events by Year
# Count occurrences of drought categories for spi3 and spi6 for each year
drought_counts <- sample_station %>%
  group_by(Year) %>%
  summarise(
    spi3_mild = sum(spi3_cat == 'Mild Drought'),
    spi3_moderate = sum(spi3_cat == "Moderate Drought"),
    spi3_severe = sum(spi3_cat == "Severe Drought"),
    spi3_extreme = sum(spi3_cat == "Extreme Drought"),
    
    spei3_mild = sum(spei3_cat == "Mild Drought"),
    spei3_moderate = sum(spei3_cat == "Moderate Drought"),
    spei3_severe = sum(spei3_cat == "Severe Drought"),
    spei3_extreme = sum(spei3_cat == "Extreme Drought"),
    
    spi6_mild = sum(spi6_cat == "Mild Drought"),
    spi6_moderate = sum(spi6_cat == "Moderate Drought"),
    spi6_severe = sum(spi6_cat == "Severe Drought"),
    spi6_extreme = sum(spi6_cat == "Extreme Drought"),
    
    spei6_mild = sum(spei6_cat == "Mild Drought"),
    spei6_moderate = sum(spei6_cat == "Moderate Drought"),
    spei6_severe = sum(spei6_cat == "Severe Drought"),
    spei6_extreme = sum(spei6_cat == "Extreme Drought"),
    
    spi12_mild = sum(spi12_cat == 'Mild Drought'),
    spi12_moderate = sum(spi12_cat == "Moderate Drought"),
    spi12_severe = sum(spi12_cat == "Severe Drought"),
    spi12_extreme = sum(spi12_cat == "Extreme Drought"),
    
    spei12_mild = sum(spei12_cat == "Mild Drought"),
    spei12_moderate = sum(spei12_cat == "Moderate Drought"),
    spei12_severe = sum(spei12_cat == "Severe Drought"),
    spei12_extreme = sum(spei12_cat == "Extreme Drought")
  )

# Draw the histogram of the Drought Categories
drought_long_spi <- drought_counts %>%
  pivot_longer(cols = starts_with(c("spi3_", "spi6_", "spi12_")), names_to = "Category", values_to = "Count")

drought_long_spei <- drought_counts %>%
  pivot_longer(cols = starts_with(c("spei3_", "spei6_", "spei12_")), names_to = "Category", values_to = "Count")

plot1 <- ggplot(drought_long_spi, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = paste0(sheetstation," Drought Category SPI Distribution"), x = "Drought Category", y = "Total Count") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot1)

plot2 <- ggplot(drought_long_spei, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity") +
  labs(title = paste0(sheetstation," Drought Category SPEI Distribution"), x = "Drought Category", y = "Total Count") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
print(plot2)

# Plotting the line graph
#for spi
drought_long_spi <- drought_counts %>%
  pivot_longer(cols = starts_with(c("spi12_", "spi6_", "spi3_")), names_to = c("Index", "Category"), values_to = "Count", names_sep = "_")

plot3 <- ggplot(drought_long_spi, aes(x = Year, y = Count, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title =  paste0(sheetstation," Drought Categories Of SPI Over the Years"),
       x = "Year",
       y = "Count of Drought Events") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_viridis_d() +
  facet_wrap(~ Category + Index, ncol = 2, scales = "free_y")
print(plot3)

#for spei
drought_long_spei <- drought_counts %>%
  pivot_longer(cols = starts_with(c("spei12_", "spei6_", "spei3_")), names_to = c("Index", "Category"), values_to = "Count", names_sep = "_")

plot4 <- ggplot(drought_long_spei, aes(x = Year, y = Count, color = Category)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title =  paste0(sheetstation," Drought Categories Of SPEI Over the Years"),
       x = "Year",
       y = "Count of Drought Events") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_color_viridis_d() +
  facet_wrap(~ Category + Index, ncol = 2, scales = "free_y")
print(plot4)

#multiple plot of index values
plot5 <- ggplot(sample_station, aes(x = factor(Year))) +  # Use factor for Year to treat it as discrete values
  geom_col(aes(y = spi6, fill = "SPI6"), width = 0.1, position = position_dodge(width = 0.2)) +   # Side-by-side dodge
  geom_col(aes(y = spei6, fill = "SPEI6"), width = 0.1, position = position_dodge(width = 0.3)) + # Same dodge for SPEI3
  scale_x_discrete(breaks = seq(min(sample_station$Year), max(sample_station$Year), by = 2)) +   # Year intervals set to 4
  labs(title = paste0(sheetstation," SPI6 and SPEI6 Indices Over Time"),
       x = "Year", 
       y = "spi6, spei6") +
  scale_fill_manual(values = c("SPI6" = "blue", "SPEI6" = "red")) +   # Custom colors for SPI3 and SPEI3
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))  # Remove legend title and rotate x-axis labels
print(plot5)
plot6 <- ggplot(sample_station, aes(x = factor(Year))) +  # Use factor for Year to treat it as discrete values
  geom_col(aes(y = spi3, fill = "SPI3"), width = 0.1, position = position_dodge(width = 0.2)) +   # Side-by-side dodge
  geom_col(aes(y = spei3, fill = "SPEI3"), width = 0.1, position = position_dodge(width = 0.3)) + # Same dodge for SPEI3
  scale_x_discrete(breaks = seq(min(sample_station$Year), max(sample_station$Year), by = 2)) +   # Year intervals set to 4
  labs(title = paste0(sheetstation," SPI3 and SPEI3 Indices Over Time"),
       x = "Year", 
       y = "spi3, spei3") +
  scale_fill_manual(values = c("SPI3" = "blue", "SPEI3" = "red")) +   # Custom colors for SPI3 and SPEI3
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))  # Remove legend title and rotate x-axis labels
print(plot6)
plot7 <- ggplot(sample_station, aes(x = factor(Year))) +  # Use factor for Year to treat it as discrete values
  geom_col(aes(y = spi12, fill = "SPI12"), width = 0.1, position = position_dodge(width = 0.2)) +   # Side-by-side dodge
  geom_col(aes(y = spei12, fill = "SPEI12"), width = 0.1, position = position_dodge(width = 0.3)) + # Same dodge for SPEI3
  scale_x_discrete(breaks = seq(min(sample_station$Year), max(sample_station$Year), by = 2)) +   # Year intervals set to 4
  labs(title = paste0(sheetstation," SPI12 and SPEI12 Indices Over Time"),
       x = "Year", 
       y = "spi12, spei12") +
  scale_fill_manual(values = c("SPI12" = "blue", "SPEI12" = "red")) +   # Custom colors for SPI3 and SPEI3
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))  # Remove legend title and rotate x-axis labels
print(plot7)


#use boxplot
drought_long_boxplot <- drought_counts %>%
  pivot_longer(
    cols = c("spi3_mild", "spi3_moderate", "spi3_severe", "spi3_extreme",
             "spi6_mild", "spi6_moderate", "spi6_severe", "spi6_extreme"),
    names_to = "Category",
    values_to = "Count"
  )
plot8 <- ggplot(drought_long_boxplot, aes(x = Category, y = Count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Drought Categories",
       x = "Drought Category",
       y = "Count of Drought Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Tilt x-axis labels for clarity
print(plot8)

#create drought categories
create_drought_categories <- function(data, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                               labels = c("Extreme", "Severe", "Moderate", "Mild")) {
  cut(data, breaks = breaks, labels = labels)
}

# Create categories for all indices
indices <- list(
  spi3 = sample_station$spi3,
  spi6 = sample_station$spi6,
  spi12 = sample_station$spi12,
  spei3 = sample_station$spei3,
  spei6 = sample_station$spei6,
  spei12 = sample_station$spei12
)

drought_categories <- lapply(indices, create_drought_categories)

#here
sp_cat <- data.frame(do.call(rbind, lapply(drought_categories, table)))
print(sp_cat)

# Compute drought characteristics
compute_drought_characteristics <- function(data, index_name) {
  data %>%  
    group_by(Year) %>% 
    summarise(
      Severity = sum(!!sym(index_name), na.rm = TRUE),
      Duration = sum(!!sym(index_name) < 0, na.rm = TRUE),
      Frequency = round(sum(!!sym(index_name) < 0, na.rm = TRUE) * 100/12, 2),
      Intensity = round(Severity/Duration, 2)
    )
}
Chara_spi3 <- compute_drought_characteristics(sample_station, "spi3")
Chara_spi6 <- compute_drought_characteristics(sample_station, "spi6")
Chara_spi12 <- compute_drought_characteristics(sample_station, "spi12")
Chara_spei3 <- compute_drought_characteristics(sample_station, "spei3")
Chara_spei6 <- compute_drought_characteristics(sample_station, "spei6")
Chara_spei12 <- compute_drought_characteristics(sample_station, "spei12")

calculate_exposure <- function(data, drought_column, total_duration) {
  total_drought_duration <- sum(data[[drought_column]] < 0, na.rm = TRUE)
  Ex <- total_drought_duration / total_duration
  message("Total Drought Duration (months): ", total_drought_duration)
  message("Drought Exposure (Ex): ", round(Ex, 4))
  return(Ex)
}

# Calculate Drought Vulnerability (Vu)
calculate_vulnerability <- function(data, drought_column) {
  SPI_drought <- data[[drought_column]][data[[drought_column]] < 0 & !is.na(data[[drought_column]])]
  M <- length(SPI_drought)
  Vu <- if (M > 0) sum(abs(SPI_drought), na.rm = TRUE) / M else NA
  message("Drought Vulnerability (Vu): ", round(Vu, 4))
  return(Vu)
}

# Calculate Resilience (Re)
calculate_resilience <- function(data, drought_column) {
  drought_durations <- data[[drought_column]][data[[drought_column]] < 0 & !is.na(data[[drought_column]])]
  M <- length(drought_durations)
  sum_DDi <- sum(drought_durations, na.rm = TRUE)
  Re <- if (M > 0 & sum_DDi != 0) M / sum_DDi else NA
  message("Resilience (Re): ", round(Re, 4))
  return(Re)
}
total_duration <-  as.numeric(sum(year_summary$count))
Chara_spei6 <- Chara_spei6 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spei6", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spei6"),
    DroughtResilience = calculate_resilience(sample_station, "spei6")
  )
Chara_spei3 <- Chara_spei3 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spei3", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spei3"),
    DroughtResilience = calculate_resilience(sample_station, "spei3")
  )
Chara_spei12 <- Chara_spei12 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spei12", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spei12"),
    DroughtResilience = calculate_resilience(sample_station, "spei12")
  )

Chara_spi3 <- Chara_spi3 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spi3", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spi3"),
    DroughtResilience = calculate_resilience(sample_station, "spi3")
  )
Chara_spi6 <- Chara_spi6 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spi6", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spi6"),
    DroughtResilience = calculate_resilience(sample_station, "spi6")
  )
Chara_spi12 <- Chara_spi12 %>%
  mutate(
    DroughtExposure = calculate_exposure(sample_station, "spi12", total_duration),
    DroughtVulnerability = calculate_vulnerability(sample_station, "spi12"),
    DroughtResilience = calculate_resilience(sample_station, "spi12")
  )


#linear model equation for annotation
lm_eqn <- function(df, x_var, y_var) {
  model <- lm(as.formula(paste(y_var, "~", x_var)), data = df)
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2, 
                   list(a = format(coef(model)[1], digits = 2), 
                        b = format(coef(model)[2], digits = 2), 
                        r2 = format(summary(model)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}

# Function to Plot SPI vs SPEI
plot_drought_index_relationship <- function(df, x_var, y_var, title) {
  # Remove rows with NA/NaN/Inf in the specified columns
  df_clean <- df %>%
    filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]), 
           !is.infinite(.data[[x_var]]), !is.infinite(.data[[y_var]]), 
           !is.nan(.data[[x_var]]), !is.nan(.data[[y_var]]))
  
  ggplot(df_clean, aes_string(x = x_var, y = y_var)) +
    geom_point(color = "blue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = title, x = x_var, y = y_var) +
    annotate("text", x = min(df_clean[[x_var]], na.rm = TRUE), y = max(df_clean[[y_var]], na.rm = TRUE), 
             label = lm_eqn(df_clean, x_var, y_var), parse = TRUE, hjust = 0)
}

# Generate plots for SPI3 vs SPEI3 and SPI6 vs SPEI6
plot9 <- plot_drought_index_relationship(sample_station, "spi3", "spei3", "Linear Relationship between SPI3 and SPEI3")
plot10 <- plot_drought_index_relationship(sample_station, "spi6", "spei6", "Linear Relationship between SPI6 and SPEI6")
plot11 <- plot_drought_index_relationship(sample_station, "spi12", "spei12", "Linear Relationship between SPI12 and SPEI12")
# Display the plots
print(plot9)
print(plot10)
print(plot11)

# Calculate Mann-Kendall trend test and Sen’s Slope for multiple variables
trend_result <- sample_station %>%
  summarise(
    Tmin_Mann_Tau = MannKendall(Tmin)$tau,
    Tmin_Mann_p = MannKendall(Tmin)$sl,
    Tmin_Sen_slope = sens.slope(Tmin)$estimate,
    Tmax_Mann_Tau = MannKendall(Tmax)$tau,
    Tmax_Mann_p = MannKendall(Tmax)$sl,
    Tmax_Sen_slope = sens.slope(Tmax)$estimate,
    Pre_Mann_Tau = MannKendall(Prec.)$tau,
    Pre_Mann_p = MannKendall(Prec.)$sl,
    Pre_Sen_slope = sens.slope(Prec.)$estimate
  )

tidy_results <- trend_result %>%
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"), names_sep = "_", values_to = "Value")
print(tidy_results)

# Trend plot of Tmin, Tmax, and Precipitation over time
plot12 <- ggplot(sample_station, aes(x = Year)) +
  geom_point(aes(y = Tmin), color = "blue", size = 2) +
  geom_smooth(aes(y = Tmin), method = "loess", se = FALSE, color = "blue") +
  geom_point(aes(y = Tmax), color = "red", size = 2) +
  geom_smooth(aes(y = Tmax), method = "loess", se = FALSE, color = "red") +
  geom_point(aes(y = Prec.), color = "green", size = 2) +
  geom_smooth(aes(y = Prec.), method = "loess", se = FALSE, color = "green") +
  labs(title = "Trends of Tmin, Tmax, and Precipitation Over Time",
       x = "Year", y = "Values",
       caption = "Blue: Tmin, Red: Tmax, Green: Precipitation") +
  theme_minimal()
print(plot12)

# Calculate drought category distributions
drought_total_counts <- sample_station %>%
  summarise(
    spi3_mild_total = sum(spi3_cat == 'Mild Drought', na.rm = TRUE),
    spi3_moderate_total = sum(spi3_cat == "Moderate Drought", na.rm = TRUE),
    spi3_severe_total = sum(spi3_cat == "Severe Drought", na.rm = TRUE),
    spi3_extreme_total = sum(spi3_cat == "Extreme Drought", na.rm = TRUE),
    
    spi6_mild_total = sum(spi6_cat == 'Mild Drought', na.rm = TRUE),
    spi6_moderate_total = sum(spi6_cat == "Moderate Drought", na.rm = TRUE),
    spi6_severe_total = sum(spi6_cat == "Severe Drought", na.rm = TRUE),
    spi6_extreme_total = sum(spi6_cat == "Extreme Drought", na.rm = TRUE),
    
    spi12_mild_total = sum(spi12_cat == 'Mild Drought', na.rm = TRUE),
    spi12_moderate_total = sum(spi12_cat == "Moderate Drought", na.rm = TRUE),
    spi12_severe_total = sum(spi12_cat == "Severe Drought", na.rm = TRUE),
    spi12_extreme_total = sum(spi12_cat == "Extreme Drought", na.rm = TRUE),
    
    spei3_mild_total = sum(spei3_cat == 'Mild Drought', na.rm = TRUE),
    spei3_moderate_total = sum(spei3_cat == "Moderate Drought", na.rm = TRUE),
    spei3_severe_total = sum(spei3_cat == "Severe Drought", na.rm = TRUE),
    spei3_extreme_total = sum(spei3_cat == "Extreme Drought", na.rm = TRUE),
    
    spei6_mild_total = sum(spei6_cat == 'Mild Drought', na.rm = TRUE),
    spei6_moderate_total = sum(spei6_cat == "Moderate Drought", na.rm = TRUE),
    spei6_severe_total = sum(spei6_cat == "Severe Drought", na.rm = TRUE),
    spei6_extreme_total = sum(spei6_cat == "Extreme Drought", na.rm = TRUE),
    
    spei12_mild_total = sum(spei12_cat == 'Mild Drought', na.rm = TRUE),
    spei12_moderate_total = sum(spei12_cat == "Moderate Drought", na.rm = TRUE),
    spei12_severe_total = sum(spei12_cat == "Severe Drought", na.rm = TRUE),
    spei12_extreme_total = sum(spei12_cat == "Extreme Drought", na.rm = TRUE)
  )

# Create the directory if it doesn't exist
dir.create(paste0("results/stations/", sheetstation), recursive = TRUE)

# List of plot objects
plot_list <- list(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10, plot11, plot12)

# Save each plot
for (i in seq_along(plot_list)) {
  ggsave(
    filename = paste0("results/stations/", sheetstation, "/plot", i, ".png"),
    plot = plot_list[[i]],  # Specify the plot to save
    width = 8, height = 6, dpi = 300  # Optional: Customize width, height, and resolution
  )
}

# Save data to Excel with multiple sheets
wb <- createWorkbook()
addWorksheet(wb, paste0(sheetstation, " Data"))
addWorksheet(wb, "Drought Cat")
addWorksheet(wb, "Chara_spi3")
addWorksheet(wb, "Chara_spi6")
addWorksheet(wb, "Chara_spi12")
addWorksheet(wb, "Chara_spei3")
addWorksheet(wb, "Chara_spei6")
addWorksheet(wb, "Chara_spei12")
addWorksheet(wb, "Trends")
addWorksheet(wb, "Drought_Total_Count")

# Write data to each sheet
writeData(wb, sheet = paste0(sheetstation, " Data"), sample_station)
writeData(wb, sheet = "Drought Cat", sp_cat, rowNames = TRUE)
writeData(wb, sheet = "Chara_spi3", Chara_spi3)
writeData(wb, sheet = "Chara_spi6", Chara_spi6)
writeData(wb, sheet = "Chara_spi12", Chara_spi12)
writeData(wb, sheet = "Chara_spei3", Chara_spei3)
writeData(wb, sheet = "Chara_spei6", Chara_spei6)
writeData(wb, sheet = "Chara_spei12", Chara_spei12)
writeData(wb, sheet = "Trends", trend_result)
writeData(wb, sheet = "Drought_Total_Count", drought_total_counts)

# Save the workbook
saveWorkbook(wb, file = paste0("results/stations/",sheetstation,"/", sheetstation, ".xlsx"), overwrite = TRUE)
