part 3
#multiple plot of index values
ggplot(kad_dr1, aes(x = factor(Year))) +  # Use factor for Year to treat it as discrete values
  geom_col(aes(y = spi6, fill = "SPI6"), width = 0.1, position = position_dodge(width = 0.2)) +   # Side-by-side dodge
  geom_col(aes(y = spei6, fill = "SPEI6"), width = 0.1, position = position_dodge(width = 0.3)) + # Same dodge for SPEI3
  scale_x_discrete(breaks = seq(min(kad_dr1$Year), max(kad_dr1$Year), by = 2)) +   # Year intervals set to 4
  labs(title = "SPI6 and SPEI6 Indices Over Time",
       x = "Year", 
       y = "spi36, spei6") +
  scale_fill_manual(values = c("SPI6" = "blue", "SPEI6" = "red")) +   # Custom colors for SPI3 and SPEI3
  theme_minimal() +
  theme(legend.title = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1))  # Remove legend title and rotate x-axis labels

## use boxplot
# Step 1: Pivot data into long format
drought_long <- drought_counts %>%
  pivot_longer(
    cols = c("spi3_mild", "spi3_moderate", "spi3_severe", "spi3_extreme",
             "spi6_mild", "spi6_moderate", "spi6_severe", "spi6_extreme"),
    names_to = "Category",
    values_to = "Count"
  )

# Step 2: Create boxplot using ggplot2
ggplot(drought_long, aes(x = Category, y = Count)) +
  geom_boxplot() +
  labs(title = "Boxplot of Drought Categories",
       x = "Drought Category",
       y = "Count of Drought Events") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Tilt x-axis labels for clarity




#### Compute drought categories
spi3_cat <- cut(spi3$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                labels = c("Extreme", "Severe", "Moderate", "Mild"))
tab_spi3_cat <- table(spi3_cat)

spi6_cat <- cut(spi6$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                labels = c("Extreme", "Severe", "Moderate", "Mild"))

spi12_cat <- cut(spi12$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

#spi24_cat <- cut(spi24$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
# labels = c("Extreme", "Severe", "Moderate", "Mild"))

spei3_cat <- cut(spei3$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

spei6_cat <- cut(spei6$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                 labels = c("Extreme", "Severe", "Moderate", "Mild"))

spei12_cat <- cut(spei12$fitted, breaks = c(-Inf,-2, -1.5, -1, -0.5),
                  labels = c("Extreme", "Severe", "Moderate", "Mild"))
#drought_cat <- cut(spi24$fitted, breaks =  c(-Inf,-2,-1.5, -0.5, 0.5, 1.5, 2),
#labels = c("Extremelydry", "verydry", "dry", "wet", "verywet", "Extremelywet"))

#drought_cat <- cut(spi12$fitted, breaks = c(-Inf,-2, -1.5,-0.5, 0.5, 1.5, 2), 
#                 labels =  c("Extremely_dry","very_dry","dry","wet", "very_wet", "Extremely_wet"))

#Tabularise all the categories according to drought indices

sp_catkad<- data.frame(rbind(spic3=table(spi3_cat),
                             speic3=table(spei3_cat),
                             spic6=table(spi6_cat),
                             speic6=table(spei6_cat),
                             spic12=table(spi12_cat),
                             speic12=table(spei12_cat)))       
View(sp_catkad) 

###b Computations of Characteristics

Chara_spi3<- kad_dr1 %>%  
  group_by(Year) %>% 
  summarise(Severity = sum(spi3,na.rm = T,2),
            Duration = sum(spi3<0,na.rm=T),
            frequency = round(sum(spi3<0,na.rm = T)*100/12,2),
            Intensity = round(sum(spi3,na.rm = T)/sum(spi3<0,na.rm=T),2))
View(Chara_spi3)    
Chara_spei3<- kad_dr1 %>%  
  group_by(Year) %>% 
  summarise(Severity = sum(spei3,na.rm = T,2),
            Duration = sum(spei3<0,na.rm=T),
            frequency = round(sum(spei3<0,na.rm = T)*100/12,2),
            Intensity = round(sum(spei3,na.rm = T)/sum(spi3<0,na.rm=T),2))
View(Chara_spei3)
Chara_spei6<- kad_dr1 %>%  
  group_by(Year) %>% 
  summarise(Severity = sum(spei3,na.rm = T,2),
            Duration = sum(spei3<0,na.rm=T),
            frequency = round(sum(spei3<0,na.rm = T)*100/12,2),
            Intensity = round(sum(spei3,na.rm = T)/sum(spi3<0,na.rm=T),2))
View(Chara_spei6)
# calcilations of Drought Characteristic , Vulnerability, Resilience and Drought Exposure
# Assuming you have a data frame 'kad_dr1' with 'spei3' and 'spi3' columns

# Step 1: Summarize drought characteristics per year (existing logic)
Chara_spei6 <- kad_dr1 %>%  
  group_by(Year) %>% 
  summarise(
    Severity = sum(spei6, na.rm = TRUE),
    Duration = sum(spei6 < 0, na.rm = TRUE),  # count months where spei3 < 0
    frequency = round(sum(spei6 < 0, na.rm = TRUE) * 100 / 12, 2),  # monthly drought frequency as a percentage
    Intensity = round(sum(spei6, na.rm = TRUE) / sum(spei6 < 0, na.rm = TRUE), 2)
  )

# Step 2: Calculate total drought duration over the entire research period (e.g., 10 years = 120 months)
TD <- 60 * 12  # Total research period in months (e.g., 10 years = 120 months)

# Total drought duration: sum of months where spei3 < 0
total_drought_duration <- sum(kad_dr1$spei6 < 0, na.rm = TRUE)

# Step 3: Calculate drought exposure (Ex) for the entire research period
Ex <- total_drought_duration / TD

# Print total drought duration and drought exposure
print(paste("Total Drought Duration (months):", total_drought_duration))
print(paste("Drought Exposure (Ex):", round(Ex, 4)))

# Step 4: Calculate Drought Vulnerability (Vu) using the SPI values
# Filter SPI values where spei3 indicates drought (spei3 < 0), and remove NAs
SPI_drought <- kad_dr1$spei6[kad_dr1$spei6 < 0 & !is.na(kad_dr1$spi3)]

# Calculate the number of drought events (ignoring NAs)
M <- length(SPI_drought)

# Compute absolute values of SPI during drought events (ignoring NAs)
abs_SPI <- abs(SPI_drought)

# Compute Drought Vulnerability (Vu), handling NAs
if (M > 0) {
  Vu <- sum(abs_SPI, na.rm = TRUE) / M
} else {
  Vu <- NA  # Set as NA if no valid drought events are found
}

# Print the drought vulnerability value
print(paste("Drought Vulnerability (Vu):", round(Vu, 4)))


# Step 5: Calculate Resilience (Re)
# Filter the drought durations where spei6 < 0, and remove NAs
drought_durations <- kad_dr1$spei6[kad_dr1$spei6 < 0 & !is.na(kad_dr1$spei6)]

# Debugging: Check the filtered drought durations
print("Drought durations (spei6 < 0):")
print(drought_durations)

# Calculate the number of drought events (ignoring NAs)
M <- length(drought_durations)  # Total number of droughts
print(paste("Number of drought events (M):", M))

# Calculate the sum of drought durations (ignoring NAs)
sum_DDi <- sum(drought_durations, na.rm = TRUE)
print(paste("Sum of drought durations (sum_DDi):", sum_DDi))

# Compute resilience (Re), handling cases where sum_DDi is zero or M is zero
if (M > 0 & sum_DDi != 0) {
  Re <- M / sum_DDi
} else {
  Re <- NA  # Set as NA if no valid drought durations are found
}

# Print the resilience value
print(paste("Resilience (Re) of the basin:", Re))


# Print the resilience value
print(paste("Resilience (Re) of the basin:", Re))


# Step 6: Optional: Add drought exposure, vulnerability, and resilience to the summarized data frame
Chara_spei6$DroughtExposure <- Ex
Chara_spei6$DroughtVulnerability <- Vu
Chara_spei6$DroughtResilience <- Re

# View the summarized drought characteristics along with exposure, vulnerability, and resilience
view(Chara_spei6)
write.csv(Chara_spei6,"CharaKad_spei6")

kad_dr1_Range<-kad_dr1 %>% 
  filter(Year>=1960 &Year<=1970)

ggplot(kad_dr1_Range,aes(spi6, spei6)) +
  geom_point(col="green") +
  geom_abline(col="red")

qplot(Year,Prec.,kad_dr1_Range)
#geom_histogram(binwidth = .5)+
# facet_wrap(~Year,ncol = 6)
# geom_freqpoly(colour="red")
#geom_smooth(method = "rlm") 



View(kad_dr1)

kad_dr1<-kad_dr1 %>% 
  mutate(Month1=strftime(dmy(paste(as.character("Month"))),"%b"))

trend_result<-  kadat %>%  
  summarise(
    Tmin_Mann_Tau= MannKendall(Tmin)$tau,
    Tmin_Mann_p= MannKendall(Tmin)$sl,
    Tmin_Sen_slope= sens.slope(Tmin)$estimate,
    
    Tmax_Mann_Tau= MannKendall(Tmax)$tau,
    Tmax_Mann_p= MannKendall(Tmax)$sl,
    Tmax_Sen_slope= sens.slope(Tmax)$estimate,
    
    Pre_Mann_Tau= MannKendall(Prec.)$tau,
    Pre_Mann_p= MannKendall(Prec.)$sl,
    Pre_Sen_slope= sens.slope(Prec.)$estimate,
  )

# Convert to tidy table format
tidy_results <- trend_result %>% 
  pivot_longer(cols = everything(),     # Pivot all columns
               names_to = c("Variable", "Statistic"),  # Split names into 'Variable' and 'Statistic'
               names_sep = "_",         # Separate based on underscore (_)
               values_to = "Value")     # Name for the values column

# View the tidy table
print(tidy_results)

# Assuming kaddat is the dataframe that contains Year, Tmin, Tmax, and Prec.

ggplot(kad, aes(x = Year)) +
  # Plot for Tmin
  geom_point(aes(y = Tmin), color = "blue", size = 2) +  # Blue points for Tmin
  geom_smooth(aes(y = Tmin), method = "loess", se = FALSE, color = "blue") +  # Blue trend line for Tmin
  
  # Plot for Tmax
  geom_point(aes(y = Tmax), color = "red", size = 2) +   # Red points for Tmax
  geom_smooth(aes(y = Tmax), method = "loess", se = FALSE, color = "red") +   # Red trend line for Tmax
  
  # Plot for Precipitation
  geom_point(aes(y = Prec), color = "green", size = 2) +  # Green points for Precipitation
  geom_smooth(aes(y = Prec), method = "loess", se = FALSE, color = "green") +  # Green trend line for Precipitation
  
  # Labels and title
  labs(title = "Trends of Tmin, Tmax, and Precipitation Over Time",
       x = "Year",
       y = "Values",
       caption = "Blue: Tmin, Red: Tmax, Green: Precipitation") +
  
  # Facet to separate the plots
  facet_wrap(~variable, scales = "free_y") +
  
  # Theme for better visibility
  theme_minimal()

# Load necessary packages

# Create a function to add linear equation and R-squared
lm_eqn <- function(df, x_var, y_var) {
  model <- lm(as.formula(paste(y_var, "~", x_var)), data = df)
  eq <- substitute(italic(y) == a + b %.% italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2, 
                   list(a = format(coef(model)[1], digits = 2), 
                        b = format(coef(model)[2], digits = 2), 
                        r2 = format(summary(model)$r.squared, digits = 3)))
  as.character(as.expression(eq))
}

# Plot for spi3 vs spei3
p1 <- ggplot(kad_dr1, aes(x = spi3, y = spei3)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Relationship between SPI3 and SPEI3", x = "SPI3", y = "SPEI3") +
  annotate("text", x = min(kad_dr1$spi3,rm.na=TRUE), y = max(kad_dr1$spei3,rm.na= TRUE), 
           label = lm_eqn(kad_dr1, "spi3", "spei3"), parse = TRUE, hjust = 0)

# Plot for spi6 vs spei6
p2 <- ggplot(Kadat, aes(x = spi6, y = spei6)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Linear Relationship between SPI6 and SPEI6", x = "SPI6", y = "SPEI6") +
  annotate("text", x = min(Kadat$spi6), y = max(Kadat$spei6), 
           label = lm_eqn(Kadat, "spi6", "spei6"), parse = TRUE, hjust = 0)