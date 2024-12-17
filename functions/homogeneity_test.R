# Function to perform a homogeneity test (ADF Test for Precipitation)
# This function performs the Augmented Dickey-Fuller (ADF) test on a specified column in the dataset to check for stationarity (homogeneity). It returns the test results and plots the time series.
# Arguments:
#   data: The dataset containing the time series data.
#   column: The name of the column to be tested for stationarity.
#   time_col: The name of the column representing time.
homogeneity_test <- function(data, column, time_col) {
  # Perform ADF test
  adf_result <- adf.test(data[[column]], alternative = "stationary")
  
  # Extract and return results as a data frame
  results <- data.frame(
    Test_Statistic = adf_result$statistic,
    P_Value = adf_result$p.value,
    Method = adf_result$method,
    Alternative_Hypothesis = adf_result$alternative,
    Critical_Values_1pct = ifelse(length(adf_result$cval) >= 1, adf_result$cval[1], NA),
    Critical_Values_5pct = ifelse(length(adf_result$cval) >= 2, adf_result$cval[2], NA),
    Critical_Values_10pct = ifelse(length(adf_result$cval) >= 3, adf_result$cval[3], NA)
  )
  
  # Plot the time series
  plot(data[[time_col]], data[[column]], type = "l",
       main = paste("Time Series Plot of", column),
       xlab = time_col, ylab = column)
  
  return(results)
}
