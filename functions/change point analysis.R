# Perform changepoint analysis on the specified variable
change_point_analysis <- function(df, variable) {
  # Perform changepoint analysis using the PELT method
  cpt_object <- cpt.meanvar(df[[variable]], method = "PELT")
  return(cpt_object)
}

# Extract changepoint information from the cpt object
extract_changepoint_info <- function(cpt_object) {
  # Extract relevant information from the changepoint object
  changepoints <- cpt_object@cpts
  method <- cpt_object@method
  penalty_value <- cpt_object@pen.value
  test_stat <- cpt_object@test.stat
  parameter_estimates <- cpt_object@param.est
  
  # Summarize the information in a data frame
  changepoint_summary <- data.frame(
    Changepoint_Locations = paste(changepoints, collapse = ", "),
    Method = method,
    Penalty_Value = penalty_value,
    Test_Statistic = test_stat,
    Parameter_Estimates = ifelse(length(parameter_estimates) > 0, paste(parameter_estimates, collapse = ", "), NA)
  )
  
  return(changepoint_summary)
}

# Parse the changepoint summary to create a clean data frame
parse_change_point_summary <- function(change_point_summary) {
  # Parse changepoint locations into a numeric vector
  changepoint_locations <- as.numeric(unlist(strsplit(change_point_summary$Changepoint_Locations, ",")))
  
  # Parse and clean parameter estimates
  parameter_estimates <- change_point_summary$Parameter_Estimates
  parameter_estimates_clean <- gsub('[c()"]', '', parameter_estimates)
  parameter_estimates_split <- strsplit(parameter_estimates_clean, ",")
  parameter_estimates_numeric <- as.numeric(unlist(parameter_estimates_split))
  
  # Separate parameter estimates (assuming alternating values)
  param_estimates1 <- parameter_estimates_numeric[seq(1, length(parameter_estimates_numeric), by = 2)]
  param_estimates2 <- parameter_estimates_numeric[seq(2, length(parameter_estimates_numeric), by = 2)]
  
  # Combine into a clean data frame
  parsed_data <- data.frame(
    Changepoint_Locations = changepoint_locations,
    Parameter_Estimates1 = param_estimates1,
    Parameter_Estimates2 = param_estimates2,
    Method = rep(change_point_summary$Method, length(changepoint_locations)),
    Penalty_Value = rep(change_point_summary$Penalty_Value, length(changepoint_locations)),
    Test_Statistic = rep(change_point_summary$Test_Statistic, length(changepoint_locations))
  )
  
  return(parsed_data)
}
