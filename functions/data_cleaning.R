# Function to clean NA and Inf values from a data frame
# 
# Purpose:
# This function is used to clean a data frame by replacing any missing (NA) or infinite (Inf) values with a specified value
# (default is 0). It is especially useful when preparing data for analysis or modeling, as NA and Inf values can interfere 
# with many functions and calculations.
#
# Parameters:
# - df: A data frame containing the data to be cleaned. The function will check each column and replace any NA or Inf values.
# - replace_with: The value to replace NA and Inf with. The default is 0, but you can specify any other value (e.g., mean or median).
#
# Returns:
# - A data frame with NA and Inf values replaced by the specified value.
#
# Example:
# clean_na_inf(df)  
# This will replace all NA and Inf values in the data frame 'df' with 0.
# clean_na_inf(df, replace_with = 999)  
# This will replace all NA and Inf values in the data frame 'df' with 999.
clean_na_inf <- function(df, replace_with = 0) {
  # Check if the column is numeric before replacing NA or Inf
  df[] <- lapply(df, function(col) {
    if (is.numeric(col)) {
      col[is.na(col) | is.infinite(col)] <- replace_with
    }
    return(col)
  })
  return(df)
}
