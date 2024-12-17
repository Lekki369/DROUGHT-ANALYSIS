# Function to reshape drought data
# This function reshapes a dataset to make it easier to analyze. It takes data where the years are spread across multiple columns and turns it into a long format where each row represents a single year's data for a specific category.
# Arguments:
#   data: The dataset you want to reshape.
#   prefix: The prefix of the column names that contain the data you want to reshape.
#   year_col: The name of the column that contains the year. Default is "Year".
#   category: The name of the new column that will contain the categories (from the column names that start with the prefix). Default is "Category".
#   count_col: The name of the new column that will contain the values from the columns that start with the prefix. Default is "Count".
pivot_drought_data <- function(data, prefix, year_col = "Year", category = "Category", count_col = "Count") {
  pivoted_data <- data %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = category,
      values_to = count_col
    ) %>%
    transmute(
      !!sym(year_col), 
      !!sym(category), 
      !!sym(count_col)
    )
  
  return(pivoted_data)
}

# Function to reshape drought data with index
# This function is similar to pivot_drought_data, but it handles column names that include both an index and a category, separated by an underscore. It reshapes the dataset in a way that splits the combined column names into two separate columns: one for the index and one for the category.
# Arguments:
#   data: The dataset you want to reshape.
#   prefix: The prefix of the column names that contain the data you want to reshape.
#   index_col: The name of the new column that will contain the index. Default is "Index".
#   category: The name of the new column that will contain the categories (from the column names that start with the prefix). Default is "Category".
#   count_col: The name of the new column that will contain the values from the columns that start with the prefix. Default is "Count".
pivot_drought_data_with_index <- function(data, prefix, index_col = "Index", category = "Category", count_col = "Count") {
  pivoted_data <- data %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = c(index_col, category),
      names_sep = "_",
      values_to = count_col
    )
  return(pivoted_data)
}

# Function to reshape drought data for boxplots
# This function reshapes a dataset for easy boxplot creation. It takes data where the years are spread across multiple columns and turns it into a long format where each row represents a single year's data for a specific category.
# Arguments:
#   data: The dataset you want to reshape.
#   prefix: The prefix of the column names that contain the data you want to reshape.
#   category: The name of the new column that will contain the categories (from the column names that start with the prefix). Default is "Category".
#   count_col: The name of the new column that will contain the values from the columns that start with the prefix. Default is "Count".
pivot_drought_boxplot_data <- function(data, prefix, category = "Category", count_col = "Count") {
  pivoted_data <- data %>%
    pivot_longer(
      cols = starts_with(prefix),
      names_to = category,
      values_to = count_col
    )
  return(pivoted_data)
}
