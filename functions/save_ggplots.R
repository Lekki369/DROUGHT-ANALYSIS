# Function to create a directory if it doesn't exist
# 
# Purpose:
# This function checks if a specified directory exists, and if not, it creates the directory.
# It is useful for ensuring that the folder where files will be saved is available.
#
# Parameters:
# - path: A character string specifying the directory path to be checked/created.
#
# Returns:
# - NULL (The function performs an action without returning any value).
#
# Example:
# create_directory("path/to/directory")
# If the directory "path/to/directory" doesn't exist, it will be created.
create_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
  }
}

# Function to save a list of ggplot objects
# 
# Purpose:
# This function saves a list of ggplot plots into a specified directory, automatically
# naming the saved files with a given prefix and numeric suffix.
#
# Parameters:
# - plot_list: A list containing ggplot objects that you want to save.
# - directory: A character string specifying the directory where the plots should be saved.
# - prefix: A character string that will be used as a prefix for each plot's filename (default is "plot").
#
# Returns:
# - NULL (The function performs an action without returning any value).
#
# Example:
# save_plots(plot_list = list(plot1, plot2), directory = "plots", prefix = "my_plot")
# This will save the plots in the "plots" directory with filenames like "my_plot1.png", "my_plot2.png".
save_plots <- function(plot_list, directory, prefix = "plot") {
  # Ensure the directory exists
  create_directory(directory)
  
  # Loop through and save each plot
  for (i in seq_along(plot_list)) {
    # Handle errors in case a plot is invalid
    tryCatch({
      ggsave(
        filename = file.path(directory, paste0(prefix, i, ".png")),
        plot = plot_list[[i]],  # Specify the plot to save
        width = 8, height = 6, dpi = 300  # Customize dimensions and resolution
      )
    }, error = function(e) {
      message(paste("Failed to save plot", i, ":", e$message))
    })
  }
  
  message("All plots have been processed for saving.")
}

