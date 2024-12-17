### This function return the drought exposure, vulnerability & resilience from their respective functions
### (calculate_exposure, calculate_vulnerability & calculate_resilience)
### [drought_data] which is the indice dataset
### [dataset] is the actual station dataset 
### [index] is the indice type (spei3, spi3 etc)
### [total duration] is the total years count in the [dataset]
add_drought_characteristics <- function(drought_data, dataset, index, total_years_count) {
  drought_data <- drought_data %>%
    mutate(
      DroughtExposure = calculate_exposure(drought_data, index, total_years_count),
      DroughtVulnerability = calculate_vulnerability(dataset, index),
      DroughtResilience = calculate_resilience(dataset, index)
    )
  return(drought_data)
}
