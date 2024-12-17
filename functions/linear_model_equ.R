# Function to generate a linear model equation as a string
# This function fits a linear model to two variables in a dataset and returns the equation of the fitted line along with the R-squared value as a string.
# Arguments:
#   df: The dataset containing the variables to be modeled.
#   x_var: The name of the variable to be used as the predictor (x-axis).
#   y_var: The name of the variable to be used as the response (y-axis).
lm_eqn <- function(df, x_var, y_var) {
  # Fit the linear model
  model <- lm(as.formula(paste(y_var, "~", x_var)), data = df)
  
  # Create the equation string with the coefficients and R-squared value
  eq <- substitute(
    italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2, 
    list(
      a = format(unname(coef(model)[1]), digits = 2, scientific = FALSE),  # Intercept
      b = format(unname(coef(model)[2]), digits = 2, scientific = FALSE),  # Slope
      r2 = format(summary(model)$r.squared, digits = 3)                    # R-squared value
    )
  )
  
  # Return the equation as a string
  as.character(as.expression(eq))
}
