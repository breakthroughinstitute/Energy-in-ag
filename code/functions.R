### Functions for fitting models and performing other tasks

## Model functions #############################################################
# Linear plateau
linearplat <- function(x, a, b, clx) {  
  ifelse(x < clx, a + b * x, a + b * clx)  
}

# Define the likelihood function for linear plateau
neg_log_likelihood <- function(params, x, y) {
  a <- params[1]
  b <- params[2]
  clx <- params[3]
  sigma <- params[4]
  
  y_pred <- ifelse(x < clx, a + b * x, a + b * clx)
  
  likelihood <- dnorm(y, mean = y_pred, sd = sigma, log = TRUE)
  nll <- -sum(likelihood)
  
  return(nll)
}

# Define the function to fit
mle_linearplat <- function(data) {
  function(params) {
    x <- data$gdp_ppp
    y <- data$tot_kcal
    neg_log_likelihood(params, x, y)
  }
}

# Plateau-linear
linearplat2 <- function(x, a, b, clx) {  
  ifelse(x < clx, a, a + b * (x - clx))
}

# Quadratic plateau
quadplat <- function(x, a, b, clx) {
  ifelse(x  < clx, a + b * x   + (-0.5 * b / clx) * x  * x,
         a + b * clx + (-0.5 * b / clx) * clx * clx)
}

# Piecewise
piecewise <- function(x, a1, b1, b2, clx) {
  ifelse(x < clx, a1 + b1 * x, a1 + b1 * clx + b2 * (x - clx))
}

# Michaelis Menten
michaelis_menten <- function(x, Vmax, Km) {
  V <- Vmax * x / (Km + x)
  return(V)
}

###
# The following model functions are used for plotting purposes only
###
# Linear model
linear_model <- function(x, m, b) {
  m * x + b
}

# Sqrt transformed
sqrt_model <- function(x, m, b) {
  m * sqrt(x) + b
}

# Log transformed
log_model <- function(x, m, b) {
  m * log(x) + b
}

# Intercept only
intercept_only_model <- function(x, b) {
  b
}

## Functions to compute AICc and wAICc table ###################################
# AICc function
compute_AICc <- function(model) {
  n <- nobs(model)
  k <- length(coef(model))
  logLik_value <- logLik(model)
  AICc_value <- -2 * logLik_value + 2 * k * (n / (n - k - 1))
  
  return(AICc_value)
}

# Function to construct wAICc table from a list of models
AICc_table <- function(models) {
  table_data <- data.frame(Model = character(), 
                           AICc = numeric(), 
                           Num_Params = integer(), 
                           Num_Obs = integer(), 
                           LogLik = numeric(),
                           DeltaAIC = numeric(), 
                           wAICc = numeric(),
                           stringsAsFactors = FALSE)
  
  # Calculate min_AICc
  min_AICc <- min(sapply(models, function(model) compute_AICc(model)))
  
  wAICc_sum <- 0
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    
    # Calculate AICc
    AICc_value <- compute_AICc(model)
    
    # Calculate Log-Likelihood
    logLik_value <- logLik(model)
    
    # Calculate Delta AIC
    delta_AIC <- AICc_value - min_AICc
    
    # Calculate wAICc
    wAICc_value <- exp(-0.5 * delta_AIC)
    wAICc_sum <- wAICc_sum + wAICc_value
    
    # Add to the table
    table_data <- rbind(table_data, data.frame(Model = names(models)[i],
                                               AICc = AICc_value,
                                               Num_Params = length(coef(model)),
                                               Num_Obs = nobs(model),
                                               LogLik = logLik_value,
                                               DeltaAIC = delta_AIC,
                                               wAICc = wAICc_value))
  }
  
  # Normalise wAICc to sum to 1
  table_data$wAICc <- table_data$wAICc / wAICc_sum
  
  # Order the table by AICc
  #table_data <- table_data[order(table_data$AICc), ]
  
  return(table_data)
}

## Construct prediction table with model-averaged ensemble model ###############
generate_predictions <- function(models, data) {
  predictions_list <- list()
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    predictions <- predict(model, newdata = data)
    predictions_list[[i]] <- predictions
  }
  
  prediction_df <- as.data.frame(do.call(cbind, predictions_list))
  colnames(prediction_df) <- names(models)
  
  return(t(prediction_df))
}

## Function to compute RMSE ####################################################
compute_rmse <- function(model, data_frame, response_var) {
  # Get model predictions
  predicted <- predict(model, newdata = data_frame)
  
  # Extract the actual values from the data frame
  observed <- data_frame[[response_var]]
  
  # Calculate RMSE
  rmse <- sqrt(mean((observed - predicted)^2))
  
  return(rmse)
}

## Function to compute GDP/capita quantiles ####################################
compute_quantile_groups <- function(df, var, q) {
  quantiles <- df %>%
    group_by(year) %>%
    mutate(
      gdp_quantile = cut({{ var }},
                         breaks = quantile({{ var }}, 
                                           probs = seq(0, 1, by = 1/q), 
                                           type = 8,
                                           na.rm = TRUE), 
                         labels = FALSE, 
                         include.lowest = TRUE),
      quantile_range = cut({{ var }},
                           breaks = quantile({{ var }}, 
                                             probs = seq(0, 1, by = 1/q), 
                                             type = 8,
                                             na.rm = TRUE),
                           include.lowest = TRUE)
    )
  
  return(quantiles)
}

#E.g. new_df <- compute_quantile_groups(df, gdp_ppp, 7)

## Function to compute kcal and gdp for economic/regional groups ###############
compute_weighted_average <- function(df, var) {
  result <- df %>%
    group_by(year, {{ var }}) %>%
    summarise(
      kcal = sum(tot_kcal * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      gdp = sum(gdp_ppp * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      population = sum(population, na.rm = TRUE)
    )
  
  return(result)
}

# E.g. new_df <- compute_weighted_average(df, gdp_quantile)

# Function to pass fitted models to ggplot's stat_function
plot_fitted_function <- function(func, coef_vector) {
  function(x) do.call(func, c(list(x), unname(as.list(coef_vector))))
}

## Plot ensemble fit results
plot_ensemble <- function(results, x, y) {
  plots <- lapply(results, function(result) {
    data <- result$data  # Extract the data frame from the nested list
    ggplot(data, aes_string(x = x, y = y)) +
      geom_point() +
      geom_line(aes_string(x = x, y = "ensemble_est"), color = "red") +
      labs(title = result$country,  # Use result$country as the title
           x = x,
           y = y) +
      theme_minimal()
  })
  
  return(plots)
}

## Plot ensemble fit results
plot_ensemble <- function(results, x, y) {
  plots <- lapply(results, function(result) {
    data <- result$data  # Extract the data frame from the nested list
    ggplot(data, aes_string(x = x, y = y)) +
      geom_point() +
      geom_line(aes_string(x = x, y = "ensemble_est"), color = "red") +
      labs(title = result$country,  # Use result$country as the title
           x = x,
           y = y) +
      theme_minimal()
  })
  
  return(plots)
}

## Plot best model
plot_best_model <- function(results, x, y) {
  ggplot(results, aes_string(x = x, y = y)) +
    geom_point() +
    geom_line(aes_string(x = x, y = "best_model"), color = "red") +
    labs(title = paste("Best model -", temp_result$best_model),  # Use the specified title
         x = x,
         y = y) +
    theme_minimal()
}
