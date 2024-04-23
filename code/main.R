## Set up up the environment

# Use decimal numbers instead of scientific notation
options(scipen=999)

# Load packages
library(tidyverse)
library(viridis)
library(minpack.lm)
library(gslnls)
library(foreach)
library(doParallel)
library(patchwork)

# Read in all functions
source("scripts/functions.R")
source("scripts/fit_models.R")

################################################################################
## Options

# Detects number of cores and sets to n-1 to use for parallel operations
# Manually set this if you need more free cores while running the code
n_cores <- detectCores()-1

iter <- 5000 # number of iterations for fitting with gsl_nls

################################################################################
## Read in data
yield_dat_raw <- read_csv(unz("data/Production_Crops_Livestock_E_All_Data.zip", 
                              "Production_Crops_Livestock_E_All_Data.csv"))

source("scripts/clean_fao_data.R")

################################################################################
## Fit and evaluate models
y = "fert_emiss_ha"
x = "yield"

{
  registerDoParallel(cores = n_cores)
  results <- NULL # list to store results in
  start_time <- proc.time()
  
  df <- subset(yield_dat, item == "Rice")
  country <- unique(df$country)
  iteration <- 1
  
  # Loop through all countries with parallelisation
  # (change %dopar% to %do% for sequential execution and debugging)
  results <- foreach(i = country, .combine = 'c') %do% { # need to make this line more general
    
    tryCatch({

      print(paste(iteration, "-", i))
      iteration <<- iteration + 1
      
    # Filter the data for the current country
    fit_df <- df[df$country == country[128], ] # for debugging - leave commented out
    fit_df <- df[df$country == i, ] # need to make this line more general
    
    # Fit models and save to results list
    model_fits <- fit_models(fit_df, y = y, x = x)
    
    temp_result <- list(
      country = i,
      model_fits = model_fits,
      AICc_table = AICc_table(model_fits),
      data = as.data.frame(fit_df[, c(y, x)]),
      predictions = generate_predictions(model_fits, fit_df)
    )
    
    temp_result$best_model <- as.character(temp_result$AICc_table[which.max(temp_result$AICc_table$wAICc), ][1])
    temp_result$data$best_model <- temp_result$predictions[which.max(temp_result$AICc_table$wAICc), ]
    temp_result$data <- temp_result$data %>%
      mutate(residuals = .data[[y]] - best_model)
    
    # Plot the best model
    fit_plot <- plot_best_model(temp_result$data, x, y)
    
    temp_dat <- temp_result$data
    
    # QQ plot to examin residuals distribution
    qqplot <-  ggplot(temp_dat, aes(sample=residuals)) + 
      stat_qq() +
      stat_qq_line(col = "red") +
      labs(x = "theoretical", y = "sample", title = "Q-Q plot") +
      theme_minimal()
    
    # Residuals plot to check for heteroskedasticity
    residuals_plot <- ggplot(temp_dat, aes(x = seq_along(residuals), y = residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(x = "Index", y = "Residuals", title = "Residuals Plot") +
      theme_minimal()

    # Autocorrelation function residuals for correlogram
    acf_residuals <- acf(na.omit(temp_dat$residuals), lag.max = 10, plot = FALSE)
    
    acf_df <- data.frame(
      lag = acf_residuals$lag,
      acf = acf_residuals$acf
    )
    
    # Correlogram plot to examine serial autocorrelation 
    correlogram <-  ggplot(acf_df, aes(x = lag, y = acf)) +
      geom_hline(yintercept = 0) +  # Add horizontal line at y = 0
      geom_segment(aes(xend = lag, yend = 0)) +  # Add segments from y = 0 to ACF values
      geom_point() +  # Add points for ACF values
      labs(x = "Lag", y = "Autocorrelation", title = paste("Correlogram of residuals")) +
      theme_minimal()
    
    temp_result$plots <- fit_plot + qqplot + residuals_plot + correlogram
    
    c(results, list(temp_result))
    
    }, error = function(e) {
      # Print error message and the value of i
      cat("Error in iteration", i, ":", conditionMessage(e), "\n")
      
      # Return a placeholder value or NULL to indicate failure
      NULL
    })
  }
  
  end_time <- proc.time()
  time_taken <- round(end_time  - start_time, 2)
  cat("Run time:", time_taken[3], "seconds")
  
  stopImplicitCluster()
}

# Output to file
current_datetime <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")
file_name <- paste0("outputs/results_", current_datetime, ".rdata")
save(results, file = file_name)

################################################################################
## Cross validation - countries vs. economic groups vs. regions vs. world
# source("cross_validation.R")

## Bootstrap confidence intervals for best performing group
# source("confidence_intervals.R")

## Produce final outputs (tables, graphs, all relevant numbers for paper)
