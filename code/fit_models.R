fit_models <- function(fit_df, y = y, x = x) {
  # List to store model fits in
  model_fits <- list()
  
  # Ensure breakpoints are at least n data points from min/max values
  max_x <- sort(fit_df[[x]], decreasing = TRUE)[6]
  min_x <- sort(fit_df[[x]], decreasing = FALSE)[6]
  
  # Linear model
  print("Fitting linear model")
  model_fits$lm_fit <- lm(paste(y, "~", x), data = fit_df)
  
  # Sqrt transformation
  print("Fitting linear (sqrt)")
  model_fits$sqrt_fit <- lm(paste(y, "~ sqrt(", x, ")", sep = ""), data = fit_df)
  
  # Log transformation
  print("Fitting linear (log)")
  model_fits$log_fit <- lm(paste(y, "~ log(", x, ")", sep = ""), data = fit_df)
  
  # Intercept only
  print("Fitting intercept only")
  model_fits$io_fit <- lm(paste(y, "~ 1", sep = ""), data = fit_df)
  
  # Piecewise linear model
  print("Fitting piecewise linear")
  model_fits$piecewise_fit <- nlsLM(paste(y, "~ piecewise(", x, ", a1, b1, b2, clx)", sep = ""),
                                    data = fit_df,
                                    start = list(a1 = model_fits$lm_fit$coefficients[1],
                                                 b1 = model_fits$lm_fit$coefficients[2],
                                                 b2 = model_fits$lm_fit$coefficients[2],
                                                 clx = mean(fit_df[[x]])),
                                    lower = c(-Inf, 0, -Inf, min_x),
                                    upper = c(Inf, Inf, Inf, max_x),
                                    control = nls.lm.control(maxiter = 1024))

  # Quadratic plateau
  print("Fitting quadratic plateau")
  model_fits$quadp_fit <- nlsLM(paste(y, "~ quadplat(", x, ", a, b, clx)", sep = ""),
                                data = fit_df,
                                start = list(a = model_fits$lm_fit$coefficients[1],
                                             b = model_fits$lm_fit$coefficients[2],
                                             clx = coef(model_fits$piecewise_fit)[4]),
                                lower = c(-Inf, -Inf, min_x),
                                upper = c(Inf, Inf, max_x),
                                control = nls.lm.control(maxiter = 1024))

  # Linear plateau
  print("Fitting linear plateau")
  model_fits$linearplat_fit <- nlsLM(paste(y, "~ linearplat(", x, ", a, b, clx)", sep = ""),
                                     data = fit_df,
                                     start = list(a = model_fits$lm_fit$coefficients[1],
                                                  b = model_fits$lm_fit$coefficients[2],
                                                  clx = coef(model_fits$piecewise_fit)[4]),
                                     lower = c(-Inf, -Inf, min_x),
                                     upper = c(Inf, Inf, max_x),
                                     control = nls.lm.control(maxiter = 1024))
  
  # # Fit the model using maximum likelihood estimation - test
  # model_fits$linearplat_mle <- mle(mle_linearplat(fit_df),
  #                                  start = c(model_fits$lm_fit$coefficients[1],
  #                                            model_fits$lm_fit$coefficients[2],
  #                                            coef(model_fits$piecewise_fit)[4],
  #                                            1),
  #                                  method = "L-BFGS-B",
  #                                  lower = c(-Inf, -Inf, min(df$gdp), 0.001),
  #                                  upper = c(Inf, Inf, max(df$gdp), Inf))
  
  # Commented out for now - not currently fitting reliably
  # Michaelis Menten model
  # print("Fitting Michaelis Menten")
  # model_fits$mm_fit <- nlsLM(paste(y, "~ michaelis_menten(", x, ", Vmax, Km)", sep = ""),
  #                            data = fit_df,
  #                            start = list(Vmax = max(fit_df[[x]]),
  #                                         Km = mean(fit_df[[x]])),
  #                            control = nls.lm.control(maxiter = 1024))
  
  
  # model_fits$mm_fit <- gsl_nls(paste(y, "~ michaelis_menten(", x, ", Vmax, Km)", sep = ""),
  #                              data = fit_df,
  #                              start = list(Vmax = max(fit_df[[x]]),
  #                                           Km = mean(fit_df[[x]])),
  #                              #lower = c(-Inf, -Inf),
  #                              #upper = c(Inf, Inf),
  #                              algorithm = "lmaccel",
  #                              control = list(scale = "levenberg", maxiter = iter))

  # # Probably won't include this model - but leaving here just in case
  # Plateau-linear _/
  # print("Fitting plateau-linear")
  # model_fits$linearplat_fit2 <- nlsLM(paste(y, "~ linearplat2(", x, ", a, b, clx)", sep = ""),
  #                                     data = fit_df,
  #                                     start = list(a = model_fits$lm_fit$coefficients[1],
  #                                                  b = model_fits$lm_fit$coefficients[2],
  #                                                  clx = coef(model_fits$piecewise_fit)[4]),
  #                                     lower = c(-Inf, -Inf, min_x),
  #                                     upper = c(Inf, Inf, max_x),
  #                                     control = nls.lm.control(maxiter = 1024))
  #
  return(model_fits)
}
