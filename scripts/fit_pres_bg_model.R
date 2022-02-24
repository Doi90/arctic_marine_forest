fit_pres_bg_model <- function(spp_data,
                              tuneParam = TRUE,
                              k = 5,
                              # features = c("default", "lqp", "lqh", "lq", "l"),
                              # parallel = TRUE,
                              # ncors = 4,
                              filepath){
  
  # features <- match.arg(features)
  # ncors <- min(ncors, parallel::detectCores() - 1)
  
  df <- spp_data$data
  
  ## Estimate the tuned regularization parameter
  
  if(tuneParam){
    
    k <- ifelse(sum(df$Value) <= k,
                sum(df$Value),
                k)
    
    val <- which(names(df) == "Value")
    
    best_params <- regularisedMaxent(data = df[ , c(val, 5:ncol(df))],
                                     kf = k,
                                     # parallel = parallel,
                                     # ncors = ncors,
                                     filepath = filepath)
    
  } else {
    
    best_params <- c("betamultiplier=1", "nothreshold")
    
  }
  
  ## Fit MaxEnt model
  
  presences <- df$Value
  covariates <- df[, 5:ncol(df)]
  
  maxmod <- dismo::maxent(x = covariates,
                          p = presences,
                          removeDuplicates = FALSE,
                          path = filepath,
                          args = c(best_params, "-J", "-P"))
  
  return(maxmod)
  
}

