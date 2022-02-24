cross_validate <- function(spp_data,
                           type = c("po", "pa"),
                           k = 5,
                           filepath,
                           # features = c("default", "lqp", "lqh", "lq", "l"),
                           # parallel = FALSE,
                           # parallel_tuning = FALSE,
                           # ncors = 4,
                           spatial_cv = FALSE,
                           poly_blocks,
                           tuneParam_CV = TRUE){
  
  library(dismo)
  
  # features <- match.arg(features)
  
  df <- spp_data$data
  
  ## Check if the arguments are correct
  
  type <- match.arg(type)
  
  k <- as.integer(k)
  
  # ncors <- as.integer(ncors)
  
  ## Stratified cross-validation folds
  
  if(spatial_cv){
    
    spsf <- sf::st_as_sf(df,
                         coords = c("Long", "Lat"),
                         crs = "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")
    
    folds <- blockCV::spatialBlock(speciesData = spsf,
                                   rows = 15,
                                   cols = 15,
                                   k = k)
    
  } else {
    
    folds <- caret::createFolds(y = as.factor(df$Value), k)
    
  }
  
  # if(parallel){
  #
  #   require(foreach)
  #   ncors <- min(ncors,
  #                parallel::detectCores() - 1)
  #   ## Make a parallel computing cluster
  #
  #   cluster <- snow::makeCluster(ncors,
  #                                type = "SOCK")
  #
  #   doSNOW::registerDoSNOW(cluster)
  #
  #   pp <- foreach::foreach(ks = seq_len(k),
  #                          .inorder = TRUE,
  #                          .export = c("fit_pres_bg_model",
  #                                      "regularisedMaxent",
  #                                      "fit_pres_abs_model"),
  #                          .packages = c('precrec',
  #                                        # 'maxnet',
  #                                        'ecospat',
  #                                        'dismo')) %dopar% {
  #
  #                                          trainSet <- unlist(folds[-ks])
  #
  #                                          testSet <- unlist(folds[ks])
  #
  #                                          if(type == "po"){
  #
  #                                            ## Fit a maxent
  #
  #                                            mxnt <- fit_pres_bg_model(spp_data = list(data = df[trainSet, ]),
  #                                                                      tuneParam = tuneParam_CV,
  #                                                                      filepath = filepath,
  #                                                                      k = k,
  #                                                                      # features = features,
  #                                                                      parallel = FALSE) # parallel must be FALSE here
  #
  #                                            prediction <- as.vector(predict(mxnt,
  #                                                                            df[testSet, 14:ncol(df)],
  #                                                                            args = "outputformat=cloglog"))
  #
  #                                          } else {
  #
  #                                            ## fit a brt
  #
  #                                            brt <- fit_pres_abs_model(spp_data = list(data = df[trainSet, ]))
  #
  #                                            prediction <- predict(brt,
  #                                                                  df[testSet , 14:ncol(df)],
  #                                                                  n.trees = brt$gbm.call$best.trees,
  #                                                                  type = "response")
  #
  #                                          }
  #
  #                                          ## Calculate the AUC
  #
  #                                          aucs <- precrec::auc(precrec::evalmod(scores = prediction,
  #                                                                                labels = df$Value[testSet]))[1, 4]
  #
  #                                          # Calculate Boyce index
  #
  #                                          pb_test <- df$Value[testSet]
  #                                          pres_indx <- which(pb_test == 1)
  #
  #                                          byc <- ecospat::ecospat.boyce(fit = prediction,
  #                                                                        obs = prediction[pres_indx],
  #                                                                        PEplot = FALSE)$Spearman.cor
  #
  #                                          # calculate the best threshold
  #                                          thedata <- data.frame(id = 1:length(prediction),
  #                                                                obs = df$Value[testSet],
  #                                                                pred = prediction)
  #
  #                                          thr <- PresenceAbsence::optimal.thresholds(DATA = thedata,
  #                                                                                     threshold = 101,
  #                                                                                     which.model = 1,
  #                                                                                     opt.methods = "MaxSens+Spec")
  #                                          outauc <- data.frame(roc = aucs,
  #                                                               boyce = byc,
  #                                                               threshold = thr$pred[1])
  #
  #                                        }
  #
  #   snow::stopCluster(cluster)
  #
  #   foreach::registerDoSEQ()
  #
  #   aucboth <- do.call(rbind.data.frame,
  #                      pp)
  #
  # } else {
  
  # pp <- vector(mode = "numeric", length = k)
  
  aucboth <- data.frame(roc = rep(0, k),
                        boyce = 0,
                        threshold = NA,
                        tss = 0)
  
  for(ks in seq_len(k)){
    
    if(spatial_cv){
      
      trainSet <- unlist(folds$folds[[ks]][[1]])
      
      testSet <- unlist(folds$folds[[ks]][[2]])
      
    } else {
      
      trainSet <- unlist(folds[-ks])
      
      testSet <- unlist(folds[ks])
      
    }
    
    if(type == "po"){
      
      ## fit a maxent
      # mxnt <- fit_pres_bg_model(list(data = df[trainSet, ]),
      #                           tuneParam = tuneParam_CV,
      #                           parallel = parallel_tuning,
      #                           ncors = ncors,
      #                           features = features)
      mxnt <- fit_pres_bg_model(spp_data = list(data = df[trainSet, ]),
                                tuneParam = tuneParam_CV,
                                filepath = filepath,
                                k = k)
      # parallel = FALSE) # parallel must be FALSE here
      
      prediction <- as.vector(predict(mxnt,
                                      df[testSet, 5:ncol(df)],
                                      args = "outputformat=cloglog"))
      
    } else {
      
      # fit a brt
      
      brt <- fit_pres_abs_model(list(data = df[trainSet, ]))
      
      prediction <- predict(brt,
                            df[testSet , 5:ncol(df)],
                            n.trees = brt$gbm.call$best.trees,
                            type = "response")
      
    }
    
    ## Calculate the AUC
    
    aucboth$roc[ks] <- precrec::auc(precrec::evalmod(scores = prediction,
                                                     labels = df$Value[testSet]))[1, 4]
    
    # Calculate Boyce index
    
    pb_test <- df$Value[testSet]
    pres_indx <- which(pb_test == 1)
    
    aucboth$boyce[ks] <- ecospat::ecospat.boyce(fit = prediction,
                                                obs = prediction[pres_indx],
                                                PEplot = FALSE)$Spearman.cor
    
    # calculate the best threshold
    thedata <- data.frame(id = 1:length(prediction),
                          obs = df$Value[testSet],
                          pred = prediction)
    
    thr <- PresenceAbsence::optimal.thresholds(DATA = thedata,
                                               threshold = 101,
                                               which.model = 1,
                                               opt.methods = "MaxSens+Spec")
    
    aucboth$threshold[ks] <- thr$pred[1]
    
    ## Calculate TSS
    
    prediction_thresh <- as.numeric(prediction > aucboth$threshold[ks])
    
    sens <- caret::sensitivity(factor(prediction_thresh,
                                      levels = c("1", "0")),
                               factor(df[trainSet, ]$Value,
                                      levels = c("1", "0")))
    
    spec <- caret::specificity(factor(prediction_thresh,
                                      levels = c("1", "0")),
                               factor(df[trainSet, ]$Value,
                                      levels = c("1", "0")))
    
    tss <- sens + spec - 1
    
    aucboth$tss[ks] <- tss
    
  }
  # }
  
  cat("Summary of the evaluation:\n")
  
  cat(sprintf("AUC-ROC score: %s ; SE = %s \n",
              round(mean(aucboth$roc), 4),
              round(sd(aucboth$roc) / sqrt(k), 4)))
  
  cat(sprintf("Boyce index: %s ; SE = %s \n",
              round(mean(aucboth$boyce), 4),
              round(sd(aucboth$boyce) / sqrt(k), 4)))
  
  cat(sprintf("Best threshold: %s ; SE = %s \n",
              round(mean(aucboth$threshold), 4),
              round(sd(aucboth$threshold) / sqrt(k), 3)))
  
  cat(sprintf("TSS: %s ; SE = %s \n",
              round(mean(aucboth$tss), 4),
              round(sd(aucboth$tss) / sqrt(k), 3)))
  
  return(c("AUC" = round(mean(aucboth$roc), 4),
           "Boyce" = round(mean(aucboth$boyce), 4),
           "Threshold" = round(mean(aucboth$threshold), 3),
           "TSS" = round(mean(aucboth$tss), 4)))
  
}
