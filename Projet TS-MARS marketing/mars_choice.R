
mars_choice = function(vector_DF, window_size = 30) {
  
  v = data.frame()
  
for (i in 1:length(vector_DF)){
  # Fit a basic MARS model
  f <- as.formula(vector_DF[i][[1]]$TimeSeries ~ .)
  
  #Since there are two tuning parameters associated with our MARS model: 
  # the degree of interactions and the number of retained terms, we need 
  # to perform a grid search to identify the optimal combination of these 
  # hyperparameters that minimize prediction error (the above pruning process 
  # was based only on an approximation of cross-validated performance on the
  # training data rather than an actual k-fold cross validation process). 
  # We will perform a cross-validated grid search to identify the optimal mix.
  # Here, we set up a search grid that assesses 30 different combinations of 
  # interaction effects (degree) and the number of terms to retain (nprune).
  
  # for reproducibiity
  set.seed(123)
  
  hyper_grid <- expand.grid(
    degree = 2, # for the synergy
    nprune = seq(2, 100, length.out = 10) %>% floor()
  )

  
  # cross validated model
  tuned_mars <- train(
    x = subset(vector_DF[i][[1]], select = -TimeSeries),
    y = as.numeric(vector_DF[i][[1]]$TimeSeries),
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "timeslice",
                             initialWindow = window_size,
                             horizon = 1,
                             fixedWindow = TRUE),
    tuneGrid = hyper_grid
  )

  # fit best model
  mars <- earth(
    f,
    data = vector_DF[i][[1]],
    degree = 2,
    nprune = tuned_mars$bestTune[[1]]
  )
  
  
  # Predictions
  y_pred = predict(mars)
  
  # Create data frame
  MARS = c(round(RMSE(y_pred, vector_DF[i][[1]]$TimeSeries),2),
           round(MAE(y_pred, vector_DF[i][[1]]$TimeSeries),2),
           round(mean(abs((vector_DF[i][[1]]$TimeSeries - y_pred) / vector_DF[i][[1]]$TimeSeries)) * 100,2),
           round(R2_Score(y_pred, vector_DF[i][[1]]$TimeSeries),2))
  
  if(length(v) == 0){
    v = data.frame(MARS)
  }
  else{
  v = cbind(v, MARS)
  }
  }
  df_comp = as.data.frame(v[1:length(v)])
  rownames(df_comp) = c("RMSE", "MAE", "MAPE", "R2")
  a <- seq(1,ncol(df_comp),1)
  colnames(df_comp) = sprintf("MARS%02d",a)
  return(df_comp)
}
