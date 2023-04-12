best_model_results = function(DataFrame_base, window_size = 30){
  
  # for reproducibility
  set.seed(123)
  
  hyper_grid <- expand.grid(
    degree = 2, 
    nprune = seq(2, 100, length.out = 10) %>% floor()
  )
  
  # cross validated model
  tuned_mars <- train(
    x = subset(DataFrame_base, select = -TimeSeries),
    y = DataFrame_base$TimeSeries,
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "timeslice",
                             initialWindow = window_size,
                             horizon = 1,
                             fixedWindow = TRUE),
    tuneGrid = hyper_grid
  )
  
  
  f <- as.formula(TimeSeries ~ .)
  
  # fit best model
  mars <- earth(
    f,
    data = DataFrame_base,
    degree = 2,
    nprune = tuned_mars$bestTune[[1]]
  )

  print(summary(mars))
  
  data <- mars$residuals
  
  autocorrelation <- function(x, lag) {
    n <- length(x)
    if(lag >= n) {
      return(NA)
    }
    numerator <- sum((x[1:(n-lag)] - mean(x))*(x[(lag+1):n] - mean(x)))
    denominator <- sum((x - mean(x))^2)
    return(numerator/denominator)
  }
  
  # Perform autocorrelation test for lags 1 to 10
  lags <- 1:10
  acf <- sapply(lags, function(lag) autocorrelation(data, lag))
  
  # Calculate critical values for significance level 0.05
  n <- length(data)
  cv_upper <- qnorm(0.025)/sqrt(n)
  cv_lower <- -qnorm(0.025)/sqrt(n)
  
  # Plot the autocorrelation function with confidence intervals
  plot(lags, acf, type='h', lwd=2, col='blue', ylim=c(-1,1))
  abline(h=c(cv_upper, cv_lower), lty=2, lwd=2, col='red')
  abline(h=0, lwd=2, col='black')
  title('Autocorrelation Test')
  legend('topright', legend=c('ACF', '95% CI', 'Zero'), lwd=c(2,2,2), col=c('blue', 'red', 'black'))

  
  # Calculate the correlation matrix of the predictor variables
  X = DataFrame_base %>% select(-TimeSeries) %>% as.matrix()
  corr_matrix <- cor(X)
  
  # Calculate the variance inflation factors (VIFs)
  VIFs <- sapply(1:ncol(X), function(i) {
    X_i <- X[, -i, drop=FALSE]
    r2_i <- summary(lm(X[, i] ~ X_i))$r.squared
    return(1 / (1 - r2_i))
  })
  
  # Calculate the condition indices
  eigen_values <- eigen(corr_matrix)$values
  condition_indices <- sqrt(eigen_values[1]/eigen_values)
  
  # Test for multicollinearity
  high_VIFs <- VIFs > 10
  if(any(high_VIFs)) {
    cat("Warning: High multicollinearity detected for variables:", paste0(colnames(X)[high_VIFs], collapse=", "), "\n")
  }
  
  high_CIs <- condition_indices > 30
  if(any(high_CIs)) {
    cat("Warning: High multicollinearity detected for variables:", paste0(colnames(X)[high_CIs], collapse=", "), "\n")
  }
  # variable importance plots
  print(vip(mars, num_features = 15))
  
  return(mars)
}
