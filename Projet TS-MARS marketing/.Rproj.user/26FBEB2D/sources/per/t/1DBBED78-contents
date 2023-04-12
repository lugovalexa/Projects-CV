compare_models <- function(data) {
  
  # train mars
  hyper_grid <- expand.grid(
    degree = 2, 
    nprune = seq(2, 100, length.out = 10) %>% floor()
  )
  set.seed(123)
  tuned_mars <- train(
    x = data %>% select(-TimeSeries),
    y = data$TimeSeries,
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "timeslice",
                             initialWindow = 50,
                             horizon = 1,
                             fixedWindow = TRUE),
    tuneGrid = hyper_grid
  )
  # train other models
  set.seed(123)
  models <- c("lm", "pcr", "pls", "glmnet", "rf", "gbm")
  
  results <- list()
  for (model in models) {
    result <- train(
      TimeSeries ~ .,
      data = data,
      method = model,
      trControl = trainControl(
        method = "timeslice",
        initialWindow = 50,
        horizon = 1,
        fixedWindow = TRUE
      ),
      metric = "RMSE",
      preProcess = c("center", "scale"),
      tuneLength = ifelse(model %in% c("glmnet", "rf", "gbm"), 10, 20),
      verbose = FALSE
    )
    
    results[[model]] <- result
  }
  # extract out of sample performance measures
  summary(resamples(list(
    Multiple_regression = results$lm, 
    PCR = results$pcr, 
    PLS = results$pls,
    Elastic_net = results$glmnet,
    Random_forest = results$rf,
    Gradient_boosting = results$gbm,
    MARS = tuned_mars
  )))$statistics$RMSE %>%
    kableExtra::kable() %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
}