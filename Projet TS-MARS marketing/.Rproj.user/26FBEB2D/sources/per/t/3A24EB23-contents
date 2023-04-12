# Define a function to generate a 2d plan matrix
generate_2d_plan <- function(column, n_values) {
  max_value <- max(column)
  seq(0, max_value, length.out = n_values)
}

best_model_visualisation = function(DataFrame, model, synergy_threshold,
                              variables = variables){
  ## 3D plot
  vars <- names(data.frame(model$modvars))
  
  # Generate 2d plan matrices for each variable
  var1 <- matrix(
    unlist(lapply(generate_2d_plan(DataFrame[,variables[1]], 30), function(x) rep(x, 30))),
    ncol = 30
  )
  var2 <- matrix(
    rep(generate_2d_plan(DataFrame[,variables[2]], 30), each = 30),
    ncol = 30
  )
  
  # Find the indices of the strings "orange" and "mango"
  idx_to_replace <- which(vars %in% c(variables[1], variables[2]))
  
  # Y pred
  y_3d <- matrix(0, nrow = 30, ncol = 30)
  X_actual <- array(0., length(vars))
  
  for (i in 1:nrow(var1)){
    for (j in 1:nrow(var2)){
      X_actual[idx_to_replace[1]] = var1[i,j]
      X_actual[idx_to_replace[2]] = var2[i,j]
      
      
      #X_actual = c(GRP_first_var[i,j], GRP_second_var[i,j], 0, 0,0,0,0,0,0)
      y_3d[i,j] <- predict(model, t(data.frame(X_actual)))
      
    }
  }
  
  # Generate data for surface plot
  y_zero <- matrix(rep(0, 30^2), ncol = 30)
  
  
  ## 2D plot
  # MARS model
  set.seed(123)
  hyper_grid <- expand.grid(
    degree = 2, 
    nprune = seq(2, 100, length.out = 10) %>% floor()
  )
  tuned_mars <- train(
    x = subset(DataFrame, select = -TimeSeries),
    y = DataFrame$TimeSeries,
    method = "earth",
    metric = "RMSE",
    trControl = trainControl(method = "timeslice",
                             initialWindow = 50,
                             horizon = 1,
                             fixedWindow = TRUE),
    tuneGrid = hyper_grid
  )
  f <- as.formula(TimeSeries ~ .)
  mars_model <- earth(
    f,
    data = DataFrame,
    degree = 2,
    nprune = tuned_mars$bestTune[[1]]
  )
  
  # Create a grid of values for two variables
  n_points <- 50
  campaign1 <- variables[1]
  campaign2 <- variables[2]
  response <- 'TimeSeries'
  campaign1_grid <- seq(min(DataFrame[[campaign1]]), max(DataFrame[[campaign1]]), length.out = n_points)
  campaign2_grid <- seq(min(DataFrame[[campaign2]]), max(DataFrame[[campaign2]]), length.out = n_points)
  grid <- expand.grid(campaign1 = campaign1_grid, campaign2 = campaign2_grid)
  names(grid) <- c(campaign1,campaign2)
  for (var in setdiff(names(DataFrame), c(campaign1, campaign2, response))) {
    grid[[var]] <- rep(mean(DataFrame[[var]]), nrow(grid))
  }
  # Predict sales for each combination of campaign1 and campaign2 using the Mars model
  grid$sales <- predict(mars_model, newdata = grid)
  # Identify positive and negative synergy regions
  grid$synergy <- numeric(nrow(grid))
  sales_diff <-  (grid[[campaign1]] * 0.5 + grid[[campaign2]] * 0.5) / grid$sales - 1
  grid$synergy[sales_diff >= synergy_threshold] <- -1
  grid$synergy[sales_diff <= -synergy_threshold] <- 1
  grid$synergy[is.na(grid$synergy)] <- 0
  
  # Create a heatmap to illustrate zones of synergy, antagonism and zero effect
  p <- ggplot(grid, aes(grid[[campaign1]], grid[[campaign2]], fill = grid$synergy,
                      text = paste0(variables[1], ":", round(grid[[campaign1]], 0), ", ",
                                    variables[2], ":", round(grid[[campaign2]], 0)))) +
    geom_tile() +
    labs(fill="Synergy") +
    scale_fill_gradient2(low = "salmon", mid = "white", high = "light green", midpoint = 0) +
    theme_minimal() +
    geom_text(aes(label = ifelse(grid$synergy == 1, "+", ifelse(grid$synergy == -1, "-", ""))),
              size = 2, hjust = 0.5, vjust = 0.5)

# Create a list containing both plots
  plots <- list(
    plot_ly(x = var1, y = t(var2), z = y_3d, type = "surface") %>%
    layout(scene = list(
      xaxis = list(title = variables[1]),
      yaxis = list(title = variables[2]),
      zaxis = list(title = "Incremental Sales")
    )) %>%
    layout(title = paste("Synergy between", variables[1], "and", variables[2]), title = list(font = 25)),
  
  ggplotly(p, tooltip = c("text")) %>%
    layout(xaxis = list(title = variables[1]),
           yaxis = list(title = variables[2]),
           title = paste("Synergy (green), antagonism (red) and zero effects (white) between", variables[1], "and", variables[2]), font = list(size = 10))
  )

  # Return the list of plots
  return(htmltools::tagList(plots))
  }

