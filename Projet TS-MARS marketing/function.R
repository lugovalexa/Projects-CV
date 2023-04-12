lag_multiple <- function(x, n_vec){
  map(n_vec, lag, x = x) %>% 
    set_names(paste0("TimeSeries_lag", n_vec)) %>% 
    as_tibble()
}

get_season <- function(date) {
  if (month(date) %in% c(12, 1, 2)) {
    return("winter")
  } else if (month(date) %in% c(3, 4, 5)) {
    return("spring")
  } else if (month(date) %in% c(6, 7, 8)) {
    return("summer")
  } else if (month(date) %in% c(9, 10, 11)) {
    return("fall")
  }
}

transform_diminishing <- function(df, variables, adstock_rates, decay_rates) {
  transf_df <- data.frame(matrix(nrow = nrow(df), ncol = 0))
  for (i in 1:length(variables)) {
    col <- df[,variables[i]]
    decay_rate <- decay_rates[i]
    adstock_rate <- adstock_rates[i]
    transf_col <- numeric(length(col))
    transf_col[1] <- col[1]^adstock_rate
    for (j in 2:length(col)) {
      transf_col[j] <- (transf_col[j-1]^decay_rate + col[j])^adstock_rate
    }
    transf_df <- cbind(transf_df, transf_col)
  }
  colnames(transf_df) <- variables
  
  df <- df[!colnames(df) %in% variables]
  df <- cbind(df, transf_df)
  
  return(df)
}

main = function(DataFrame, 
         drop_seas = FALSE,
         related_var_season = 'seasonfall',
         standardize = FALSE,
         LAG = 0,
         breaks = 0, 
         adStock = FALSE,
         variables_ads = c()){

  DataFrame = DataFrame %>% na.omit()
  
  DataFrame$Date = as.Date(DataFrame$Date)
  
  freq <- as.numeric(mean(diff(DataFrame$Date)))
  
  # Create a time series object
  ts_temp <- ts(
    DataFrame$TimeSeries, 
    frequency = 365.14/freq)
  
  ## Decompose a time series into seasonal, trend and irregular components
  ts1 <- stl(ts_temp, s.window = "periodic")
  
  if (adf.test(DataFrame$TimeSeries)$p.value > 0.05){
    if (breaks > 0) {
      
      # Identify structural breaks
      x <- DataFrame$TimeSeries
      breaks_list <- breakpoints(x ~ 1, breaks = breaks)
      
      # Extract segments corresponding to each break
      segments <- data.frame(start = c(1, breaks_list$breakpoints + 1),
                             end = c(breaks_list$breakpoints, length(x)),
                             stringsAsFactors = FALSE)
      
      # Remove trend from each segment using a linear model
      for (i in 1:nrow(segments)) {
        x[segments$start[i]:segments$end[i]] <- resid(lm(x[segments$start[i]:segments$end[i]] ~ seq_along(x[segments$start[i]:segments$end[i]])))
      }
      DataFrame$TimeSeries <- x
    }
  }
    
  if (drop_seas == TRUE){
    DataFrame$TimeSeries <- ts1$time.series[, "remainder"]
    ## Save seasonal component for using in the model
    DataFrame$seasonality_all = ts1$time.series[, "seasonal"]
    
    med = median(DataFrame$seasonality_all)
    sd = sd(DataFrame$seasonality_all)
    dif_high = med + sd
    dif_low = med - sd
    
    DataFrame$high_seasons = if_else(dif_high > DataFrame$seasonality_all, 0, as.numeric(DataFrame$seasonality_all))
    
    DataFrame$low_seasons = if_else(dif_low < DataFrame$seasonality_all, 0, as.numeric(DataFrame$seasonality_all))
    
    # Get the season based on month
    DataFrame$season <- apply(DataFrame, 1, function(x) get_season((x["Date"])))
    
    # Create dummy variables for season
    one_hot_season <- dummyVars(~ season, data <- DataFrame)
    one_hot_season_df <- data.frame(predict(one_hot_season, newdata <- DataFrame))
    
    # Concatenate dummy season and the rest of the df
    DataFrame <- cbind(DataFrame, one_hot_season_df)
    
    # Drop the season
    DataFrame <-DataFrame %>% select(-c(season, seasonality_all, related_var_season[1]))
    
    }
  
  if (standardize == TRUE){
      DataFrame_numeric = DataFrame %>%
        select(where(is.numeric) & !where(~all(.x %in% 0:1)), -Date) 
      DataFrame_numeric_scaled = scale(DataFrame_numeric) 
      DataFrame <- do.call(cbind, list(DataFrame %>% select(Date, where(~all(.x %in% 0:1))), DataFrame_numeric_scaled))

  }
  
  if (LAG > 0){
    
    DataFrame = DataFrame %>%
      mutate(lag_multiple(as.numeric(TimeSeries), 1:LAG)) %>%
      na.omit() %>% 
      as.data.frame(row.names = 1:nrow(.))
    
  }
  
  if (adStock == TRUE){
    
    max_decay = 1
    max_adstock = 1
    
    decay_rates <- seq(0.1, max_decay, length.out = length(variables_ads)+3)
    adstock_rates <- seq(0.1, max_adstock, length.out = length(variables_ads)+3)
    
    # Generate all possible combinations of decay rates
    combinations <- combn(decay_rates, length(variables_ads))
    # Create a matrix from the combinations
    num_combinations <- ncol(combinations)
    matrix_decay <- matrix(0, nrow = num_combinations, ncol = length(variables_ads))
    matrix_adstock <- matrix(0, nrow = num_combinations, ncol = length(variables_ads))
    for (i in 1:num_combinations) {
      matrix_decay[i, ] <- combinations[, i]
      matrix_adstock[i,] <- combinations[, i]
    }
    
    mse <- matrix(0, dim(matrix_decay)[1], dim(matrix_adstock)[1]) # initialize matrix of MSE values
    best_decay <- rep(0, ncol(DataFrame)) # initialize vector of best decay rates
    best_adstock <- rep(0, ncol(DataFrame)) # initialize vector of best adstock rates
    
    for (j in 1:dim(matrix_decay)[1]) {
      for (k in 1:dim(matrix_adstock)[1]) {
        df_transf <- transform_diminishing(DataFrame, variables_ads, matrix_adstock[j,], matrix_decay[k,])
        DataFrame = data.frame(df_transf)
      }
    }
  }
  return(DataFrame)
}
