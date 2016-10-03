NormalIntradayPrediction_LargeCalls <- function(Data.training, lg, Interval){
  # Data.training = dataframe(DateTime, Items), that is cleaned data with fixed interval
  # lg = no. of days to forecast, that starts from next unavailable day
  
  # output = lg*60*24 numeric data
  
  ############################################
  ### 1. BoxCox transformation
  ### 2. Intraday prediction
  ### 3. Inverse BoxCox
  ############################################
  
  
  ### 1. BoxCox transformation
  Input.data <- Data.training
  Lambda <- BoxCox.lambda(Input.data$Items)
  Input.data$BoxCox <- BoxCox(Input.data$Items, Lambda)
  
  
  ### 2. Intraday prediction
  Fit <- tryCatch(
    {
      Seasonal1 <- 24 #Distributed smaller interval later, 60*24/as.integer(Interval)
      Seasonal2 <- 7*Seasonal1
      Data.msts <- msts(Input.data$BoxCox, seasonal.periods = c(Seasonal1, Seasonal2))
      Fit <- tbats(Data.msts, use.box.cox = F, 
                   seasonal.periods = c(Seasonal1, Seasonal2),
                   use.trend = T,  use.damped.trend= T,
                   use.arma.errors = T)
    },
    # warning = function(cond){
    #   Data.ts <- ts(Input.data$BoxCox, frequency = 7*24*60/as.integer(Interval))
    #   Fit <- ets(Data.ts)
    #   return(Fit)
    # },
    error = function(cond){
      Data.ts <- ts(Input.data$BoxCox, frequency = 7*24*60/as.integer(Interval))
      Fit <- ets(Data.ts)
      return(Fit)
    }
  )
  
  Results.temp <- forecast(Fit, h =lg*24)
  
  ### 3. Inverse BoxCox
  Results <- InvBoxCox(as.numeric(Results.temp$mean), Lambda)
  Results[which(Results < 0)] <- 0 
  
  return(Results)
}
