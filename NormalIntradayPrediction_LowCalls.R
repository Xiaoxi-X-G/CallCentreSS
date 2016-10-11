NormalIntradayPrediction_LowCalls <- function(Data.training, lg, Interval){
  # Data.training = dataframe(DateTime, Items), that is cleaned data with fixed interval
  # lg = no. of days to forecast, that starts from next unavailable day
  
  # output = lg X 60*24/Interval matrix
  
  ###################################################
  ### 1. Aggregate the data to daily level
  ### 2. Smooth the data using Loess, 7-points per ploynomial
  ### 3. Forecast daily arrival calls
  ### 4. Distributed to each intraday interval
  ###################################################
  
  #### 1. Aggregate the data to daily level ####
  Data.training.daily.temp <- Data.training
  Data.training.daily.temp[,1] <- as.POSIXct(Data.training.daily.temp[,1], origin = "1970-01-01", tz="GMT")
  
  Data.training.daily <- aggregate(as.integer(Data.training.daily.temp$Items),
                                   list(Date=format(Data.training.daily.temp$DateTime, "%Y-%m-%d")),
                                   FUN=sum)
  colnames(Data.training.daily)[2] <- "Items"
  
  ### 2. Smooth the data using Loess,  ####
  # NOPoint <- 7 # Define locate data-set, i.e., 7-points per ploynomial
  # alpha <- NOPoint/nrow(Data.training.daily)
  # lo <- loess(Data.training.daily$Items ~ as.numeric(as.POSIXct(Data.training.daily$Date, origin = "1970-01-01", tz="GMT")),
  #             span = alpha,
  #             parametric = F)
  
  Lambda <- BoxCox.lambda(Data.training.daily$Items)
  Data.training.daily$BoxCox <- BoxCox(Data.training.daily$Items, Lambda)
  
  
  ### 3. Forecast daily arrival calls ####
  #Data.ts <- msts(lo$fitted, seasonal.periods = 7)
  Data.ts <- msts(Data.training.daily$BoxCox, seasonal.periods = 7)
  Fit <- tryCatch(
    {
      Fit <-  bats(Data.ts, use.box.cox = F, 
                    seasonal.periods = 7,
                    use.trend = T,  use.damped.trend= T,
                    use.arma.errors = T)
    },
    warning = function(cond){
      Fit <- ets(Data.ts)
      return(Fit)
    },
    error = function(cond){
      Fit <- ets(Data.ts)
      return(Fit)
    }
  )
  Results.temp <- forecast(Fit, h =lg)
  Results <- InvBoxCox(as.numeric(Results.temp$mean), Lambda)
  Results[which(Results < 0)] <- 0 

  
  ### 4. Distributed to each intraday interval ####
  Data.matrix.intra <- t(matrix(as.numeric(Data.training$Items), nrow  = 24*60/as.integer(Interval)))
  
  colnames(Data.matrix.intra) <- seq(from =0, by=as.integer(Interval)/60,
                                     length = 60/as.integer(Interval)*24)
  
  Coeff.temp <- Data.matrix.intra/rowSums(Data.matrix.intra)
  Coeff.temp[is.nan(Coeff.temp)] <- 0 # for zero call arrivals 
  
  Matrix.inter.coeff <- matrix(ncol = 24*60/as.integer(Interval), nrow = 7)
  
  for (i in 1:7){ # the intra day coeff of each weekday
    Matrix.inter.coeff[(8-i),] <- apply(Coeff.temp[seq(nrow(Coeff.temp)-i+1, 1, by = -7), ],
                                        MARGIN = 2,
                                        FUN = mean)
  }
  
  
  Results.intra <- Matrix.inter.coeff[rep(c(1:7), length = length(Results)),] *
    t(matrix(rep(Results, each = 24*60/as.integer(Interval)), nrow = 24*60/as.integer(Interval)))
  
  
  return(Results.intra)
}