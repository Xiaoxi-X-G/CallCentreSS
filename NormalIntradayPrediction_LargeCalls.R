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
  lg <- Days.testing
  Input.data.hourly <- aggregate(Data.training[,2], list(DateTime = cut(as.POSIXct(Data.training[,1], 
                                                                                   origin="1970-01-01", 
                                                                                   tz="GMT"),
                                                                        breaks="hour")), 
                                 FUN=sum)
  
  colnames(Input.data.hourly) <- c("DateTime","Items")
  Lambda <- BoxCox.lambda(Input.data.hourly$Items)
  Input.data.hourly$BoxCox <- BoxCox(Input.data.hourly$Items, Lambda)
  Input.data.hourly$log <- log(Input.data.hourly$BoxCox + abs(min(Input.data.hourly$BoxCox)) + 1)
  
  ### 2. Intraday prediction
  Fit <- tryCatch(
    {
      Seasonal1 <- 24 #Distributed smaller interval later, 60*24/as.integer(Interval)
      Seasonal2 <- 7*Seasonal1
      Data.msts <- msts(Input.data.hourly$log, seasonal.periods = c(Seasonal1, Seasonal2))
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
      Data.ts <- ts(Input.data.hourly$log, frequency = 7*24*60/as.integer(Interval))
      Fit <- ets(Data.ts)
      return(Fit)
    }
  )
  
  Results.temp <- forecast(Fit, h =lg*24)
  
  ### 3. Inverse BoxCox
  
  Results.hourly <- InvBoxCox(exp(as.numeric(Results.temp$mean))-(1+abs(min(Input.data.hourly$BoxCox))), Lambda)
  Results.hourly[which(Results.hourly < 0)] <- 0 
  
  
  
  
  ### 4. Distributed to each intraday interval ####
  if (as.integer(Interval)==60){
    Results.output <- Results.hourly
  }else{
    Days.training <- nrow(Data.training)/(24*60/as.integer(Interval))
    
    cols <- 60/as.integer(Interval)
    Data.matrix.intra3d <- array(0, 
                                 dim = c(60/as.integer(Interval)*24/cols, cols, Days.training),
                                 dimnames = list(seq(0, 23, by = 1)))
    Data.matrix.intra <- t(matrix(as.numeric(Data.training$Items), nrow  = 24*60/as.integer(Interval)))
    
    Coeff.temp3d <- array(0, 
                          dim = c(60/as.integer(Interval)*24/cols, cols, Days.training),
                          dimnames = list(seq(0, 23, by = 1)))
    
    for (m in 1:Days.training){
      Data.matrix.intra3d[,,m] <- t(matrix(Data.matrix.intra[m,], nrow = cols))
      Coeff.temp3d[,,m] <- Data.matrix.intra3d[,,m]/rowSums(Data.matrix.intra3d[,,m])
    }
    Coeff.temp3d[is.nan(Coeff.temp3d)] <- 0 
    
    
    Matrix.inter.coeff3d <- array(0, 
                                  dim = c(60/as.integer(Interval)*24/cols, cols, 7),
                                  dimnames = list(seq(0, 23, by = 1)))
    # Matrix.inter.coeff3d[,,1]<- next weekdays
    # .
    # .
    # .
    # Matrix.inter.coeff3d[,,1]<- Today's date
    
    for (i in 1:7){ # the intra day coeff of each weekday, Assuming continuous of the data
      Matrix.inter.coeff3d[,,(8-i)] <- apply(Coeff.temp3d[,,seq(dim(Matrix.inter.coeff3d)[3]-i+1, 1, by = -7) ],
                                             MARGIN = c(1,2),
                                             FUN = mean)
    }
    
    Matrix.inter.coeff3d.all <- Matrix.inter.coeff3d[,,rep(c(1:7), length = lg)]
    Results.output <- matrix(0, nrow = lg, ncol = 24*60/as.integer(Interval))
    
    for (p in 1:lg){
      Results.output[p,] <- as.vector(t(Matrix.inter.coeff3d.all[,,p] * 
                                          Results.hourly[((p-1)*24+1):(p*24)]))
    }
    
  }
  
  return(Results.output)
}
