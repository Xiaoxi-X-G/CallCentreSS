AbnormalPred <- function(DataAllClean, AbnormalInfo, Interval, Format.FirstDate, LastDate, StartDate, FinishDate){
#### Output:
  #### AbnormalResults = matrix of Ind X 60*24/as.integer(Interval)
  
  
##### Inputs:
  #### DataAllClean = data.frame(DataTime, Items)
  #### AbnormalInfo = data.frame(Dates, Annual, ....TypeID)
  #### Interval = Breakdown interval, character
  #### Format.FirstDate = First day of all the formated historical data, character
  #### LastDay = Last day of the Training data
  
  
  #######################################################################
  ##### Find Targeted Ind
  ##### Use TypeID to group data and then apply weighted smoothing to forecast
  #######################################################################
  
  
  Ind <- unique(AbnormalInfo[which((AbnormalInfo[,1] >= as.Date(StartDate))
                                   &(AbnormalInfo[,1] <= as.Date(FinishDate))), 
                             3])
  
  ## Initialize a matrix to store abnormal forecast results
  AbnormalResults <- matrix(ncol = length(Ind), nrow = 60*24/as.integer(Interval))
  colnames(AbnormalResults) <- Ind
  if (length(Ind) > 0){
    for (i in Ind){
      AbnormalDate.temp <- AbnormalInfo[which(AbnormalInfo[,3] == i),1]
      AbnormalDate <- AbnormalDate.temp[which((AbnormalDate.temp >= Format.FirstDate)
                                              & (AbnormalDate.temp <= LastDate))]
      ## Initialize a matrix to store abnormal data
      AbnormalHistory <- matrix(ncol=length(AbnormalDate), nrow = 60*24/as.integer(Interval))
      
      if (length(AbnormalDate) > 0){
        for (j in (1:length(AbnormalDate))){
          AbnormalHistory[, j] <- 
            DataAllClean[which(format(as.POSIXct(DataAllClean[,1], origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d") 
                               == AbnormalDate[j] ), 2]
        }
        colnames(AbnormalHistory) <- as.character(AbnormalDate)
        
        ## compute Abnormal results 
        AbnormalResults[,i] <- AbnormalHistory %*% ExponentialCoeff(length(AbnormalDate),0.6)
      }
    }
  }
  
  return(AbnormalResults)
}