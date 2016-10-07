Imputation <- function(Data.training, Interval){
  #### Inputs:
  # Data.training = data.frame, DateTime & Items
  # Interval = character <= 60
  
  #### Output = Imputated Data.training  
  
  ########################################################  
  #1. Rearrange the Data.traning according to Peroid
  #2. Find outliers and replaced by NA
  #3. Imputate NA with 2nd order polynormial
  #4. Convert data back to Data.training format
  #########################################################
  
  Period <-  7*24*60/as.integer(Interval) # assume repeat every Period points
  Rearranged.df <- data.frame(Wk=c(), TimeDayOfWeek=c(), Items=c())
  
  TimeLine <- c()
  for (n in 1:Period){ 
    ToCheck <- Data.training[seq(n, nrow(Data.training), by =Period ), ]
    ToCheck$DayOfWk <- weekdays(as.POSIXct(ToCheck$DateTime, origin="1970-01-01", tz="GMT"))
    temp.dataframe <- 
      data.frame(Wk=seq(1, nrow(ToCheck), by =1),
                 TimeDayofWeek = paste(format(as.POSIXct(ToCheck$DateTime, origin="1970-01-01", tz="GMT"), "%H:%M:%S"), 
                                       ToCheck$DayOfWk, sep = ","),
                 Items = Outliers(ToCheck$Items))
    
    Rearranged.df <- rbind(Rearranged.df, temp.dataframe)
    TimeLine <- c(TimeLine, ToCheck$DateTime)
  }
  
  ImputeData.temp <- 
    amelia(Rearranged.df, m=1, ts="Wk", cs = "TimeDayofWeek", 
           polytime = 2, intercs = T,
           bounds = matrix(c(3, 0, max(Data.training$Items)), nrow=1, ncol = 3))
  
  
  Ind <- order(as.POSIXct(TimeLine, origin="1970-01-01", tz="GMT"))
  ImputeData <- data.frame(DateTime=TimeLine[Ind], Items = ImputeData.temp$imputations[[1]]$Items[Ind]) 
  
  
  return(ImputeData)
}