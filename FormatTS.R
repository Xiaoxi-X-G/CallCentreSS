FormatTS <- function(DataAll, FirstDate, LastDate, Interval){
  ### Format the time series DataAll with fixed Interval from FirstDate to LastDate
  ### DataAll = dataframe(DateTime, Value)  
  ### FirstDate, LastDate = Date 
  ### Interval = int & (>=2) & (  residual(60/Interval) == 0 )      
  
  DataAll[,1] <- as.POSIXct(DataAll[,1], origin = "1970-01-01", tz="GMT")
  DataAll<-DataAll[order(DataAll[,1]),]
  
  if (format(DataAll[1,1]) != "00:00" ){ # Force DataAll start with n:00:00 to solve cut() issues
    Insert.row <- data.frame(CellTime =  as.POSIXct(paste(format(DataAll[1,1], "%Y-%m-%d"), paste(format(DataAll[1,1], "%H"), ":00:00", sep=""))
                                                   , origin = "1970-01-01", tz="GMT"),
                            Tot_num_incoming = 0,
                            stringsAsFactors = F)
    
    DataAll <- rbind(Insert.row, DataAll)
  }
  
  ### Middle
  DataAllClean.temp <- data.frame(table(cut(DataAll[,1], breaks="min")))
  DataAllClean.temp[which(DataAllClean.temp[,2]==1),2] <- DataAll[,2]
  DataAllClean.temp[,1] <- as.POSIXct(DataAllClean.temp[,1], origin = "1970-01-01", tz="GMT")
  
  DataAllClean <- aggregate(DataAllClean.temp[, 2], list(DateTime=cut(DataAllClean.temp[,1], breaks = paste(Interval, "mins"))), FUN=sum )
  DataAllClean$x <- as.character(DataAllClean$x)
  
  ### For head and tails
  temp1 <- as.POSIXct(c(paste(FirstDate, "00:00:00"), as.character(DataAllClean[1,1])), origin = "1970-01-01", tz="GMT")
  Head.temp <- data.frame(table(cut(temp1, breaks= paste(Interval, "mins"))))
  colnames(Head.temp) <- c("DateTime", "x")
  Head <- Head.temp[-nrow(Head.temp),]
  Head$DateTime <- as.character(Head$DateTime)
  Head[1,2] <- 0
  
  temp2 <- as.POSIXct(c(as.character(DataAllClean[nrow(DataAllClean),1]), paste(as.Date(LastDate) + 1, "00:00:00")), origin = "1970-01-01", tz="GMT")
  tail.temp <- data.frame(table(cut(temp2, breaks= paste(Interval, "mins"))))
  colnames(tail.temp) <- c("DateTime", "x")
  Tail <- tail.temp[-which(tail.temp$x==1),]
  Tail$DateTime <- as.character(Tail$DateTime)
  
  DataAllCleanOutput <- rbind.data.frame(Head, DataAllClean, Tail, stringsAsFactors = F)
  colnames(DataAllCleanOutput)[2] <- "Items"
  
  return(DataAllCleanOutput)
}