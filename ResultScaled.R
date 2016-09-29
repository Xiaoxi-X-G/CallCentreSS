ResultScaled <-function(Results.finial, OpenDayTime, StartDate, FinishDate, Interval){
  ### Results are updated based on opening hour, to be cut-off if falls outside, and 
  # scale the rest to keep daily prediction same
  
  ### Inputs
  ### Results.finial = vector of ((FinishDate - StartDate) + 1 ) * 24 * 60 / Interval
  ### OpenDayTime = dataframe(Dates, OpenFrom, OpenTo)
  
  ### Output =  vector of ((FinishDate - StartDate) + 1 ) * 24 * 60 / Interval
  
  Results.finial.matrix <- t(matrix(Results.finial, nrow = 24*60/as.integer(Interval)))
  colnames(Results.finial.matrix) <- format(seq(as.POSIXct(paste(StartDate, "00:00:00"), origin="1970-01-01", tz="GMT"),
                                                by = paste(Interval, "mins"),
                                                length.out = 60*24/as.integer(Interval)), "%H:%M:%S")
  
  rownames(Results.finial.matrix) <- as.character(seq(as.Date(StartDate), as.Date(FinishDate), by = "1 day"))
  Results.scaled.matrix <- matrix(rep(0, length=length(Results.finial)), 
                                  ncol = 24*60/as.integer(Interval))
  for (d in 1:length(OpenDayTime$Dates)){
    if ((OpenDayTime[d, 2] == "00:00:00") & (OpenDayTime[d, 3] == "00:00:00")){ # close days
      next
    }else{ # otherwise
      Ind <- which( chron(times = colnames(Results.finial.matrix)) >= chron(times = OpenDayTime[d,2])
                    & chron(times = colnames(Results.finial.matrix)) <= chron(times = OpenDayTime[d,3]))
      Results.scaled.matrix[d, Ind] <- (sum(Results.finial.matrix[d,]) / sum(Results.finial.matrix[d, Ind]))*Results.finial.matrix[d, Ind]
    }
  }
  
  
  # Results.scaled.finial <- as.vector(t(Results.scaled.matrix))
  return(as.vector(t(Results.scaled.matrix)))
  
}