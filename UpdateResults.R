UpdateResults <- function(Results, AbnormalResults, ExceptionalDayandEffects, StartDate, FinishDate, Interval){
  ###### Outputs: 
  #### Results.matrix = lg*60*24 numeric data
  
  ###### Inputs:  
  #### Results = lg*60*24 numeric data
  #### AbnormalResults = matrix of Ind X 60*24/as.integer(Interval)
  #### ExceptionalDayandEffects = list(ExceptionalDays, ProximityDays)
  #### StartDate, FinishDate, Interval = character
  
  
  Results.matrix <- matrix(Results, nrow = 24*60/as.integer(Interval))
  colnames(Results.matrix) <- as.character(seq(as.Date(StartDate), as.Date(FinishDate), by = "1 day"))
  
  temp.exp1 <- ExceptionalDayandEffects[[1]]
  temp.exp1[,1] <- as.character(temp.exp1[,1])
  colnames(temp.exp1) <- c("Date", "Annual", "TypeID")
  temp.exp2 <- ExceptionalDayandEffects[[2]]
  temp.exp2[,1] <- as.character(temp.exp2[,1])
  colnames(temp.exp2) <- c("Date", "Annual", "TypeID")
  
  temp.exp12 <- rbind(temp.exp1, temp.exp2)
  
  AbnormalDays <- temp.exp12[which((as.Date(temp.exp12[,1]) >= as.Date(StartDate))
                                   &(as.Date(temp.exp12[,1]) <= as.Date(FinishDate))),
                             c(1,3)]
  
  for (m in 1:length(AbnormalDays[,1])) {
    Results.matrix[, AbnormalDays[m,1]] <- AbnormalResults[, AbnormalDays[m,2]]
  }
  
  FinialResults <- as.vector(Results.matrix)
  
  return(FinialResults)
}