rm(list = ls())

RScriptPath<-"C:/Users/ptech3/Dropbox/Ploytech/CallCenter/StateSpace"

source(paste(RScriptPath, "/FormatTS.R", sep=""))
source(paste(RScriptPath, "/NormalIntradayPrediction_LowCalls.R", sep="")) 
source(paste(RScriptPath, "/NormalIntradayPrediction_LargeCalls.R", sep="")) 
source(paste(RScriptPath, "/ExceptionalDayandEffectFormat.R", sep="")) 
source(paste(RScriptPath, "/ExponentialCoeff.R", sep="")) 
source(paste(RScriptPath, "/UpdateResults.R", sep="")) 
source(paste(RScriptPath, "/AbnormalPred.R", sep="")) 
source(paste(RScriptPath, "/ResultScaled.R", sep=""))




library(RODBC)
library(forecast)
require(MASS)

######### link to DB, pull one queue
odbcDataSources()

conn<-odbcConnect("localdb") #
DataAll.temp <- sqlQuery(conn, "SELECT [CellTime], [Tot_num_incoming], [Tot_time_incoming] FROM [CallCenter].[dbo].[CallCenter_DataRaw] where QueueID = 'Public_Incident';", as.is = T)
odbcClose(conn)

######### Clean data
DataAll <- DataAll.temp[,c(1,2)]
DataAll[,1] <- as.POSIXct(DataAll[,1], origin = "1970-01-01", tz="GMT")
DataAll<-DataAll[order(DataAll[,1]),]

Format.FirstDate <- as.character(as.Date(DataAll$CellTime[1]))
Format.LastDate <- as.character(as.Date(tail(DataAll$CellTime, n=1)))
Interval <- "60"

DataAllClean <- FormatTS(DataAll, Format.FirstDate, Format.LastDate, Interval)
DataAllClean$Items <- as.numeric(DataAllClean$Items) 





# ######### Check Intra and Inter day correlation ####
# require(corrgram) 
# require(corrplot) 
# 
# ### Inter day
# Corrgram.data.inter.temp <- DataAllClean
# Corrgram.data.inter.temp[,1] <- as.POSIXct(Corrgram.data.inter.temp[,1], origin = "1970-01-01", tz="GMT")
# 
# Corrgram.data.inter <- tapply(as.integer(Corrgram.data.inter.temp$Items),
#                               as.factor(format(Corrgram.data.inter.temp$DateTime, "%Y-%m-%d")), sum)
# 
# Corrgram.data.inter <- aggregate(as.integer(Corrgram.data.inter.temp$Items),
#           list(Date=format(Corrgram.data.inter.temp$DateTime, "%Y-%m-%d")),
#           FUN=sum)
# colnames(Corrgram.data.inter)[2] <- "Value"
# 
# Corrgram.data.inter$WkDay <- weekdays(as.Date(Corrgram.data.inter$Date))
# #Corrgram.data.inter$WkDay <- as.numeric(as.factor(Corrgram.data.inter$WkDay) )
# 
# 
# boxplot(Corrgram.data.inter$Value ~ Corrgram.data.inter$WkDay)
# 
# Corrgram.matrix.inter <- t(matrix(Corrgram.data.inter$Value[c(1:(7*floor(nrow(Corrgram.data.inter)/7)))], nrow  = 7))
# colnames(Corrgram.matrix.inter) <- head(Corrgram.data.inter$WkDay,n=7)
# 
# 
# # m1
# corrgram(Corrgram.matrix.inter, order = F,
#          upper.panel = panel.pie,
#          lower.panel = panel.conf)
# 
# 
# # m2
# Correlation.inter <- cor(Corrgram.matrix.inter)
# corrplot(Correlation.inter, order = "hclust")
# 
# 
# ### Intra
# Corrgram.data.intra.temp <- DataAllClean
# Corrgram.matrix.intra <- t(matrix(as.numeric(Corrgram.data.intra.temp$Items), nrow  = 24*60/as.integer(Interval)))
# 
# colnames(Corrgram.matrix.intra) <- seq(from =0, by=as.integer(Interval)/60,
#                                        length = 60/as.integer(Interval)*24)
# # m1
# corrgram(Corrgram.matrix.intra, order = FALSE,
#          upper.panel = panel.pie,
#          lower.panel = panel.conf,
#          cor.method="pearson")
# 
# 
# # m2
# Correlation.intra <- cor(Corrgram.matrix.intra)
# corrplot(Correlation.intra, order = "hclust")


#### Segment for training and testing ####
Training.End <- "2012-08-21"
  
Days.training <- 12*7
Days.testing <- 7*2-1
Data.training <- DataAllClean[which((as.Date(DataAllClean$DateTime)>= (as.Date(Training.End)- Days.training+1 ))
                            & (as.Date(DataAllClean$DateTime)<= as.Date(Training.End))),]

Data.testing <- DataAllClean[which((as.Date(DataAllClean$DateTime)>= (as.Date(Training.End) + 1 ))
                                    & (as.Date(DataAllClean$DateTime)<= (as.Date(Training.End) + Days.testing))),]

plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue", 
     ylim=c(0, max(Data.training$Items)),
     main = "Training + Testing")
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), 
      type = "o", pch = 22, lty = 2, col = "red")



StartDate <- format(as.POSIXct(Data.testing[1,1], origin = "1970-01-01", tz="GMT") , "%Y-%m-%d")
FinishDate <- format(as.POSIXct(Data.testing[nrow(Data.testing),1], origin = "1970-01-01", tz="GMT") , "%Y-%m-%d")

FirstDate <- format(as.POSIXct(Data.training[1,1], origin = "1970-01-01", tz="GMT") , "%Y-%m-%d")
LastDate <- format(as.POSIXct(Data.training[nrow(Data.training),1], origin = "1970-01-01", tz="GMT") , "%Y-%m-%d")  






###########################################################################
###### Exponential Day forecast ###### 
ExceptionalDatesCSV <- read.csv("ExceptionalDatesRight.csv") 
ExceptionalDayandEffects <- ExceptionalDayandEffectFormat(ExceptionalDatesCSV, Format.FirstDate, FinishDate)


# Check if abnormalday forecasting is requried
if (length(which((ExceptionalDayandEffects[[2]]$Dates>=as.Date(StartDate))
                 &(ExceptionalDayandEffects[[2]]$Dates<=as.Date(FinishDate)))) == 0){
  AbnormalResults <- c()
}else{
  AbnormalResults.temp1 <- AbnormalPred(DataAllClean, AbnormalInfo=ExceptionalDayandEffects[[1]], 
                                        Interval, Format.FirstDate, LastDate, StartDate, FinishDate)
  AbnormalResults.temp2 <- AbnormalPred(DataAllClean, AbnormalInfo=ExceptionalDayandEffects[[2]], 
                                        Interval, Format.FirstDate, LastDate, StartDate, FinishDate)
  AbnormalResults <- cbind(AbnormalResults.temp1, AbnormalResults.temp2)
}



######################################################################
#### Check intreday correlation ####
Data.training.daily.temp <- Data.training
Data.training.daily.temp[,1] <- as.POSIXct(Data.training.daily.temp[,1], origin = "1970-01-01", tz="GMT")

Data.training.daily <- aggregate(as.integer(Data.training.daily.temp$Items),
                                 list(Date=format(Data.training.daily.temp$DateTime, "%Y-%m-%d")),
                                 FUN=sum)
colnames(Data.training.daily)[2] <- "Value"


Data.training.daily$Wk <- weekdays(as.Date(Data.training.daily$Date))
## Check zeros

###########
require(Amelia)

Outliers <- function(x){
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = T)
  H <- 1.5*IQR(x, na.rm = T)
  y <- x
  y[x < (qnt[1]-H)] <- NA
  y[x > (qnt[2]+H)] <- NA
  return(y)
}

#### Training data preprocessing ####
#Data.training$Items[sample(1:nrow(Data.training),100, replace = T)] <- 0 # random 0

Data.training$Items[500:600] <- NA # burst NA
Period <-  7*24*60/as.integer(Interval) # assume repeat every Period points
Rearranged.df <- data.frame(Wk=c(), TimeDayOfWeek=c(), Items=c())

TimeLine <- c()
for (n in 1:Period){ 
  ToCheck <- Data.training[seq(n, nrow(Data.training), by =Period ), ]
  ToCheck$DayOfWk <- weekdays(as.POSIXct(ToCheck$DateTime, origin="1970-01-01", tz="GMT"))
  temp.dataframe <- data.frame(Wk=seq(1, nrow(ToCheck), by =1),
                               TimeDayofWeek = paste(format(as.POSIXct(ToCheck$DateTime, origin="1970-01-01", tz="GMT"), "%H:%M:%S"), ToCheck$DayOfWk, sep = ","),
                               Items = Outliers(ToCheck$Items))
  Rearranged.df <- rbind(Rearranged.df, temp.dataframe)
  TimeLine <- c(TimeLine, ToCheck$DateTime)
}

ImputeData.temp <- amelia(Rearranged.df, m=1, ts="Wk", cs = "TimeDayofWeek", 
                           polytime = 2, intercs = T,
                          bounds = matrix(c(3, 0, max(Data.training$Items)), nrow=1, ncol = 3))

#tscsPlot(ImputeData.temp, cs = "13:00:00,Wednesday", var = "Items")

Ind <- order(as.POSIXct(TimeLine, origin="1970-01-01", tz="GMT"))
ImputeData <- data.frame(DateTime=TimeLine[Ind], Items = ImputeData.temp$imputations[[1]]$Items[Ind]) 

plot(Data.training$Items[100:800],  type ="o", col= "blue")
lines(ImputeData$Items[100:800], type = "o", pch = 22, lty = 2, col = "red")





      
      

######## Check var and exp ####
Data.training$Wk <- weekdays(as.Date(Data.training$Date))
Data.training.matrix <- t(matrix(Data.training$Items[which(Data.training$Wk == "Friday")], 
                                 nrow = 24*60/as.integer(Interval)))
sqrt(apply(Data.training.matrix, MARGIN = 2, var)) 
apply(Data.training.matrix, MARGIN = 2, mean)

sqrt(apply(Data.training.matrix, MARGIN = 2, var)) / apply(Data.training.matrix, MARGIN = 2, mean)


#####################################################################
###### Intraday forecast  ########
if (mean(Data.training[,2], na.rm = T) < 25){ #need to be normalized
  Results <- as.vector(t(NormalIntradayPrediction_LowCalls(Data.training, Days.testing, Interval)))
}else{
  Results <- as.vector(t(NormalIntradayPrediction_LargeCalls(Data.training, Days.testing, Interval)))
}



plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue",  ylim=c(0, max(Data.training$Items)), cex.axis=1.5)
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), type = "o", pch = 22, lty = 2, col = "red")
lines(c(rep(0, length= nrow(Data.training)), Results), type = "o", pch = 22,  col = "green")

plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results), type = "o", pch = 22,  col = "green")


##############################################
####### Replace by Abnormal Results if required ##########
if(length(AbnormalResults)>0){
  Results.finial <- UpdateResults(Results, AbnormalResults, ExceptionalDayandEffects, StartDate, FinishDate, Interval)
}else{
  Results.finial <- Results
}




#########################################
##### Trim and Scale result based on OpeningHours ####
source(paste(RScriptPath, "/OpenCloseDayTime.R", sep=""))
source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
DatabaseName<-"Time2Work_EZCorp"
LocationID <- 9
## Note: use Ezcorp database, assuming exist the same format
require(chron)
OpenDayTime <- OpenCloseDayTime(StartDate, FinishDate, LocationID,RScriptPath, DatabaseName)

Results.scaled.finial <- ResultScaled(Results.finial, OpenDayTime, StartDate, FinishDate, Interval)




###### Format output #####
Results.finial.format <- 
  data.frame(DateTime = seq(as.POSIXct(paste(StartDate, "00:00:00"), origin="1970-01-01", tz="GMT"),
                            by = paste(Interval, "mins"),
                            length.out = length(Results.scaled.finial)),
             Value = Results.scaled.finial,
             stringsAsFactors = F)





####### Plot #######

plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue",  ylim=c(0, max(Data.training$Items)), cex.axis=1.5)
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), type = "o", pch = 22, lty = 2, col = "red")
lines(c(rep(0, length= nrow(Data.training)), Results.finial.format[,2]), type = "o", pch = 22,  col = "green")


plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results.finial.format[,2]), type = "o", pch = 22,  col = "green")


#################################################################  
######## Residual check - per hours ######
RMSE <- sqrt(mean((Data.testing$Items-Results)^2, na.rm =T))
Data.testing$Pred <- Results

Residual <- as.numeric(Data.testing$Pred - Data.testing$Items)
Data.testing$Residual <- Residual
plot(Residual)
mean(Residual)
hist(Residual)
acf(Residual)
qqnorm(Residual)

# mean(1-abs(Data.testing$Pred - Data.testing$Items)/(Data.testing$Pred), rm.na=T)
temp11 <- 1 - abs(Data.testing$Pred - Data.testing$Items)/(Data.testing$Pred)
mean(temp11[is.finite(temp11)])

### Daily error
DailyResult <- aggregate(Data.testing[,c(2,3)],  
                list(format(as.POSIXct(Data.testing$DateTime, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")), 
                 sum)

colnames(DailyResult)[1] <- "Date"

DailyResult$Residual <- DailyResult$Items - DailyResult$Pred

RMSE.daily <- sqrt(mean((DailyResult$Items - DailyResult$Pred)^2, na.rm = T))

mean(1 - abs(DailyResult$Residual)/ (DailyResult$Items))
acf(DailyResult$Residual)


