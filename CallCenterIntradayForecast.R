rm(list = ls())

## Rscript path
RScriptPath<-"C:/Users/ptech3/Dropbox/Ploytech/CallCenter/StateSpace"


## Load libraries and functions 
require(RODBC)
require(forecast)
require(MASS)
require(Amelia)
require(chron)


source(paste(RScriptPath, "/FormatTS.R", sep=""))
source(paste(RScriptPath, "/NormalIntradayPrediction_LowCalls.R", sep="")) 
source(paste(RScriptPath, "/NormalIntradayPrediction_LargeCalls.R", sep="")) 
source(paste(RScriptPath, "/ExceptionalDayandEffectFormat.R", sep="")) 
source(paste(RScriptPath, "/ExponentialCoeff.R", sep="")) 
source(paste(RScriptPath, "/UpdateResults.R", sep="")) 
source(paste(RScriptPath, "/AbnormalPred.R", sep="")) 
source(paste(RScriptPath, "/ResultScaled.R", sep=""))
source(paste(RScriptPath, "/Outliers.R", sep=""))
source(paste(RScriptPath, "/Imputation.R", sep=""))

########
##0. Pull Historical data from database
##1. Set the starting date for forecasting and forecasting period
##2. Format the data with fixed Interval, First Date ~ Finish Date
##3. Segment data, 12 week for the training and rest for testing
##4. Pull Exceptional Day and Opening Hour data
##5. Data preprocessing ---> moved to Intraday prediction
##6. Intraday prediction
##7. Exceptional Day forecasting and Update forecasting results
##8. Scale and update results according Opening HOurs
##9. Compare with testing data and plot
########



########################################################################
#### 0. Pull Historical data from database#######
odbcDataSources()
conn<-odbcConnect("localdb") #
DataAll.temp <- sqlQuery(conn, "SELECT [CellTime], [Tot_num_incoming], [Tot_time_incoming] FROM [CallCenter].[dbo].[CallCenter_DataRaw] where QueueID = 'Public_Incident';", as.is = T)
odbcClose(conn)

DataAll <- DataAll.temp[,c(1,2)]
DataAll[,1] <- as.POSIXct(DataAll[,1], origin = "1970-01-01", tz="GMT")
DataAll<-DataAll[order(DataAll[,1]),]
colnames(DataAll) <- c("DateTime", "Items") 
##############################################################################

#### 1. Set the starting date for forecasting and forecasting period ####
Training.End <- "2012-01-10"
Days.training <- 12*7
Days.testing <- 7*1-1
##############################################################################

#### 2. Format the data with fixed Interval#####
Format.FirstDate <- as.character(as.Date(DataAll[1,1]))
Format.LastDate <- as.character(as.Date(tail(DataAll[,1], n=1)))
Interval <- "60"

DataAllClean <- FormatTS(DataAll, Format.FirstDate, Format.LastDate, Interval)
DataAllClean$Items <- as.numeric(DataAllClean$Items) 
##############################################################################

#### 3. Segment data, 12 week for the training and rest for testing####

Data.training <- DataAllClean[which((as.Date(DataAllClean$DateTime)>= (as.Date(Training.End)- Days.training+1 ))
                                    & (as.Date(DataAllClean$DateTime)<= as.Date(Training.End))),]

Data.testing <- DataAllClean[which((as.Date(DataAllClean$DateTime)>= (as.Date(Training.End) + 1 ))
                                   & (as.Date(DataAllClean$DateTime)<= (as.Date(Training.End) + Days.testing))),]


StartDate <- as.character(as.Date(Training.End)+1)
FinishDate <- as.character(format(as.POSIXct(tail(Data.testing$DateTime, n=1), origin="1970-01-01", tz = "GMT"),
                                  "%Y-%m-%d"))
FirstDate <- as.character(format(as.POSIXct(Data.training$DateTime[1], origin = "1970-01-01", tz = "GMT"),
                                 "%Y-%m-%d"))
LastDate <- Training.End


plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue", 
     ylim=c(0, max(Data.training$Items)),
     main = "Training + Testing")
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), 
      type = "o", pch = 22, lty = 2, col = "red")
##############################################################################

#### 4. Pull Exceptional Day and Opening Hour data#####
ExceptionalDatesCSV <- read.csv("ExceptionalDatesRight.csv") 
ExceptionalDayandEffects <- ExceptionalDayandEffectFormat(ExceptionalDatesCSV, Format.FirstDate, FinishDate)

source(paste(RScriptPath, "/OpenCloseDayTime.R", sep=""))
source(paste(RScriptPath, "/TranslateDayofWeek.R", sep=""))
DatabaseName<-"Time2Work_EZCorp"
LocationID <- 9
OpenDayTime <- OpenCloseDayTime(StartDate, FinishDate, LocationID, RScriptPath, DatabaseName)
##############################################################################

# #### 5. Data preprocessing######
# # Data.training$Items[500:600] <- NA # testing burst NA
# # Data.training$Items[sample(1:nrow(Data.training), 100)] <- NA
# 
# Data.training.imputated <- Imputation(Data.training, Interval)
# 
# Data.training$Items[is.na(Data.training$Items)] <-0
# plot(Data.training$Items[900:2000],  type ="o", col= "blue")
# lines(Data.training.imputated$Items[900:2000], type = "o", pch = 22, lty = 2, col = "red")
# 
# plot(Data.training.imputated$Items[400:1200],  type ="o", col= "red")


Period <-  7*24*60/as.integer(Interval) # assume repeat every Period points
Rearranged.matrix <- matrix( ncol = Period, nrow=ceiling(nrow(Data.training)/Period))

for (n in 1:Period){ 
  ToCheck <- Data.training[seq(n, nrow(Data.training), by =Period ), ]
  Rearranged.matrix[1:nrow(ToCheck), n] <- ToCheck$Items
}

Poisson.CK <- mean(apply(Rearranged.matrix, 
                         MARGIN = 2, 
                         FUN = function(x){var(x, na.rm = T)<=1.2*mean(x,na.rm = T)}),
                   na.rm = T)


##############################################################################

#### 6. Intraday prediction######

if (Poisson.CK < 0.7){ # more than 70% of the data is not poisson distributed
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
##############################################################################

##### 7. Exceptional Day forecasting and Update forecasting results####

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


# Update forecasting results if requried
if(length(AbnormalResults)>0){
  Results.finial <- UpdateResults(Results, AbnormalResults, ExceptionalDayandEffects, StartDate, FinishDate, Interval)
}else{
  Results.finial <- Results
}

plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results.finial), type = "o", pch = 22,  col = "green")
##############################################################################

##### 8. Scale and update results according Opening HOurs#####
Results.scaled.finial <- ResultScaled(Results.finial, OpenDayTime, StartDate, FinishDate, Interval)

 ##Format output
Results.finial.format <- 
  data.frame(DateTime = seq(as.POSIXct(paste(StartDate, "00:00:00"), origin="1970-01-01", tz="GMT"),
                            by = paste(Interval, "mins"),
                            length.out = length(Results.scaled.finial)),
             Items = Results.scaled.finial,
             stringsAsFactors = F)

plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results.finial.format$Items), type = "o", pch = 22,  col = "green")
##############################################################################

##### 9. Compare with testing data and plot#####
 #### Residual check - per hours
RMSE <- sqrt(mean((Data.testing$Items-Results)^2, na.rm =T))
Data.testing$Pred <- Results

Residual <- as.numeric(Data.testing$Pred - Data.testing$Items)
Data.testing$Residual <- Residual
plot(Residual)
mean(Residual)
hist(Residual)
acf(Residual)
qqnorm(Residual)

temp11 <- 1 - abs(Data.testing$Pred - Data.testing$Items)/(Data.testing$Pred)
mean(temp11[is.finite(temp11)])

#### Residual check - per day
DailyResult <- aggregate(Data.testing[,c(2,3)],  
                         list(format(as.POSIXct(Data.testing$DateTime, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")), 
                         sum)
colnames(DailyResult)[1] <- "Date"
DailyResult$Residual <- DailyResult$Items - DailyResult$Pred
RMSE.daily <- sqrt(mean((DailyResult$Items - DailyResult$Pred)^2, na.rm = T))
mean(1 - abs(DailyResult$Residual)/ (DailyResult$Items))
acf(DailyResult$Residual)
##############################################################################