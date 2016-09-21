rm(list = ls())

RScriptPath<-"C:/Users/ptech3/Dropbox/Ploytech/CallCenter/StateSpace"

source(paste(RScriptPath, "/FormatTS.R", sep=""))
source(paste(RScriptPath, "/NormalIntradayPrediction_LowCalls.R", sep="")) 
source(paste(RScriptPath, "/NormalIntradayPrediction_LargeCalls.R", sep="")) 



library(RODBC)
library(forecast)
require(MASS)

######### link to DB, pull one queue
odbcDataSources()

conn<-odbcConnect("localdb") #
DataAll <- sqlQuery(conn, "SELECT [CellTime], [Tot_num_incoming] FROM [CallCenter].[dbo].[CallCenter_DataRaw] where QueueID = 'Public_Incident';", as.is = T)
odbcClose(conn)

######### Clean data
DataAll[,1] <- as.POSIXct(DataAll[,1], origin = "1970-01-01", tz="GMT")
DataAll<-DataAll[order(DataAll[,1]),]

FirstDate <- as.character(as.Date(DataAll$CellTime[1]))
LastDate <- as.character(as.Date(tail(DataAll$CellTime, n=1)))
Interval <- "30"

DataAllClean <- FormatTS(DataAll, FirstDate, LastDate, Interval)
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
Training.End <- "2012-06-21"
  
Days.training <- 6*7-1
Days.testing <- 2*7 -3
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


#### Check intreday correlation
Data.training.daily.temp <- Data.training
Data.training.daily.temp[,1] <- as.POSIXct(Data.training.daily.temp[,1], origin = "1970-01-01", tz="GMT")

Data.training.daily <- aggregate(as.integer(Data.training.daily.temp$Items),
                                 list(Date=format(Data.training.daily.temp$DateTime, "%Y-%m-%d")),
                                 FUN=sum)
colnames(Data.training.daily)[2] <- "Value"



if (mean(Data.training.daily$Value, na.rm = T) < 100){
  Results <- as.vector(t(NormalIntradayPrediction_LowCalls(Data.training, Days.testing, Interval)))
}else{
  Results <- NormalIntradayPrediction_LargeCalls(Data.training, Days.testing, Interval)
}



plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue",  ylim=c(0, max(Data.training$Items)), cex.axis=1.5)
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), type = "o", pch = 22, lty = 2, col = "red")
lines(c(rep(0, length= nrow(Data.training)), Results), type = "o", pch = 22,  col = "green")


plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results), type = "o", pch = 22,  col = "green")


  
######## Residual check - per hours
RMSE <- sqrt(mean((Data.testing$Items-Results)^2, na.rm =T))
Data.testing$Pred <- Results

Residual <- as.numeric(Data.testing$Pred - Data.testing$Items)
plot(Residual)
mean(Residual)
hist(Residual)
acf(Residual)
qqnorm(Residual)

mean(1-abs(Data.testing$Pred - Data.testing$Items)/max(Data.testing$Items, rm.na=T), rm.na=T)


### Daily error
DailyResult <- aggregate(Data.testing[,c(2,3)], 
                list(format(as.POSIXct(Data.testing$DateTime, origin = "1970-01-01", tz = "GMT"), "%Y-%m-%d")), 
                 sum)

colnames(DailyResult)[1] <- "Date"

DailyResult$Residual <- DailyResult$Items - DailyResult$Pred
mean(1 - abs(DailyResult$Residual)/ max(DailyResult$Items))

