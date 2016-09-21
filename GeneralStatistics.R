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
Training.End <- "2012-03-21"
  
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

# Corrgram.matrix.inter <- t(matrix(Corrgram.data.inter$Value[c(1:(7*floor(nrow(Corrgram.data.inter)/7)))], nrow  = 7))
# colnames(Corrgram.matrix.inter) <- head(Corrgram.data.inter$WkDay,n=7)
# Correlation.inter <- cor(Corrgram.matrix.inter)
# 
# Avg.Correlation.inter <- (sum(abs(Correlation.inter)) - 7) /(7*7-7)
#corrplot(Correlation.inter, order = "hclust")

if (mean(Data.training.daily$Value, na.rm = T) < 100){
  Results <- as.vector(t(NormalIntradayPrediction_LowCalls(Data.training, Days.testing, Interval)))
}else{
  Results <- NormalIntradayPrediction_LargeCalls(Data.training, Days.testing, Interval)
}

  
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
  Input.data <- Data.training
  Lambda <- BoxCox.lambda(Input.data$Items)
  Input.data$BoxCox <- BoxCox(Input.data$Items, Lambda)
  
  
  ### 2. Intraday prediction
  Fit <- tryCatch(
    {
      Seasonal1 <- 60*24/as.integer(Interval)
      Seasonal2 <- 7*Seasonal1
      Data.msts <- msts(Input.data$BoxCox, seasonal.periods = c(Seasonal1, Seasonal2))
      Fit <- tbats(Data.msts, use.box.cox = F, 
                         seasonal.periods = c(Seasonal1, Seasonal2),
                         use.trend = T,  use.damped.trend= T,
                         use.arma.errors = T)
    },
    warning = function(cond){
      Data.ts <- ts(Input.data$BoxCox, frequency = 7*24*60/as.integer(Interval))
      Fit <- ets(Data.ts)
      return(Fit)
    },
    error = function(cond){
      Data.ts <- ts(Input.data$BoxCox, frequency = 7*24*60/as.integer(Interval))
      Fit <- ets(Data.ts)
      return(Fit)
    }
  )
  
  Results.temp <- forecast(Fit, h =lg*24*60/as.integer(Interval))

  ### 3. Inverse BoxCox
  Results <- InvBoxCox(as.numeric(Results.temp$mean), Lambda)
  Results[which(Results < 0)] <- 0 
  
  return(Results)
}


###############
#### preprocessing for Large Data (with strong correlation)####
# 1. BoxCox
LoessSmooth <- Data.training
Lambda <- BoxCox.lambda(LoessSmooth$Items)
LoessSmooth$BoxCox <- BoxCox(LoessSmooth$Items, Lambda)
#LoessSmooth$LogPlus1 <- log(LoessSmooth$Items+1)

hist(LoessSmooth$Items)
hist(LoessSmooth$BoxCox)
#hist(LoessSmooth$LogPlus1)


######################################### 
#### State-Space Model ####
#Input.data <- Data.training$Items
Input.data <- as.numeric(LoessSmooth$BoxCox)
Seasonal1 <- 60*24/as.integer(Interval)
Seasonal2 <- 7*Seasonal1
Data.msts <- msts(Input.data, seasonal.periods = c(Seasonal1, Seasonal2))
Fit.tbats <- tbats(Data.msts, use.box.cox = T, 
                   seasonal.periods = c(Seasonal1, Seasonal2),
                   use.trend = T,  use.damped.trend= T,
                   use.arma.errors = T)

lg <- Days.testing*24*60/as.integer(Interval)

Results.temp <- forecast(Fit.tbats, h =lg)
Results <- as.numeric(Results.temp$mean)
Results <- InvBoxCox(Results.temp$mean, Lambda)
#Results <- exp(as.numeric(Results.temp$mean)) - 1

Results[which(Results < 0 )] <- 0

# Ck <- data.frame(Results.temp$mean, Exp = exp(Results.temp$mean) - 1, Results, Data.testing$Items)

# plot(c(lo$fitted, rep(0, length= nrow(Data.testing))),
#      type ="o", col= "blue",  ylim=c(0, max(lo$fitted)), cex.axis=1.5,
#      main = 'Loess + Predicted')
# lines(c(rep(0, length= nrow(Data.training)), Results.temp$mean), type = "o", pch = 22, lty = 2, col = "red")


# plot(c(exp(lo$fitted)-1, rep(0, length= nrow(Data.testing))),
#      type ="o", col= "blue",  cex.axis=1.5,
#      main = 'Exp_Loess + Exp_Predicted')
# lines(c(rep(0, length= nrow(Data.training)), Results), 
#       type = "o", pch = 22, lty = 2, col = "red")


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


# FormatTS <- function(DataAll, FirstDate, LastDate, Interval){
# ### Format the time series DataAll with fixed Interval from FirstDate to LastDate
# ### DataAll = dataframe(DateTime, Value)  
# ### FirstDate, LastDate = Date 
# ### Interval = int & (>=2) & (  residual(24*60/Interval) == 0 )     
#   
#   DataAll[,1] <- as.POSIXct(DataAll[,1], origin = "1970-01-01", tz="GMT")
#   DataAll<-DataAll[order(DataAll[,1]),]
#   
#   ### Middle
#   DataAllClean.temp <- data.frame(table(cut(DataAll[,1], breaks="min")))
#   DataAllClean.temp[which(DataAllClean.temp[,2]==1),2] <- DataAll[,2]
#   DataAllClean.temp[,1] <- as.POSIXct(DataAllClean.temp[,1], origin = "1970-01-01", tz="GMT")
#   
#   DataAllClean <- aggregate(DataAllClean.temp[, 2], list(DateTime=cut(DataAllClean.temp[,1], breaks = paste(Interval, "mins"))), FUN=sum )
#   DataAllClean$x <- as.character(DataAllClean$x)
#   
#   ### For head and tails
#   temp1 <- as.POSIXct(c(paste(FirstDate, "00:00:00"), as.character(DataAllClean[1,1])), origin = "1970-01-01", tz="GMT")
#   Head.temp <- data.frame(table(cut(temp1, breaks= paste(Interval, "mins"))))
#   colnames(Head.temp) <- c("DateTime", "x")
#   Head <- Head.temp[-nrow(Head.temp),]
#   Head$DateTime <- as.character(Head$DateTime)
#   Head[1,2] <- 0
#   
#   temp2 <- as.POSIXct(c(as.character(DataAllClean[nrow(DataAllClean),1]), paste(as.Date(LastDate) + 1, "00:00:00")), origin = "1970-01-01", tz="GMT")
#   tail.temp <- data.frame(table(cut(temp2, breaks= paste(Interval, "mins"))))
#   colnames(tail.temp) <- c("DateTime", "x")
#   Tail <- tail.temp[-which(tail.temp$x==1),]
#   Tail$DateTime <- as.character(Tail$DateTime)
#   
#   DataAllCleanOutput <- rbind.data.frame(Head, DataAllClean, Tail, stringsAsFactors = F)
#   colnames(DataAllCleanOutput)[2] <- "Items"
#   
#   return(DataAllCleanOutput)
# }
# 
# #DataAll <- DataAll[-which(DataAll$Tot_num_incoming==0),]




# ### middle
# DataAllClean.temp <- data.frame(table(cut(DataAll[,1], breaks="min")))
# DataAllClean.temp[which(DataAllClean.temp[,2]==1),2] <- DataAll[,2]
# DataAllClean.temp[,1] <- as.POSIXct(DataAllClean.temp[,1], origin = "1970-01-01", tz="GMT")
# 
# DataAllClean <- aggregate(DataAllClean.temp[, 2], list(DateTime=cut(DataAllClean.temp[,1], breaks = paste(Interval, "mins"))), FUN=sum )
# DataAllClean$x <- as.character(DataAllClean$x)
# 
# ### For head and tails
# temp1 <- as.POSIXct(c(paste(FirstDate, "00:00:00"), as.character(DataAllClean[1,1])), origin = "1970-01-01", tz="GMT")
# Head.temp <- data.frame(table(cut(temp1, breaks= paste(Interval, "mins"))))
# colnames(Head.temp) <- c("DateTime", "x")
# Head <- Head.temp[-nrow(Head.temp),]
# Head$DateTime <- as.character(Head$DateTime)
# Head[1,2] <- 0
# 
# temp2 <- as.POSIXct(c(as.character(DataAllClean[nrow(DataAllClean),1]), paste(as.Date(LastDate) + 1, "00:00:00")), origin = "1970-01-01", tz="GMT")
# tail.temp <- data.frame(table(cut(temp2, breaks= paste(Interval, "mins"))))
# colnames(tail.temp) <- c("DateTime", "x")
# Tail <- tail.temp[-which(tail.temp$x==1),]
# Tail$DateTime <- as.character(Tail$DateTime)
# 
# DataAllCleanOutput <- rbind.data.frame(Head, DataAllClean, Tail, stringsAsFactors = F)
# colnames(DataAllCleanOutput)[2] <- "Items"
