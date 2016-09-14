rm(list = ls())


RScriptPath<-"C:/gxx/r/project/CallCenter"
source(paste(RScriptPath, "/FormatTS.R", sep=""))


library(RODBC)


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
Interval <- "60"

DataAllClean <- FormatTS(DataAll, FirstDate, LastDate, Interval)
DataAllClean$Items <- as.numeric(DataAllClean$Items)
 


######### Check Intra and Inter day correlation ####
require(corrgram)
require(corrplot)

### Inter day
Corrgram.data.inter.temp <- DataAllClean
Corrgram.data.inter.temp[,1] <- as.POSIXct(Corrgram.data.inter.temp[,1], origin = "1970-01-01", tz="GMT")

Corrgram.data.inter <- tapply(as.integer(Corrgram.data.inter.temp$Items),
                              as.factor(format(Corrgram.data.inter.temp$DateTime, "%Y-%m-%d")), sum)

Corrgram.data.inter <- aggregate(as.integer(Corrgram.data.inter.temp$Items), 
          list(Date=format(Corrgram.data.inter.temp$DateTime, "%Y-%m-%d")), 
          FUN=sum)
colnames(Corrgram.data.inter)[2] <- "Value"

Corrgram.data.inter$WkDay <- weekdays(as.Date(Corrgram.data.inter$Date))
#Corrgram.data.inter$WkDay <- as.numeric(as.factor(Corrgram.data.inter$WkDay) )


boxplot(Corrgram.data.inter$Value ~ Corrgram.data.inter$WkDay)

Corrgram.matrix.inter <- t(matrix(Corrgram.data.inter$Value[c(1:(7*floor(nrow(Corrgram.data.inter)/7)))], nrow  = 7))
colnames(Corrgram.matrix.inter) <- head(Corrgram.data.inter$WkDay,n=7)


# m1
corrgram(Corrgram.matrix.inter, order = F,
         upper.panel = panel.pie,
         lower.panel = panel.conf)


# m2
Correlation.inter <- cor(Corrgram.matrix.inter)
corrplot(Correlation.inter, order = "hclust")


### Intra
Corrgram.data.intra.temp <- DataAllClean
Corrgram.matrix.intra <- t(matrix(as.numeric(Corrgram.data.intra.temp$Items), nrow  = 24*60/as.integer(Interval)))

colnames(Corrgram.matrix.intra) <- seq(from =0, by=as.integer(Interval)/60,
                                       length = 60/as.integer(Interval)*24)
# m1
corrgram(Corrgram.matrix.intra, order = FALSE,
         upper.panel = panel.pie,
         lower.panel = panel.conf,
         cor.method="pearson")


# m2
Correlation.intra <- cor(Corrgram.matrix.intra)
corrplot(Correlation.intra, order = "hclust")


#### Segment for training and testing ####
Test.Start <- "2013-03-01"
wk.total <- 9
DataClean <- DataAllClean[which((as.Date(DataAllClean$DateTime)>= as.Date(Test.Start))
                                 & (as.Date(DataAllClean$DateTime)<= (as.Date(Test.Start)+wk.total*7-1))),]

wk.training <- 7
wk.testing <- 2
Data.training <- head(DataClean, n = wk.training*7*24*(60/as.integer(Interval)))
Data.testing <- tail(DataClean, n = wk.testing*7*24*(60/as.integer(Interval)))


plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue", 
     ylim=c(0, max(DataClean$Items)))
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), type = "o", pch = 22, lty = 2, col = "red")

###############
#### preprocessing ####
# 1. BoxCox
require(MASS)

LoessSmooth <- Data.training
Lambda <- BoxCox.lambda(LoessSmooth$Items)
LoessSmooth$BoxCox <- BoxCox(LoessSmooth$Items, Lambda)

#2. Smoothing
lo <- loess(LoessSmooth$BoxCox ~ as.numeric(as.POSIXct(LoessSmooth$DateTime, origin = "1970-01-01", tz="GMT")),
            span = 0.01)
plot(LoessSmooth$BoxCox,   type ="o", col= "blue",  ylim=c(min(LoessSmooth$BoxCox), max(LoessSmooth$BoxCox)))
lines(lo$fitted, type = "o", pch = 22, lty = 2, col = "red")




#########################################
#### State-Space Model ####
require(forecast)
#Input.data <- Data.training$Items
Input.data <- lo$fitted
Seasonal1 <- 60*24/as.integer(Interval)
Seasonal2 <- 7*Seasonal1
Data.msts <- msts(Input.data, seasonal.periods = c(Seasonal1, Seasonal2))
fit.tbats <- tbats(Data.msts, use.box.cox = T, use.trend = T, 
                   use.damped.trend = T, use.arma.errors = T)

lg <- wk.testing*7*24*60/as.integer(Interval)

Results.temp <- forecast(fit.tbats, h =lg)
#Results <- Results.temp$mean
Results <- InvBoxCox(Results.temp$mean, Lambda)

Results[which(Results < 0 )] <- 0

plot(c(Data.training$Items, rep(0, length= nrow(Data.testing))),
     type ="o", col= "blue",  ylim=c(0, max(DataClean$Items)), cex.axis=1.5)
lines(c(rep(0, length= nrow(Data.training)), Data.testing$Items), type = "o", pch = 22, lty = 2, col = "red")
lines(c(rep(0, length= nrow(Data.training)), Results), type = "o", pch = 22,  col = "green")

plot(Data.testing$Items, type = "o", col = "red")
lines(as.numeric(Results), type = "o", pch = 22,  col = "green")


RMSE <- sqrt(mean((Data.testing$Items-Results)^2, na.rm =T))

ConfidenceLevel <- 0.90

Data.testing$Up <- Data.testing$Items*(1+1- ConfidenceLevel)
Data.testing$Lo <- Data.testing$Items*(ConfidenceLevel)
  
Data.testing$CoverIndicator<- ((Results <= (Data.testing$Items*(1+ 1-ConfidenceLevel)))
                                    & (Results >= (Data.testing$Items*ConfidenceLevel)))
Data.testing$Pred <- Results

  
######## Residual check
Residual <- Data.testing$Pred - Data.testing$Items
plot(Residual)
mean(Residual)
acf(Residual)
qqnorm(Residual)

1-mean(abs(Data.testing$Pred - Data.testing$Items)/mean(Data.testing$Items))

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
