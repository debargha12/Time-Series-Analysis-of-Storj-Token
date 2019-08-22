library(dplyr)
library(forecast)
#reading storjTokenPrice file
storjTokenPrice<- read.delim('C:/Users/Debargha/Desktop/Spring 2k19/data science project/storjTokenPrice.txt', sep= "", header = FALSE)
names(storjTokenPrice) <- c("DATE","OPEN","HIGH","LOW","CLOSE","VOLUME","MARKETCAP")
df1 = subset(storjTokenPrice, select = -c(HIGH,LOW,CLOSE,VOLUME,MARKETCAP) )
df1$DATE <- format(as.Date(df1$DATE, format = "%m/%d/%Y"), "%d/%m/%Y")#changing the format of date
df1 <- df1[order(as.Date(df1$DATE, format="%d/%m/%Y")),]#Ordering the dates in ascending order
df1$DATE<-as.Date(df1$DATE, "%d/%m/%Y")#Converting DATE to Date format from Chr format
inds <- seq(as.Date(min(df1$DATE)), as.Date(max(df1$DATE)), by = "day")
ts1<-ts(df1$OPEN,start=c(2017, as.numeric(format(inds[1], "%j"))),
        frequency = 365.25)#converting to timeseries format, check with command class(ts1)
#install.packages("lubridate")
library(lubridate)
ts1<-msts(df1$OPEN,start=decimal_date(as.Date("2017-07-02")),frequency=365.25)
start(ts1)
end(ts1)
plot.ts(ts1)

#Testing for stationarity
#Augmented Dickey-Fuller (ADF) t-test Small p-values suggest that the data is stationary and doesn't need to be differenced stationarity.
adf_test <- adf.test(df1$OPEN,alternative ='stationary')
print(adf_test)
#data not stationary
#Kwiatkowski-Philips-Schmidt-Shin (KPSS) test Here accepting null? hypothesis means that the series is stationary, and small p-value suggest that the series is NOT Stationary and a differencing is required.
kpss_test <- kpss.test(df1$OPEN)
print(kpss_test)
#data not stationary


#Time plots
library(ggplot2)
library(ggfortify)
autoplot(ts1)+ggtitle("Storj Token Price")+ xlab("Year")+ylab("Token Price")

#Seasonal plots
library(ggplot2)
ggseasonplot(ts1, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Token Price") +
  ggtitle("Storj Token Price")
ggsubseriesplot(ts1) +
  ylab("Token Price") +
  ggtitle("Storj Token Price")
gglagplot(ts1)

#autocorrelation
ggAcf(ts1)


#using simple forecasting methods
autoplot(ts1) +
  autolayer(meanf(ts1, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(ts1, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(ts1, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for StorjToken OPEN prices") +
  xlab("Year") + ylab("Price") +
  guides(colour=guide_legend(title="Forecast"))

msts <- msts(df1$OPEN,seasonal.periods = c(7,365.25),start = decimal_date(as.Date("2017-07-02")))
plot(msts, main="Storj Token Price", xlab="Year", ylab="Daily Opening Price")

library(forecast)
tbats <- tbats(msts)
plot(tbats, main="Multiple Season Decomposition")

sp<- predict(tbats,h=14)
plot(sp, main = "TBATS Forecast", include=14)
print(sp)

tsw <- ts(df1$OPEN, start = decimal_date(as.Date("2017-07-02")), frequency = 7)
mytslm <- tslm(tsw ~ trend + season)
print(mytslm)
residarima1 <- auto.arima(mytslm$residuals)
residualsArimaForecast <- forecast(residarima1, h=14)
residualsF <- as.numeric(residualsArimaForecast$mean)

regressionForecast <- forecast(mytslm,h=14)
regressionF <- as.numeric(regressionForecast$mean)

forecastR <- regressionF+residualsF
print(forecastR)

Mapetbats =c()
Mapearima =[]
for (i in 1:20)
{ nTest <- 14*i  
nTrain <- length(msts)- nTest 
train <- window(msts,start=decimal_date(as.Date("2017-07-02")),end=c(decimal_date(as.Date("2017-07-02")),nTrain))
test <- window(msts, start=c(decimal_date(as.Date("2017-07-02")),nTrain+1), end=c(decimal_date(as.Date("2017-07-02")),nTrain+14))

s <- tbats(train)
sp<- predict(s,h=14)

cat("----------------------------------
      
Data Partition",i,"
      
Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
Test Set includes 14 time periods. Observations", nTrain+1, "to", nTrain+14,"
      
")
print(accuracy(sp,test)[6])
Mapetbats<- c(Mapetbats,accuracy(sp,test)[5])
cat("
      
      ")
print(sp$model)
}

for (i in 1:20)
{
  nTest <- 14*i  
  nTrain <- length(tsw)- nTest 
  train <- window(tsw,start=decimal_date(as.Date("2017-07-02")),end=c(decimal_date(as.Date("2017-07-02")),nTrain))
  test <- window(tsw, start=c(decimal_date(as.Date("2017-07-02")),nTrain+1), end=c(decimal_date(as.Date("2017-07-02")),nTrain+14))
  
  trainlm <- tslm(train ~ trend + season)
  trainlmf <- forecast(trainlm,h=14)
  
  residauto <- auto.arima(trainlm$residuals)
  residf <- forecast(residauto,h=14)
  
  y <- as.numeric(trainlmf$mean)
  x <- as.numeric(residf$mean)
  sp <- x+y
  
  cat("----------------------------------
      
Data Partition",i,"
      
Training Set includes",nTrain," time periods. Observations 1 to", nTrain, "
Test Set includes 14 time periods. Observations", nTrain+1, "to", nTrain+14,"
      
")
  
  print(accuracy(sp,test))
  print(residauto)
  
  cat("
    
    ")
  
}
