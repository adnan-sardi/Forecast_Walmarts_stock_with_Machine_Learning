
#Import libraries
library(dplyr)
library(ggplot2)
library(dplyr)
library(tseries)
library("xts")
library(tseries)
library(forecast)
library(tsbox)

#Open and read dataset
setwd("/Users/adnan/Desktop/Time series Walmart")
wmt <- read.csv("WMT Cronologia Dati.csv", sep=",")

#looking at part of dataframe using head() and tail()
head(wmt)
tail(wmt)

#View data in tabular form
View(wmt)
wmt <- wmt[,c(1:7)]
wmt <- wmt[c(1:252),]

#Retrieve the dimension of object
dim(wmt)

#Convert dataframe to timeseries
class(wmt)

# transpose of dataframe
transpose <- t(wmt$Value)

# converting the result to dataframe
transpose <- as.data.frame(transpose)

# calculating reverse of dataframe
rev_data_frame <- rev(transpose)

# transpose of reverse dataframe
rev_data_frame <- t(rev_data_frame)

# converting the result to dataframe
wmt_value_tr <- as.data.frame(rev_data_frame)
View(wmt_value_tr)
wmt <- cbind(wmt,wmt_value_tr)
colnames(wmt)[8] <- "Value reverse"
wmt

wmt_ts<-ts(wmt$`Value reverse`,start=2001,end=2021,frequency = 12)
class(wmt_ts)
wmt_ts

"
#Here I convered in time 
wmt$Data <- as.Date(wmt$Data, format="%d-%m-%Y")
dayofYear <- as.numeric(format(wmt[264,1],"%j"))
wmt_ts = ts(wmt$`Value reverse`, start = c(2000,dayofYear), frequency = 12)
class(wmt_ts)
wmt_ts"

#Verify start,end and frequency about time series
start(wmt_ts)
end(wmt_ts)
frequency(wmt_ts)

#Plot time series
plot(wmt_ts, ylab="Value per stock($)")

#Clearer visualization of the trend
layout(1:2)
plot(aggregate(wmt_ts))
boxplot(wmt_ts ~ cycle(wmt_ts))

#Decomposition
wmt_ts.decompose <- decompose(wmt_ts,"multiplicative")
head(ts(wmt_ts.decompose$random))
plot(wmt_ts.decompose)

#Correlogram
acf(wmt_ts.decompose$random[10:230])

#Examine residul values with function
stand<-function(x){m=mean(x)
s=(var(x)^0.5)
z=(x-m)/s
return(z)}
res.stl<-wmt_ts.decompose$random[10:230]
res.stand<-stand(res.stl)
plot(res.stand,main="Diagramma dei residui standardizzati")

#QQ-plot
qqnorm(res.stand)
abline(0,1)

#Normality test, I use Shapiro-Wilk 
shapiro.test(res.stand)

#Correlogram of residual
acf(res.stand,main="Correlogramma dei residui")

#Ljung-Box and Box-Pierce test
lb<-Box.test(res.stand, lag = 12, type ="Ljung-Box")
bp<-Box.test(res.stand, lag = 12, type ="Box-Pierce")
lb
bp
#Residuals are related

#Logaritmic trasformation for decrease variance
lwmt_ts <- log(wmt_ts)
plot(lwmt_ts,type='l')

#Different trasformation for delete trend
dlwmt_ts <- diff(log(wmt_ts))
plot(dlwmt_ts,type='l')

#Stationary test with Dickey-Fuller and Phillips-Perron
adf.test(dlwmt_ts,alternative = "stationary",k=0)
pp.test(dlwmt_ts)
#Is stationary

#Estimating the model
best_model <- auto.arima(dlwmt_ts, seasonal = F)
fitMA1 <- arima(dlwmt_ts, c(0, 0, 1))
fitMA2 <- arima(dlwmt_ts, c(0, 0, 2))
fitMA3 <- arima(dlwmt_ts, c(0, 0, 3))
fitAR1 <- arima(dlwmt_ts, c(1, 0, 0))
fitAR2 <- arima(dlwmt_ts, c(2, 0, 0))
fitAR3 <- arima(dlwmt_ts, c(3, 0, 0))
fitARMA11 <- arima(dlwmt_ts, c(1, 0, 1))
fitARMA12 <- arima(dlwmt_ts, c(1, 0, 2))
fitARMA21 <- arima(dlwmt_ts, c(2, 0, 1))
AIC(fitMA1,fitMA2,fitMA3,fitAR1,fitAR2,fitAR3,
    fitARMA11,fitARMA12,fitARMA21)
best_model
#The best model is ARMA(1,0)

#Model residuals
fitARMA10 <- arima(dlwmt_ts, c(1,0,0))
res.standARMA10 <- residuals(fitARMA10)
qqnorm(res.standARMA10); qqline(res.standARMA10)


#Normality test
shapiro.test(res.standARMA10)
jarque.bera.test(res.standARMA10)

#Forecast (We should use expanding/rolling window)
pred <- predict(fitARMA10, n.ahead = 12)
ts.plot(dlwmt_ts, pred$pred,lty = c(1,3))

for_ARMA10 <- forecast(fitARMA10,h=12)
autoplot(for_ARMA10)

'#For valuate the model -> RMSE
RealWalmart <- dlwmt_ts[1:263]
seModel <- (RealWalmart-rolling_model)^2
mseModel <- mean(seModel)
rmseModel <- sqrt(mseModel)
rmseModel

Where rolling_model is the model created with rolling window forecast'









