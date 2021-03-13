#ARIMA MODEL
#http://ucanalytics.com/blogs/step-by-step-graphic-guide-to-forecasting-through-arima-modeling-in-r-manufacturing-case-study-example/
#Load Libraries
library(fpp2)
library(forecast)
library(tseries)
library(ggplot2)
library(urca)

#Read the Data
data0 <-read.csv(file.choose(),header=T,sep=',')
values <- data0[, -1]
data <- ts(values,start= 1991,frequency=52)
plot(data)


#Stationarity Testing
data%>%ur.kpss()%>%summary()

#Differencing
ndiffs(data)
plot(diff(data))

#Log Transform
datalog1 <- log10(data)
plot(datalog1)

#ACF and PACF
par(mfrow = c(1,2))
acf(diff(datalog1),main='ACF')
pacf(diff(datalog1),main='PACF')

#Arima Model Fitting
#Non-Seasonal ARIMA
ARIMAfit=auto.arima(data,seasonal=FALSE,stepwise=FALSE,approximation=FALSE)
summary(ARIMAfit)

#Seasonal ARIMA
sARIMAfit=auto.arima(data)
summary(sARIMAfit)

#SeasonalARIMA has better AICc

#Forecast
par(mfrow = c(1,1))
autoplot(forecast(sARIMAfit))


#Residual Diagnostics
checkresiduals(sARIMAfit)
shapiro.test(res)
McLeod.Li.test(sARIMAfit)

#GARCH MODEL
#http://eclr.humanities.manchester.ac.uk/index.php/R_GARCH

#Install Library
library(quantmod)
library(rugarch)
library(rmgarch)

#Data Differencing
data1=diff(data)
auto.arima(data1, seasonal = FALSE, stepwise=FALSE, approximation = FALSE) #mencari model arma yang tepat

#Model Spec
ug_spec = ugarchspec()
ug_spec <- ugarchspec(mean.model=list(armaOrder=c(1,2)))

#Model Fitting
ugfit = ugarchfit(spec = ug_spec, data = data1)
ugfit
ugfit@fit$coef

#Forecast
ugfore <- ugarchforecast(ugfit, n.ahead = 104)
ugfore
ug_f <- ugfore@forecast$sigmaFor
plot(ug_f, type = "l") #plotting volatility

ug_var <- ugfit@fit$var   # save the estimated conditional variances
ug_res2 <- (ugfit@fit$residuals)^2   # save the estimated squared residuals

plot(ug_res2, type = "l")
lines(ug_var, col = "green")

#Plotting Volatility
ug_var_t <- c(tail(ug_var,104),rep(NA,52))  # gets the last 104 observations
ug_res2_t <- c(tail(ug_res2,104),rep(NA,52))  # gets the last 104 observations
ug_f <- c(rep(NA,104),(ug_f)^2)

plot(ug_res2_t, type = "l")
lines(ug_f, col = "blue")
lines(ug_var_t, col = "green")
