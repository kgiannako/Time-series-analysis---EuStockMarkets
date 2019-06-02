data(EuStockMarkets)
class(EuStockMarkets)
#Daily Closing Prices of Major European Stock Indices, 1991-1998
df<-EuStockMarkets
#time series from Jan 1991 to Dec 1998, T=1860
start(df)
end(df)
str(df)
summary(df)

#time series plot with regression line
plot(df)

plot(df[,1], main="DAX")
abline(reg=lm(df[,1]~time(df[,1])))

plot(df[,2],main="SMI")
abline(reg=lm(df[,2]~time(df[,2])))

plot(df[,3], main="CAC")
abline(reg=lm(df[,3]~time(df[,3])))

plot(df[,4], main="FTSE")
abline(reg=lm(df[,4]~time(df[,4])))

#time series do not seem stationary

#plot of the mean per year
plot(aggregate(df,FUN=mean))


library(DataCombine)
df <- as.data.frame(df)

# both acf() and pacf() generates plots by default
acfRes <- acf(df[,"DAX"]) # autocorrelation
#Autocorrelation is the correlation of a Time Series with lags of itself
pacfRes <- pacf(df[,"DAX"])  # partial autocorrelation
#Partial Autocorrelation is the correlation of the time series with a lag of itself,
#with the linear dependence of all the lags between them removed.
acf(df[,"FTSE"])
pacf(df[,"FTSE"])

library(tseries)
adf.test(df[,"DAX"]) # p-value < 0.05 indicates the TS is not stationary
adf.test(df[,2])
adf.test(df[,3])
adf.test(df[,"FTSE"])

#very high p-values, time series stationary

library(forecast)
#---------------------------------------------------------------------------------------
# Seasonal Differencing
#nsdiffs(df)  # number for seasonal differencing needed

#df_seasdiff <- diff(df, lag=frequency(df), differences=1)  # seasonal differencing
#plot(df_seasdiff, type="l", main="Seasonally Differenced")  # still not stationary!
#--------------------------------------------------------------------------------------

# Make it stationary
#DAX
ndiffs(df[,1], test="adf")  # number of differences need to make it stationary
 
stationaryDAX <-  diff(df[,1], differences= 1)
plot(stationaryDAX, type="l", main="Differenced and Stationary")  # appears to be stationary
acf(stationaryDAX)
pacf(stationaryDAX)

#FTSE
ndiffs(df[,4], test="adf")  # number of differences need to make it stationary
 
stationaryFTSE <- diff(df[,4], differences= 1)
plot(stationaryFTSE, type="l", main="Differenced and Stationary")  # appears to be stationary
acf(stationaryFTSE)
pacf(stationaryFTSE)

#The general regression equation which incorporates a constant and a linear trend is used and the t-statistic for a first order autoregressive coefficient equals one is computed. The number of lags used in the regression is k. The default value of trunc((length(x)-1)^(1/3))
#adf test to check for stationarity
adf.test(stationaryDAX)
adf.test(stationaryFTSE)
#p<0.01 hence stationary

#Cointegration
#Both time series are I(1)
ndiffs(df[,"DAX"], test="adf")
ndiffs(df[,"FTSE"], test="adf")

library(quantmod)
library(egcm)
is.cointegrated(egcm(df[,"DAX"], df[,"FTSE"]))
egcm(df[,"DAX"], df[,"FTSE"])

# egcm has a plot method, which can be useful
plot(egcm(df[,1], df[,2]))
summary(egcm(df[,1], df[,2]))

po.test(df[,c(1,4)])
#null: no cointegration
#p=0.15 suggests no cointegration
#Engle-Granger two-step method
#Available in po.test() from tseries (named after Phillips and Ouliaris)

library(lmtest)
grangertest(stationaryDAX~ stationaryFTSE, order=2)
#F test null: all coeffs are zero
#small p value so not all zero
#FTSE cause DAX
#Wald test performed
grangertest(stationaryFTSE~ stationaryDAX, order=2)
#DAX doesnt cause FTSE


#---------------------ARIMA------------------------------------------------
auto.arima(df[,"DAX"], max.p=15, max.q=15, max.P=10, max.Q = 10, max.order = 30, nmodels = 150, trace=TRUE)
#ARIMA(1,2,0)
auto.arima(df[,"FTSE"], max.p=15, max.q=15, max.P=10, max.Q = 10, max.order = 30, nmodels = 150, trace=TRUE)
#ARIMA(0,1,1)

ArimaDAX<- arima(df[,"DAX"], order=c(1,2,0))

ArimaFTSE<- arima(df[,"FTSE"], order = c(0,1,1) )

summary(ArimaDAX)
summary(ArimaFTSE)

plot(fitted(ArimaDAX))
lines(df[,1], col="red")

plot(fitted(ArimaFTSE))
lines(df[,4], col="red")


coeftest(ArimaDAX)
coeftest(ArimaFTSE)


acf(ArimaDAX$residuals)
library(FitAR)
boxresult<-LjungBoxTest (ArimaDAX$residuals,k=1,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ArimaDAX$residuals)
qqline(ArimaDAX$residuals)


acf(ArimaFTSE$residuals)
boxresult<-LjungBoxTest (ArimaFTSE$residuals,k=1,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(ArimaFTSE$residuals)
qqline(ArimaFTSE$residuals)

#Forecast
library(forecast)
predict(ArimaDAX,n.ahead = 5)
predict(ArimaFTSE,n.ahead = 5)
futurVal <- forecast(ArimaDAX ,h=15, level=c(99))
f<- forecast(ArimaFTSE, h=15, level=c(99))
plot(futurVal)
plot(f)

mean(ArimaDAX$residuals^2)

ssr = sum((fitted(ArimaDAX) - mean(df[,1]))^2)
sse=sum(ArimaDAX$residuals^2)
R<- ssr/(ssr+sse)


#-------jarque bera-------------
library(tsoutliers)
JarqueBera.test(ArimaDAX)
JarqueBera.test(ArimaFTSE)
 

