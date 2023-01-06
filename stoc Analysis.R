library(ggplot2)
library(forecast)
library(tseries)
library(prophet)
library(quantmod)

library(tsfknn)
#
getSymbols("^GSPC",src="yahoo",from="2015-01-01",to = "2020-06-04")
head(GSPC)
#try to visualize the close price data with the following graph
autoplot(GSPC$GSPC.Close)
chart_Series(GSPC,TA=NULL)
# add Bollinger Band chart, % Bollinger change, Volume Traded and Moving Average Convergence Divergence to the above graph.
chartSeries(GSPC,TA=c(addVo(),addBBands(),addMACD()))
date<-index(GSPC)
date=as.Date(date)

print(adf.test(GSPC$GSPC.Close,k=1))
par(mfrow=c(1,2))
#Plot ACF and PACF
acf(GSPC$GSPC.Close)
pacf(GSPC$GSPC.Close)
par(mfrow=c(1,1))
modelfit<-auto.arima(GSPC$GSPC.Close,lambda = "auto")
summary(modelfit)
plot(resid(modelfit))
# Histogram of Residuals & Normality Assumption
hist(resid(modelfit),freq = F,ylim = c(0,9000))
e=resid(modelfit)
curve(dnorm(x,mean=mean(e),sd=sd(e)),add=TRUE)
tsdiag(modelfit)
#
Box.test(modelfit$residuals,lag=2,type="Ljung-Box")
Box.test(modelfit$residuals,type="Ljung-Box")
plot(as.ts(GSPC$GSPC.Close))
#With this 3 graphs we focus on the Ljung-Box p-values. For the Ljung-Box test we have that our null hypothesis is:
  
  #H??: The dataset points are independently distributed.

#With this null hypothesis, a significant p-value greater than 0.05 does not rejects the fact that the dataset points are not correlated.
#a<-ts(GSPC$GSPC.Close)
lines(modelfit$fitted,col="blue")
#our forecast for the next 30 days.
price_forecasting<-plot(forecast(modelfit,h=30))
head(price_forecasting$mean)
head(price_forecasting$lower)
head(price_forecasting$upper)
#Dividing the data into train & test sets
N=length(GSPC$GSPC.Close)
n=0.8*N
train<-GSPC$GSPC.Close[1:n,]
test<-GSPC$GSPC.Close[(n+1):N,]
train_arima<-auto.arima(train$GSPC.Close,lambda = "auto")
summary(train_arima)
a=length(test)
trainarima_fit<-forecast(train_arima,h=a)
#Plotting mean predicted  values vs real data
meanvalue<-as.vector(trainarima_fit$mean)
preci<-as.vector(test$GSPC.Close)
plot(meanvalue,type="l",col="red")
lines(preci,type="l")
#In the red line we see our mean forecasting prediction tendency over the real close price of the stock. The tendency shows a good approach predicting the future direction of the close price.