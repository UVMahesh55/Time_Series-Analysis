#choosing file and loading data
sunspots<-read.csv(file.choose())
library(ggfortify)
library(tseries)
library(ggplot2)
library(forecast)
require(graphics)
library(tidyverse)

sunspots

ss <-sunspots[,-1]
ss

#2.Building Time Series for Sunspots Data
sstimeseries <- ts(ss, frequency=12, start=c(1900,1))
sstimeseries

#FINDING Class,str,Frequency of our Time Series Object
class(sstimeseries)
str(sstimeseries)
frequency(sstimeseries)

#3. Plot the yearly (or other suitable periodic) mean values
plot(sstimeseries,xlab="Date", ylab = "Monthly Sunspots",main="SUNSPOTS DEGREES FROM 1900-1984",col = "orange")

abline(reg = lm(sstimeseries~time(sstimeseries)),col="blue")
plot(aggregate(sstimeseries,FUN = mean),col = "orange")

#4. Plot the monthly (or other suitable periodic) boxplots
boxplot(sstimeseries~cycle(sstimeseries),xlab="Date", ylab = "Monthly SunSpots (in degrees)", main ="Boxplot for Monthly SunSpots 1900-1984",col = "green")

#19.cleaning Time Series Object
SST = tsclean(sstimeseries)	
boxplot(SST~cycle(SST), xlab="Date", ylab = "Monthly SunSpots (in degrees)", main ="Boxplot with no outliers")
SST

#5. Decompose the time series using the stl function. What type of trend does it show?

SST_stl <- stl(SST, s.window = "periodic")
plot(SST_stl, main = "Time series decompostion using stl")

trend_SST = ma(sstimeseries,order = 12, centre = T )
plot(as.ts(sstimeseries),col = "orange")
lines(trend_SST)
plot(as.ts(trend_SST))

#7. Removing trend and seasonality
Dtr = sstimeseries/trend_SST
plot(as.ts(Dtr), col="orange")

m = t(matrix(data = trend_SST, nrow = 12))
seaSST = colMeans(m,na.rm = T)
plot(as.ts(rep(seaSST,12)), col = "orange")

res = sstimeseries / (trend_SST + seaSST)
plot(as.ts(res))

Str <- lm(SST ~ c(1:length(SST)))
plot(SST_stl, main = "Original Trend")
plot(resid(Str),type = "l",ylab = "T_Residue", main = "Residue after removing trend")


Sts.sa <- seasadj(SST_stl) # de-seasonalize
plot(SST, type="l", main = "Original Time Series") # original 
plot(Sts.sa, type="l", main = "Seasonal Adjusted Time Series") # seasonal adjusted

SS_train <- ts(SST, start = c(1900,1), end = c(1960,12), frequency = 12)
SS_train

#8.Building HoltWinter Model for 75% of Data
HWSS <- HoltWinters(SS_train)
HWSS
plot(HWSS, main = "Original time series against the Predicted time series(ADD)")

HWSS <- HoltWinters(SS_train)
HWSS
#Plotting HoltWinter model and Original Model
plot(HWSS, main = "Original time series against the Predicted time series(ADD)")  

plot(fitted(HWSS))

hotSS <- HoltWinters(SS_train, beta = FALSE, gamma = FALSE)
hotSS
hotSS$fitted # Store the forecasts made by HW


plot(hotSS,main = "Original time series against the Predicted time series")


hotSS1 <- HoltWinters(SS_train,gamma = FALSE)
hotSS1
hotSS1$fitted


plot(hotSS1, main = "Original time series against the Predicted time series")

#Predicting Values Using holtWinters MOdel
HW_pred = predict(HWSS,n.ahead = 24*12)
round(HW_pred) # predicted values for next 24 years

ts.plot(SST,HW_pred,col = "navyblue")

H_pred = predict(hotSS,n.ahead = 24*12)
H_pred # predicted values for next 24 years
ts.plot(SST,H_pred,col = "purple")


J_Pred = forecast(hotSS,h=288)
plot(J_Pred)


plot(J_Pred$residuals)


H_pred1 = predict(hotSS1,n.ahead = 24*12)
round(H_pred1) # predicted values for next 24 years


data3 = round(H_pred1) # predicted values for next 24 years
data4 = round(tail(SST,288),0) # original values
ts.plot(SST,H_pred1,col = "navyblue", main = "Original vs Predicted Values") # dotted predicted lines


p <- predict(hotSS1,n.ahead = 288,prediction.interval=TRUE,level=0.95)
plot(hotSS1,p)


S_Pred1 = forecast(hotSS1,h=288)
plot(S_Pred1)

#10. Plot the predicted values along with the actual values to compare them.


X = time(data3)
Y1 = data.frame(data3)
Y2 = data.frame(data4)
df = tbl_df(data.frame(X,Y1,Y2))
df
ggplot(df,aes(X))+
  geom_line(aes(y=data3),colour='red')+
  geom_line(aes(y=data4),colour='blue')+
  labs(y = "SunSpots (in degrees)", x = "Time")+
  ylim(0,288)+xlim(1961,1984)+
  ggtitle("Comparison of predicted value of sunspots and actual spots")


plot.ts(data3, col="red", type = "l",xlim=c(1984,1985),ylim=c(0,12), ylab = "SUNSPOTS (in degrees)")
par(new=TRUE)
plot.ts(data4, col="blue",type="l",xlim=c(1983,1984),ylim=c(0,12), ylab = "SUNSPOTS (in degrees)")


#11. Compute the rms error between the predicted and actual values.

library(MLmetrics)
library(Metrics)
#MODEL 1
HWSS$SSE
H_rmse = rmse(HW_pred,data4)
H_rmse

#MODEL 2
hotSS$SSE
HW_rmse = rmse(H_pred,data4)
HW_rmse

#MODEL 3
hotSS1$SSE
HW1_rmse = rmse(H_pred1,data4)
HW1_rmse

#Building Arima Model for 75% of data and Predicting For next 25% of Data and Comparision
arimaSS <- auto.arima(SS_train)
arimaSS
arimaSS$x
ggtsdiag(arimaSS)


#Method 1 : Using predict()
predicted <- predict(arimaSS,n.ahead = 24*12)
predicted
accuracy(arimaSS$fitted, predicted$pred)
ts.plot(SST,predicted$pred,col = "green")

data1 = round(tail(predicted$pred,288),0)

data2 = round(tail(SST,288),0)

data1
data2



x = time(data1)
y1 = data.frame(data1)
y2 = data.frame(data2)
df = tbl_df(data.frame(x,y1,y2))
df
ggplot(df,aes(x))+
  geom_line(aes(y=data1),colour='red')+
  geom_line(aes(y=data2),colour='blue')+
  labs(y = "SunSpots (in degrees)", x = "Time")+
  ylim(0,288)+xlim(1961,1984)+
  ggtitle("Comparison of predicted value of SunSpots and actual SunSpots")


plot.ts(data1, col="red", type = "l",xlim=c(1978,1985),ylim=c(0,288), ylab = "SunSpots (in degrees)")
par(new=TRUE)
plot(data2, col="blue",type="l",xlim=c(1978,1985),ylim=c(0,288), ylab = "SunSpots (in degrees)", main = "Predicted value vs Actual Monthly SunSpots for Years 1978-1984")
rmse(data1,data2)



plot(log(sstimeseries)) # homongenizing variance

m = diff(log(sstimeseries))
plot(m) # homogenizing mean


pacf(sstimeseries)

acf(sstimeseries, plot = "True")

pacf(diff(sstimeseries)) # p = 3

acf(diff(sstimeseries))

#17.Changing Values of P,d,f of Arima model
fit = arima((SS_train),c(1,1,2),seasonal = list(order=c(1,1,2),period=12))
fit

pred = predict(fit,n.ahead = 24*12)
pred # next 24 years pred values 
plot(pred$pred, col="blue")
plot(pred$se, col="green")


#Step 4 : Testing the model
data11 = round(tail(pred$pred,288),0) # predicted values
data22 = round(tail(sstimeseries,288),0) # original values
data11
data22


#Step 5 : Visualization
#Comparison of predicted value of Sunspots in degrees for next 24 years 
plot.ts(data11, col="red", type = "l",ylim=c(0,288), ylab = "Sunspots (in degrees)", main = "Comparison of predicted value of Sunspots and actual Sunspots")
par(new=TRUE)
plot.ts(data22, col="blue",type="l",ylim=c(0,288), ylab = "Sunspots (in degrees)", main = "Comparison of predicted value of Sunspots and actual Sunspots")

#Predicted values vs Actual MOnthly SunSpots for the year 1984
plot.ts(data1, col="red", type = "l",xlim=c(1978,1984),ylim=c(0,100), ylab = "Sunspots (in degrees)", main = "Predicted values vs Actual Monthly Sunspots for the years 1978-1984")
par(new=TRUE)
plot(data2, col="blue",type="l",xlim=c(1978,1984),ylim=c(0,200), ylab = "Sunspots (in degrees)", main = "Predicted values vs Actual Monthly Sunspots for the years 1978-1984")

#11. Compute the rms error between the predicted and actual values.
rmse(data1,data2)#HoltWinters
rmse(data11,data22)#Arima

#18.Comparision b/w Raw Data and Cleaned Data
par(mfrow = c(2,1))
HW <- HoltWinters(sstimeseries) # This will give same fitting as auto.arima()
HW
plot(HW, main = "Original time series against the Fitted time series : Raw Data")
HWC <- HoltWinters(SST) # This will give same fitting as auto.arima()
HWC
plot(HWC, main = "Original time series against the Fitted time series : Cleaned Data")

#19.Checking error for cleaned data and Uncleaned Data
HW$SSE #Uncleanced Data
HWC$SSE #Cleaned Data
