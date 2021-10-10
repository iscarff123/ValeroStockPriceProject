### 2018 Summer II STAT 485 Project
### Ian Scarff

### Create a predictive model for Valero (VLO) stock value.
### Arima Model

### There is a package called "pdfetch" in R
### It has a fucntion caled pdfetch
### This function is able to fetch financial data from the internet.

### call pdfetch
library(pdfetch)



### Import daily VLO data from Yahoo Finance. 
### Fetch closing value starting at the beginning of 2010

VLO.d <- pdfetch_YAHOO("VLO",
                     fields = c("close"),
                     from = "2010-01-01",
                     to = Sys.Date(),
                     interval = "1d")



### Convert to data frame
library(data.table)
VLO.d <- as.data.frame(VLO.d)
VLO.d <- setDT(VLO.d, keep.rownames = TRUE)

colnames(VLO.d) <- c("Date.d", "Price.d")

View(VLO.d)

library(MASS)
library(timeSeries)
library(forecast)
attach(VLO.d)

len.d <- nrow(VLO.d)

### Use the first (n - 100) rows to train the model as test data for the model

### Convet into logorithmic form
### Stocks are based on returns, and returns are based on percentages
### Converting to log captures these attributes in the time series

lnstock = log(Price.d[1:(len.d - 100)])
lnstock


### Look at ACF, PACF, and the Dickey-Fuller Test

acf(lnstock, lag.max = 20)

### Gradual decent in lags

pacf(lnstock, lag.max = 20)

### Immidiate drop in lags. This indicates that data is stationary
### Constant mean, constant variance, constant autocorrelation
### We want data to be stationay. 
### If it wasn't, we wouldn't be able to accurately forecast 

### Another way to test for stationarity is the Dickey-Fuller test

### Perfom Dickey-Fuller Test
### Look for stationarity in both the original and differenced time series

diff.lnstock <- diff(lnstock,1)
diff.lnstock

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(lnstock)
### p-value for test is >0.05. Fail to reject Ho


tseries::adf.test(diff.lnstock)
### p-value for test is <0.05. Reject Ho and accept Ha

### Convert log(test data) to time series
price.arima <- ts(lnstock)


### Fit ARIMA model
### the "auto.arima" function automatically chooses the best 
### number of autoregressors, order of the moving average model,
### and the number of differences.

fit.lnstock <- auto.arima(price.arima)
plot(price.arima, type = "l", main = "ARIMA Fit", sub = "Daily")


### Forecast values
forecasted.values <- forecast(fit.lnstock, h = 100)
forecasted.values
plot(forecasted.values, sub = "Daily")

forecasted.values.extracted <- as.numeric(forecasted.values$mean)

### Revert back to price (non-logorithmic data)

final.forecasted.values <- exp(forecasted.values.extracted)
final.forecasted.values


### Calculate mean percentage error

df.d <- data.frame(Price.d[(len.d- 99):len.d], final.forecasted.values)
col.headings <- c("Actual Price.d", "Forecasted Price.d")
names(df.d) <- col.headings
attach(df.d)

percentage.error.d <- ((df.d$`Actual Price.d`-df.d$`Forecasted Price.d`)/
                       (df.d$`Actual Price.d`))
percentage.error.d
pe.d <- mean(percentage.error.d)
pe.d

### mean percentage error is about x%.
### This means that, on average, there is about an x% difference between 
###                              the actual values and the forecasted values

### Ljung-Box Test
### This will test if the residuals are random.
### If there is correlation between residuals, could skew the accuracy of 
### the estimate

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock$residuals, lag = 15, type = "Ljung-Box")

### All p-values are above 0.5.
### Ho is accepted. All residuals are random


#### Now that the arima model is decently accurate, forecast


### First, double check to see that the full data is stationary

### Take log of full data

ln.stock.full.d <- log(Price.d)

acf(ln.stock.full.d)

### Gradual decent in lags

pacf(ln.stock.full.d)

### Shows that data is stationary
### However, lag 18 is suspect

### Dickey-Fuller Test

diff.lnstock.full.d <- diff(ln.stock.full.d,1)
diff.lnstock.full.d

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(ln.stock.full.d)

### p-value for test is >0.05. Fail to reject Ho

tseries::adf.test(diff.lnstock.full.d)

### p-value for test is <0.05. Reject H. Data is stationary

### Convert log(full data) into time series

price.arima.full.d <- ts(ln.stock.full.d)

### Fit ARIMA model to log(full data)

fit.lnstock.full.d <- auto.arima(price.arima.full.d)
plot(ln.stock.full.d, type = "l", main = "Raw ln(Data) Vs. ARIMA Model", xlab = "Daily",
     ylab = "ln(value)", lwd = 2)
lines(price.arima.full.d, type = "l", col = "red", lty =2, lwd = 2)
legend("topleft", legend = c("Raw ln(Data)","ARIMA Model"), col = c("black","red"),
       lty = c(1,2), lwd = c(2,2))

### Ljung-Box Test

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock.full.d$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock.full.d$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock.full.d$residuals, lag = 15, type = "Ljung-Box")

### All p-values are above 0.5.
### Ho is accepted. All residuals are random

### Forecast values for the next 10 days
forecasted.values.full.d <- forecast(fit.lnstock.full.d, h = 10)
forecasted.values.full.d
plot(forecasted.values.full.d, sub = "Daily")

library(zoom)
zoomplot.zoom(xlim = c(len.d-100,len.d+20))

forecasted.values.extracted.full.d <- as.numeric(forecasted.values.full.d$mean)
upper.bounds <- as.numeric(forecasted.values.full.d$upper)
lower.bounds <- as.numeric(forecasted.values.full.d$lower)
upper.80 <- upper.bounds[1:10]
upper.95 <- upper.bounds[11:20]
lower.80 <- lower.bounds[1:10]
lower.95 <- lower.bounds[11:20]

### Revert back to price (non-logorithmic data)

final.forecasted.values.full.d <- exp(forecasted.values.extracted.full.d)
final.upper.80 <- exp(upper.80)
final.lower.80 <- exp(lower.80)
final.upper.95 <- exp(upper.95)
final.lower.95 <- exp(lower.95)

### Display results neatly

library(lubridate)

### generate the next days
day <- as.Date(Date.d[len.d])+50
future.date <- timeSequence(from = as.Date(Date.d[len.d])+1, by = "day", 
                        to = day)

### Keep only weekdays

future.dates <- future.date[isWeekday(future.date)]

### Remove any New York Stock Exchange Holidays

for(i in 1:length(future.dates)){
  if(isHoliday(future.dates[i], holidays = holidayNYSE()) == TRUE){
    future.dates <- future.dates[-i]
  }
}

### Combine values and the first 10 days

future.forecast.d <- cbind.data.frame(future.dates[1:10],
                                      final.forecasted.values.full.d,
                                      final.upper.95,
                                      final.lower.95,
                                      final.upper.80,
                                      final.lower.80)
colnames(future.forecast.d) <- c("Date", "Value", "95% Upper bound",
                                 "95% Lower bound", "80% Upper bound",
                                 "80% Lower bound")

### Print out predictions

a <- future.forecast.d
b <- capture.output(a)
c <- paste(b,"\n",sep="")
cat("\nStarting from", VLO.d$Date.d[len.d], 
    "the predicted values for the next 10 days are:", "\n",
    c, "\n")

### Make graph showing prediction intervals
### Plot predicted values
plot(future.forecast.d$Date,future.forecast.d$Value, pch = 16, col = "black",
     xlab = "Date", ylab = "Value", main = "Predicted Future Daily VLO Value",
     ylim = range((min(final.lower.95)-1):(max(final.upper.95)+1)))
### Plot 95% interval bounds
points(future.forecast.d$Date, final.upper.95, pch = 15, col = "green2")
points(future.forecast.d$Date, final.lower.95, pch = 15, col = "green2")
### Connect points with segments
segments(future.forecast.d$Date,future.forecast.d$Value, future.forecast.d$Date,
         final.upper.95, col = "green2", lwd = 3)
segments(future.forecast.d$Date,future.forecast.d$Value, future.forecast.d$Date,
         final.lower.95, col = "green2", lwd = 3)
### Plot 80% interval bounds
points(future.forecast.d$Date, final.upper.80, pch = 18, col = "red")
points(future.forecast.d$Date, final.lower.80, pch = 18, col = "red")
### Connect points with segments
segments(future.forecast.d$Date,future.forecast.d$Value, future.forecast.d$Date,
         final.upper.80, col = "red", lwd = 3)
segments(future.forecast.d$Date,future.forecast.d$Value, future.forecast.d$Date,
         final.lower.80, col = "red", lwd = 3)
### Add legent
legend("topleft",legend = c("Predicted Value","95% Interval","80% Interval"),
       col = c("black","green2","red"), pch = c(16,15,18), horiz = TRUE, bty = "n", lty = c(0,1,1))

### Do the same for weekly

detach(VLO.d)
detach(df.d)

### Import weekly VLO data from Yahoo Finance.

VLO.w <- pdfetch_YAHOO("VLO",
                       fields = c("close"),
                       from = "2010-01-01",
                       to = Sys.Date(),
                       interval = "1wk")


### Convert to data frame
VLO.w <- as.data.frame(VLO.w)
VLO.w <- setDT(VLO.w, keep.rownames = TRUE)

colnames(VLO.w) <- c("Date.w", "Price.w")

View(VLO.w)

attach(VLO.w)

len.w <- nrow(VLO.w)

### Use the first (n - 50) rows to train the model as test data for the model

### Convet into logorithmic form
### Stocks are based on returns, and returns are based on percentages
### Converting to log captures these attributes in the time series

lnstock.w.t = log(Price.w[1:(len.w - 50)])
lnstock.w.t


### Look at ACF, PACF, and the Dickey-Fuller Test

acf(lnstock.w.t, lag.max = 20)

### Gradual decent in lags

pacf(lnstock.w.t, lag.max = 20)

### Immidiate drop in lags. This indicates that data is stationary
### Constant mean, constant, variance, constant autocorrelation
### We want data to be stationay. 
### If it wasn't, we wouldn't be able to accurately forecast 

### Another way to test for stationarity is the Dickey-Fuller test

### Perfom Dickey-Fuller Test
### Look for stationarity in both the original and differenced time series

diff.lnstock.w.t <- diff(lnstock.w.t,1)
diff.lnstock.w.t

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(lnstock.w.t)
### p-value for test is >0.05. Fail to reject Ho


tseries::adf.test(diff.lnstock.w.t)
### p-value for test is <0.05. Reject Ho and accept Ha

### Convert log(test data) to time series
price.arima.w.t <- ts(lnstock.w.t)


### Fit ARIMA model
### the "auto.arima" function automatically chooses the best 
### number of autoregressors, order of the moving average model,
### and the number of differences.

fit.lnstock.w.t <- auto.arima(price.arima.w.t)
plot(price.arima.w.t, type = "l", main = "ARIMA Fit", sub = "Weekly")


### Forecast values
forecasted.values.w.t <- forecast(fit.lnstock.w.t, h = 50)
forecasted.values.w.t
plot(forecasted.values.w.t, sub = "Weekly")

forecasted.values.extracted.w.t <- as.numeric(forecasted.values.w.t$mean)

### Revert back to price (non-logorithmic data)

final.forecasted.values.w.t <- exp(forecasted.values.extracted.w.t)
final.forecasted.values.w.t


### Calculate mean percentage error

df.w <- data.frame(Price.w[(len.w- 49):len.w], final.forecasted.values.w.t)
col.headings <- c("Actual Price.w", "Forecasted Price.w")
names(df.w) <- col.headings
attach(df.w)

percentage.error.w <- ((df.w$`Actual Price.w`-df.w$`Forecasted Price.w`)
                     /(df.w$`Actual Price.w`))
percentage.error.w
pe.w <- mean(percentage.error.w)
pe.w

### mean percentage error is about x%.
### This means that, on average, there is about an x% difference between 
###                              the actual values and the forecasted values

### Ljung-Box Test
### This will test if the residuals are random.
### If there is correlation between residuals, could skew the accuracy of 
### the estimate

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock.w.t$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock.w.t$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock.w.t$residuals, lag = 15, type = "Ljung-Box")

### After lag 10, residuals are not random.
### But randomness seems to slowly come back after lag 15


#### Now that the arima model is decently accurate, forecast


### First, double check to see that the full data is stationary

### Take log of full data

ln.stock.full.w <- log(Price.w)

acf(ln.stock.full.w)

### Gradual decent in lags

pacf(ln.stock.full.w)

### Shows that data is stationary

### Dickey-Fuller Test

diff.lnstock.full.w <- diff(ln.stock.full.w,1)
diff.lnstock.full.w

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(ln.stock.full.w)

### p-value for test is >0.05. Fail to reject Ho

tseries::adf.test(diff.lnstock.full.w)

### p-value for test is <0.05. Reject H. Data is stationary

### Convert log(full data) into time series

price.arima.full.w <- ts(ln.stock.full.w)

### Fit ARIMA model to log(full data)

fit.lnstock.full.w <- auto.arima(price.arima.full.w)
plot(ln.stock.full.w, type = "l", main = "Raw ln(Data) Vs. ARIMA Model", xlab = "Weekly",
     ylab = "ln(value)", lwd = 2)
lines(price.arima.full.w, type = "l", col = "red", lty =2, lwd = 2)
legend("topleft", legend = c("Raw ln(Data)","ARIMA Model"), col = c("black","red"),
       lty = c(1,2), lwd = c(2,2))

### Ljung-Box Test

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock.full.w$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock.full.w$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock.full.w$residuals, lag = 15, type = "Ljung-Box")

### After lag 10, residuals are not random
### But randomness seems to slowly come back after lag 15

### Forecast values for the next 10 days
forecasted.values.full.w <- forecast(fit.lnstock.full.w, h = 10)
forecasted.values.full.w
plot(forecasted.values.full.w, sub = "Weekly")

library(zoom)
zoomplot.zoom(xlim = c(len.w-100,len.w+20))

forecasted.values.extracted.full.w <- as.numeric(forecasted.values.full.w$mean)
upper.bounds <- as.numeric(forecasted.values.full.w$upper)
lower.bounds <- as.numeric(forecasted.values.full.w$lower)
upper.80 <- upper.bounds[1:10]
upper.95 <- upper.bounds[11:20]
lower.80 <- lower.bounds[1:10]
lower.95 <- lower.bounds[11:20]

### Revert back to price (non-logorithmic data)

final.forecasted.values.full.w <- exp(forecasted.values.extracted.full.w)
final.upper.80 <- exp(upper.80)
final.lower.80 <- exp(lower.80)
final.upper.95 <- exp(upper.95)
final.lower.95 <- exp(lower.95)
### Display results neatly

library(lubridate)

### generate the next 50 days
Weeks <- as.Date(Date.w[len.w])+100
future.weeks <- timeSequence(from = as.Date(Date.w[len.w]), by = "week", 
                            to = Weeks)


### Combine values and the first 10 days

future.forecast.w <- cbind.data.frame(future.weeks[1:10],
                                      final.forecasted.values.full.w,
                                      final.upper.95,
                                      final.lower.95,
                                      final.upper.80,
                                      final.lower.80)
colnames(future.forecast.w) <- c("Date.w", "Value.w", "95% Upper bound",
                                 "95% Lower bound", "80% Upper bound",
                                 "80% Lower bound")

### Print out predictions

d <- future.forecast.w
e <- capture.output(d)
f <- paste(e,"\n",sep="")
cat("\nStarting from", VLO.w$Date.w[len.w], 
    "the predicted values for the next 10 weeks are:", "\n",
    f, "\n")

### Make graph showing prediction intervals
### Plot predicted values
plot(future.forecast.w$Date.w,future.forecast.w$Value.w, pch = 16, col = "black",
     xlab = "Date", ylab = "Value", main = "Predicted Future Weekly VLO Value",
     ylim = range((min(final.lower.95)-1):(max(final.upper.95)+1)))
### Plot 95% interval bounds
points(future.forecast.w$Date.w, final.upper.95, pch = 15, col = "green2")
points(future.forecast.w$Date.w, final.lower.95, pch = 15, col = "green2")
### Connect points with segments
segments(future.forecast.w$Date.w,future.forecast.w$Value.w, future.forecast.w$Date.w,
         final.upper.95, col = "green2", lwd = 3)
segments(future.forecast.w$Date.w,future.forecast.w$Value.w, future.forecast.w$Date.w,
         final.lower.95, col = "green2", lwd = 3)
### Plot 80% interval bounds
points(future.forecast.w$Date.w, final.upper.80, pch = 18, col = "red")
points(future.forecast.w$Date.w, final.lower.80, pch = 18, col = "red")
### Connect points with segments
segments(future.forecast.w$Date.w,future.forecast.w$Value.w, future.forecast.w$Date.w,
         final.upper.80, col = "red", lwd = 3)
segments(future.forecast.w$Date.w,future.forecast.w$Value.w, future.forecast.w$Date.w,
         final.lower.80, col = "red", lwd = 3)
### Add legent
legend("topleft",legend = c("Predicted Value","95% Interval","80% Interval"),
       col = c("black","green2","red"), pch = c(16,15,18), horiz = TRUE, bty = "n", lty = c(0,1,1))


### Do the same for monthly



detach(VLO.w)
detach(df.w)

### Import weekly VLO data from Yahoo Finance.

VLO.mo <- pdfetch_YAHOO("VLO",
                       fields = c("close"),
                       from = "2010-01-01",
                       to = Sys.Date(),
                       interval = "1mo")


### Convert to data frame
VLO.mo <- as.data.frame(VLO.mo)
VLO.mo <- setDT(VLO.mo, keep.rownames = TRUE)

colnames(VLO.mo) <- c("Date.mo", "Price.mo")

View(VLO.mo)

attach(VLO.mo)

len.mo <- nrow(VLO.mo)

### Use the first (n - 15) rows to train the model as test data for the model

### Convet into logorithmic form
### Stocks are based on returns, and returns are based on percentages
### Converting to log captures these attributes in the time series

lnstock.mo.t = log(Price.mo[1:(len.mo - 15)])
lnstock.mo.t


### Look at ACF, PACF, and the Dickey-Fuller Test

acf(lnstock.mo.t, lag.max = 20)

### Gradual decent in lags

pacf(lnstock.mo.t, lag.max = 20)

### Immidiate drop in lags. This indicates that data is stationary
### Constant mean, constant, variance, constant autocorrelation
### We want data to be stationay. 
### If it wasn't, we wouldn't be able to accurately forecast 

### Another way to test for stationarity is the Dickey-Fuller test

### Perfom Dickey-Fuller Test
### Look for stationarity in both the original and differenced time series

diff.lnstock.mo.t <- diff(lnstock.mo.t,1)
diff.lnstock.mo.t

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(lnstock.mo.t)
### p-value for test is >0.05. Fail to reject Ho


tseries::adf.test(diff.lnstock.mo.t)
### p-value for test is <0.05. Reject Ho and accept Ha

### Convert log(test data) to time series
price.arima.mo.t <- ts(lnstock.mo.t)


### Fit ARIMA model
### the "auto.arima" function automatically chooses the best 
### number of autoregressors, order of the moving average model,
### and the number of differences.

fit.lnstock.mo.t <- auto.arima(price.arima.mo.t)
plot(price.arima.mo.t, type = "l", main = "ARIMA Fit", sub = "Monthly")


### Forecast values
forecasted.values.mo.t <- forecast(fit.lnstock.mo.t, h = 15)
forecasted.values.mo.t
plot(forecasted.values.mo.t, sub = "Monthly")

forecasted.values.extracted.mo.t <- as.numeric(forecasted.values.mo.t$mean)

### Revert back to price (non-logorithmic data)

final.forecasted.values.mo.t <- exp(forecasted.values.extracted.mo.t)
final.forecasted.values.mo.t


### Calculate mean percentage error

df.mo <- data.frame(Price.mo[(len.mo - 14):len.mo], final.forecasted.values.mo.t)
col.headings <- c("Actual Price.mo", "Forecasted Price.mo")
names(df.mo) <- col.headings
attach(df.mo)

percentage.error.mo <- ((df.mo$`Actual Price.mo`-df.mo$`Forecasted Price.mo`)
                       /(df.mo$`Actual Price.mo`))
percentage.error.mo
pe.mo <- mean(percentage.error.mo)
pe.mo

### mean percentage error is about x%.
### This means that, on average, there is about an x% difference between 
###                              the actual values and the forecasted values

### Ljung-Box Test
### This will test if the residuals are random.
### If there is correlation between residuals, could skew the accuracy of 
### the estimate

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock.mo.t$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock.mo.t$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock.mo.t$residuals, lag = 15, type = "Ljung-Box")

### All p-values are about 0.05. Residuals are random


#### Now that the arima model is decently accurate, forecast


### First, double check to see that the full data is stationary

### Take log of full data

ln.stock.full.mo <- log(Price.mo)

acf(ln.stock.full.mo)

### Gradual decent in lags

pacf(ln.stock.full.mo)

### Shows that data is stationary

### Dickey-Fuller Test

diff.lnstock.full.mo <- diff(ln.stock.full.mo,1)
diff.lnstock.full.mo

### Ho: times series is unstationary
### Ha: time series is stationary

tseries::adf.test(ln.stock.full.mo)

### p-value for test is >0.05. Fail to reject Ho

tseries::adf.test(diff.lnstock.full.mo)

### p-value for test is <0.05. Reject H. Data is stationary

### Convert log(full data) into time series

price.arima.full.mo <- ts(ln.stock.full.mo)

### Fit ARIMA model to log(full data)

fit.lnstock.full.mo <- auto.arima(price.arima.full.mo)
plot(ln.stock.full.mo, type = "l", main = "Raw ln(Data) Vs. ARIMA Model", xlab = "Monthly",
     ylab = "ln(value)", lwd = 2)
lines(price.arima.full.mo, type = "l", col = "red", lty =2, lwd = 2)
legend("topleft", legend = c("Raw ln(Data)","ARIMA Model"), col = c("black","red"),
       lty = c(1,2), lwd = c(2,2))

### Ljung-Box Test

### Ho: Residuals are random
### Ha: Residuals are not random

Box.test(fit.lnstock.full.mo$residuals, lag = 5, type = "Ljung-Box")
Box.test(fit.lnstock.full.mo$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.lnstock.full.mo$residuals, lag = 15, type = "Ljung-Box")

### All p-values are above 0.05. Residuals are random

### Forecast values for the next 10 months
forecasted.values.full.mo <- forecast(fit.lnstock.full.mo, h = 10)
forecasted.values.full.mo
plot(forecasted.values.full.mo, sub = "Monthly")

library(zoom)
zoomplot.zoom(xlim = c(len.mo-15,len.mo+20))

forecasted.values.extracted.full.mo <- as.numeric(forecasted.values.full.mo$mean)
upper.bounds <- as.numeric(forecasted.values.full.mo$upper)
lower.bounds <- as.numeric(forecasted.values.full.mo$lower)
upper.80 <- upper.bounds[1:10]
upper.95 <- upper.bounds[11:20]
lower.80 <- lower.bounds[1:10]
lower.95 <- lower.bounds[11:20]

### Revert back to price (non-logorithmic data)

final.forecasted.values.full.mo <- exp(forecasted.values.extracted.full.mo)
final.upper.80 <- exp(upper.80)
final.lower.80 <- exp(lower.80)
final.upper.95 <- exp(upper.95)
final.lower.95 <- exp(lower.95)

### Display results neatly

library(lubridate)

### generate the next months
Months <- as.Date(Date.mo[len.mo])+300
future.months <- timeSequence(from = as.Date(Date.mo[len.mo]), by = "month", 
                             to = Months)


### Combine values and the first 10 months

future.forecast.mo <- cbind.data.frame(future.months[1:10],
                                      final.forecasted.values.full.mo, 
                                      final.upper.95,
                                      final.lower.95,
                                      final.upper.80,
                                      final.lower.80)
colnames(future.forecast.mo) <- c("Date.mo", "Value.mo","95% Upper bound",
                                  "95% Lower bound", "80% Upper bound",
                                  "80% Lower bound")

### Print out predictions

g <- future.forecast.mo
h <- capture.output(g)
i <- paste(h,"\n",sep="")
cat("\nStarting from", VLO.mo$Date.mo[len.mo], 
    "the predicted values for the next 10 months are:", "\n",
    i, "\n")

### Make graph showing prediction intervals
### Plot predicted values
plot(future.forecast.mo$Date.mo,future.forecast.mo$Value.mo, pch = 16, col = "black",
     xlab = "Date", ylab = "Value", main = "Predicted Future Monthly VLO Value",
     ylim = range((min(final.lower.95)-1):(max(final.upper.95)+1)))
### Plot 95% interval bounds
points(future.forecast.mo$Date.mo, final.upper.95, pch = 15, col = "green2")
points(future.forecast.mo$Date.mo, final.lower.95, pch = 15, col = "green2")
### Connect points with segments
segments(future.forecast.mo$Date.mo,future.forecast.mo$Value.mo, future.forecast.mo$Date.mo,
         final.upper.95, col = "green2", lwd = 3)
segments(future.forecast.mo$Date.mo,future.forecast.mo$Value.mo, future.forecast.mo$Date.mo,
         final.lower.95, col = "green2", lwd = 3)
### Plot 80% interval bounds
points(future.forecast.mo$Date.mo, final.upper.80, pch = 18, col = "red")
points(future.forecast.mo$Date.mo, final.lower.80, pch = 18, col = "red")
### Connect points with segments
segments(future.forecast.mo$Date.mo,future.forecast.mo$Value.mo, future.forecast.mo$Date.mo,
         final.upper.80, col = "red", lwd = 3)
segments(future.forecast.mo$Date.mo,future.forecast.mo$Value.mo, future.forecast.mo$Date.mo,
         final.lower.80, col = "red", lwd = 3)
### Add legent
legend("topleft",legend = c("Predicted Value","95% Interval","80% Interval"),
       col = c("black","green2","red"), pch = c(16,15,18), horiz = TRUE, bty = "n", lty = c(0,1,1))



detach(VLO.mo)
detach(df.mo)




