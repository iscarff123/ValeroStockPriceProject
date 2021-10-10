### 2018 Summer II STAT 485 Project
### Ian Scarff

### Create a predictive model for Valero (VLO) stock value.
### Linear Regression Model


### To predict future stock value for Valero, 
### let's look at various predictor varieables.

### Use: VLO Stock Data
###      Crude Oil Price from WTI, Cushing OKLAHOMA
###      S&P 500 (A measure of the overall market)
###      VIX (A measure of market volatility)
###      50 Day Moving Averages


### There is a package called "pdfetch" in R
### It has a fucntion called pdfetch
### This function is able to fetch financial data from the internet.

### Call pdfetch
library(pdfetch)

### Call other packages
library(data.table)
library(MASS)
library(car)
library(forecast)
library(tseries)



### using glm() first.



### Import VLO from Yahoo. Fetch close starting at the beginning of 2010
VLO.data <- pdfetch_YAHOO("VLO",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = Sys.Date(),
                          interval = "1d")

### Import S&P 500 from Yahoo. Fetch close starting at the beginning of 2010
SP500.data <- pdfetch_YAHOO("^GSPC",
                            fields = c("close"),
                            from = "2010-01-01",
                            to = Sys.Date(),
                            interval = "1d")

### Import VIX from Yahoo. Fetch close starting at the beginning of 2010
VIX.data <- pdfetch_YAHOO("^VIX",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = Sys.Date(),
                          interval = "1d")


### Use Quandl to get WTI price

library(Quandl)
Quandl.api_key("kshysppmy7gzkZ-MqEKG")

WTI.price <- Quandl("EIA/PET_RWTC_D",type = "xts", start_date = "2010-01-01",
                    force_irregular = FALSE)





### Create workable data set, only include dates with full data
Data <- merge.xts(VLO.data,WTI.price,VIX.data,SP500.data, all = FALSE)
View(Data)

### Only a few dates have been removed

Data <- as.data.frame(Data)
Data <- setDT(Data, keep.rownames = TRUE)
View(Data)

colnames(Data) <- c("Date", "VLO", "WTI", "VIX", "SP500")
Data$Date <- as.Date(Data$Date)


View(Data)


### Calculate 50 Day Moving Average for VLO and S&P500
VLO_MA <- TTR::SMA(Data$VLO, n = 50)
SP500_MA <- TTR::SMA(Data$SP500, n = 50)

#### Make a sequence of dates
Date.n <- seq(c(1:(length(Data$Date))))


### Assign variables
VLO <- Data$VLO
WTI <- Data$WTI
VIX <- Data$VIX
SP500 <- Data$SP500

### The Dickey-Fuller Test won't work with NAs.

### However, we can remove the 49 entries for all variables because the 50 MA
### Does nothing to model the first 49 days

VLO <- VLO[-c(1:49)]
WTI <- WTI[-c(1:49)]
VLO_MA <- VLO_MA[(-c(1:49))]
SP500 <- SP500[(-c(1:49))]
SP500_MA <- SP500_MA[-c(1:49)]
VIX <- VIX[-c(1:49)]
Date.n <- Date.n[-c(1:49)]

### Now perform the Dickey-Fuller Test

adf.test(VLO)
adf.test(VLO_MA)
adf.test(WTI)
adf.test(SP500)
adf.test(SP500_MA)
adf.test(VIX)
adf.test(Date.n)

### Every variable but VIX is unstationary
### Take Differences and shift data
### Don't take difference and shift Date because it will result in a vector of the same constant

VLO.diff.shift <- diff(VLO,1) + abs(min(diff(VLO,1))) + 1
VLO_MA.diff.shift <- diff(VLO_MA,1) + abs(min(diff(VLO_MA,1))) + 1
WTI.diff.shift <- diff(WTI,1) + abs(min(diff(WTI,1))) + 1
SP500.diff.shift <- diff(SP500,1) + abs(min(diff(SP500,1))) + 1
SP500_MA.diff.shift <- diff(SP500_MA,1) + abs(min(diff(SP500_MA,1))) + 1

### Re-test

adf.test(VLO.diff.shift)
adf.test(VLO_MA.diff.shift)
adf.test(WTI.diff.shift)
adf.test(SP500.diff.shift)
adf.test(SP500_MA.diff.shift)

### All data is now stationary

### Remove the first entry in VIX and Date to make all data the same length

VIX <- VIX[-c(1)]
Date.n <- Date.n[-c(1)]

### Data is now ready to be modeled

model1 <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX + Date.n + WTI.diff.shift:VLO_MA.diff.shift + 
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift + 
                VIX:SP500_MA.diff.shift)
summary(model1)

### Use ANOVA partial F test to remove predictors
### Ho: Models do not significantly differ
### Ha: Full model is significantly better

### Start with Date.n, which has the highest p-value

reduced.model <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                       SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA.diff.shift + 
                       SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift + 
                       VIX:SP500_MA.diff.shift)

anova(reduced.model,model1, test = "F")

### Remove Date

model2 <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA.diff.shift + 
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift + 
                VIX:SP500_MA.diff.shift)
summary(model2)

### The interaction between SP500_MA and VIX has the highest p-value.

reduced.model <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                       SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA.diff.shift + 
                       SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model2, test = "F")

### Remove

model3 <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA.diff.shift + 
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model3)

### VLO_MA has the highest p-value. See if we can remove its interaction term

reduced.model <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                       SP500_MA.diff.shift + VIX +
                       SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model3, test = "F")

### Remove

model4 <- glm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX +
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model4)

### WTI has the highest p-value.

reduced.model <- glm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                       SP500_MA.diff.shift + VIX +
                       SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model4, test = "F")

### Remove

model5 <- glm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX +
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model5)

### The intercept has the highest p-value. 

reduced.model <- glm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                       SP500_MA.diff.shift + VIX +
                       SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

anova(reduced.model,model5, test = "F")

### Remove intercept
### We can remove it because the difference between two VLO values can be zero.

model6 <- glm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX +
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model6)

### All variables are now significant. Plot model

par(mfrow=c(2,2))
plot(model6)

### Debatable constant variance. Look at possible transformations. 

X <- cbind(VLO_MA.diff.shift, SP500.diff.shift, SP500_MA.diff.shift, VIX)
trans.x <- powerTransform(X)
summary(trans.x)

### Transform

model7 <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                I((SP500.diff.shift)^(1.5)):I((SP500_MA.diff.shift)^(1.5)) + 
                I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)
summary(model7)

### SP500 has high p-value. Test interaction between SP500 and SP500_MA first.
### (interaction has high p-value)

reduced.model <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                       I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                       I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)

anova(reduced.model,model7, test = "F")

### Remove

model8 <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)
summary(model8)

### SP500 has high p-value. Test interaction between SP500 and VIX first.

reduced.model <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                       I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) - 1)

anova(reduced.model,model8, test = "F")

### ANOVA suggest that the interaction term should not be removed.

model8 <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)
summary(model8)

### No other variables are insignificant. Plot model

par(mfrow=c(2,2))
plot(model8)

### See if VLO needs transformation

trans.y <- powerTransform(model8)
summary(trans.y)

### Transform

model9 <- glm(I((VLO.diff.shift)^(1.45)) ~ I((VLO_MA.diff.shift)^(-1))
              + I((SP500.diff.shift)^(1.5)) + 
                I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)
summary(model9)

### Plot model

plot(model9)

### This model isn't good. Go back to model 8 and remove interaction term.

model8 <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) +
                I((VIX)^(-0.8)):I((SP500.diff.shift)^(1.5)) - 1)

model8.a <- glm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                  I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) - 1)
summary(model8.a)

### All variables are significant. Plot model

plot(model8.a)

### See if VLO needs transformation

trans.y.a <- powerTransform(model8.a)
summary(trans.y.a)

### Transform

model9.a <- glm(I((VLO.diff.shift)^(1.65)) ~ I((VLO_MA.diff.shift)^(-1)) + 
                  I((SP500.diff.shift)^(1.5)) + 
                  I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) - 1)
summary(model9.a)

### SP500_MA has high p-value

reduced.model <- glm(I((VLO.diff.shift)^(1.65)) ~ I((VLO_MA.diff.shift)^(-1)) + 
                       I((SP500.diff.shift)^(1.5)) + I((VIX)^(-0.8)) - 1)

anova(reduced.model,model9.a, test = "F")

### Remove

model10.a <- glm(I((VLO.diff.shift)^(1.65)) ~ I((VLO_MA.diff.shift)^(-1)) + 
                   I((SP500.diff.shift)^(1.5)) + I((VIX)^(-0.8)) - 1)
summary(model10.a)

### All variables are significant. Plot model

plot(model10.a)

### Variance looks decently constant

### Model 6 and model 10.a look vary similar.
### Compare both models using marginal model plots

par(mfrow=c(1,1))
mmp(model6)

### Data is modeled very well

mmp(model10.a)

### Data is not modeled well.

### Continue with model 6.

model6 <- glm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                SP500_MA.diff.shift + VIX +
                SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model6)

### See if VLO needs transformation

trans.y.a <- powerTransform(model6)
summary(trans.y.a)

### Transform

model6.a <- glm(I((VLO.diff.shift)^(1.25)) ~ VLO_MA.diff.shift + SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model6.a)

### All variables are significant. Plot model

par(mfrow=c(2,2))
plot(model6.a)

### Much better variance

### Look at marginal model plot

par(mfrow=c(1,1))
mmp(model6.a)

### Data is modeled vary well

### Look at added variable plots

avPlots(model6.a)

### There are problems with co-linearity. 
### Remove the interaction between SP500 and S

model6.b <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model6.b)

### All variables are significant. Plot model

par(mfrow=c(2,2))
plot(model6.b)
par(mfrow=c(1,1))
mmp(model6.b)

### Variance is still constant and data is modeled well

avPlots(model6.b)

### There still may or may not be problems with co-linearity
### Remove the interaction between SP500 and SP500_MA 

model6.c <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  VIX:SP500.diff.shift - 1)
summary(model6.c)

### SP500_MA is no longer significant. Remove

model6.d <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  VIX +
                  VIX:SP500.diff.shift - 1)
summary(model6.d)

### All variables significant

par(mfrow=c(2,2))
plot(model6.d)
par(mfrow = c(1,1))
mmp(model6.d)

### The variance and data modeling in model 6.b is better. Go back

model6.b <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

avPlots(model6.b)

### Remove the interaction between SP500 and VIX first.

model6.e <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  SP500.diff.shift:SP500_MA.diff.shift - 1)
summary(model6.e)

### VIX no longer significant. Remove

model6.f <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift +
                  SP500.diff.shift:SP500_MA.diff.shift - 1)
summary(model6.f)

### All variables now significant

par(mfrow=c(2,2))
plot(model6.f)
par(mfrow=c(1,1))
mmp(model6.f)

### Variance is not constant and data is not modeled well. Go back to model 6.b

model6.b <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                  SP500_MA.diff.shift + VIX +
                  SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
par(mfrow=c(2,2))
plot(model6.b)
par(mfrow=c(1,1))
mmp(model6.b)
avPlots(model6.b)

### It looks like that model 6.b is the best

final.model <- glm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                     SP500_MA.diff.shift + VIX +
                     SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)



################ DELETE ALL DATA FROM ENVIRONMENT BEFORE CONTINUING!!! #################



####################################################################################
####################################################################################
####################################################################################


### Call pdfetch

library(pdfetch)

### Call other packages
library(data.table)
library(MASS)
library(car)
library(forecast)
library(tseries)

### Import VLO from Yahoo. Fetch close starting at the beginning of 2010
VLO.data <- pdfetch_YAHOO("VLO",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = Sys.Date(),
                          interval = "1d")

### Import S&P 500 from Yahoo. Fetch close starting at the beginning of 2010
SP500.data <- pdfetch_YAHOO("^GSPC",
                            fields = c("close"),
                            from = "2010-01-01",
                            to = Sys.Date(),
                            interval = "1d")

### Import VIX from Yahoo. Fetch close starting at the beginning of 2010
VIX.data <- pdfetch_YAHOO("^VIX",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = Sys.Date(),
                          interval = "1d")


### Use Quandl to get WTI price

library(Quandl)
Quandl.api_key("kshysppmy7gzkZ-MqEKG")

WTI.price <- Quandl("EIA/PET_RWTC_D",type = "xts", start_date = "2010-01-01",
                    force_irregular = FALSE)





### Create workable data set, only include dates with full data
Data <- merge.xts(VLO.data,WTI.price,VIX.data,SP500.data, all = FALSE)
View(Data)

### Only a few dates have been removed

Data <- as.data.frame(Data)
Data <- setDT(Data, keep.rownames = TRUE)
View(Data)

colnames(Data) <- c("Date", "VLO", "WTI", "VIX", "SP500")
Data$Date <- as.Date(Data$Date)


View(Data)


### Calculate 50 Day Moving Average for VLO and S&P500
VLO_MA <- ts(TTR::SMA(Data$VLO, n = 50))
SP500_MA <- ts(TTR::SMA(Data$SP500, n = 50))



### Assign variables
VLO <- ts(Data$VLO)
WTI <- ts(Data$WTI)
VIX <- ts(Data$VIX)
SP500 <- ts(Data$SP500)

### The Dickey-Fuller Test won't work with NAs.

### However, we can remove the 49 entries for all variables because the 50 MA
### Does nothing to model the first 49 days

VLO <- ts(VLO[-c(1:49)])
WTI <- ts(WTI[-c(1:49)])
VLO_MA <- ts(VLO_MA[(-c(1:49))])
SP500 <- ts(SP500[(-c(1:49))])
SP500_MA <- ts(SP500_MA[-c(1:49)])
VIX <- ts(VIX[-c(1:49)])

### Percentage of data removed
data.orig.len <- nrow(Data)
data.new.len <- length(VLO)
percent.rem <- 1 - (data.new.len/data.orig.len)
percent.rem

### Now perform the Dickey-Fuller Test

adf.test(VLO)
adf.test(VLO_MA)
adf.test(WTI)
adf.test(SP500)
adf.test(SP500_MA)
adf.test(VIX)


### Every variable but VIX is unstationary
### Take Differences and shift data
### Don't take difference and shift Date because it will result in a vector of the same constant

VLO.diff.shift <- diff(VLO,1) + abs(min(diff(VLO,1))) + 1
VLO_MA.diff.shift <- diff(VLO_MA,1) + abs(min(diff(VLO_MA,1))) + 1
WTI.diff.shift <- diff(WTI,1) + abs(min(diff(WTI,1))) + 1
SP500.diff.shift <- diff(SP500,1) + abs(min(diff(SP500,1))) + 1
SP500_MA.diff.shift <- diff(SP500_MA,1) + abs(min(diff(SP500_MA,1))) + 1

### Re-test

adf.test(VLO.diff.shift)
adf.test(VLO_MA.diff.shift)
adf.test(WTI.diff.shift)
adf.test(SP500.diff.shift)
adf.test(SP500_MA.diff.shift)

### All data is now stationary

### However, the differenced data starts at an index of 2, not 1.
### This will cause problems further down the road.
### Use the Oarray() function to set the first index as 1

library(Oarray)
VLO.diff.shift <- ts(Oarray(VLO.diff.shift, offset = 1))
VLO_MA.diff.shift <- ts(Oarray(VLO_MA.diff.shift, offset = 1))
WTI.diff.shift <- ts(Oarray(WTI.diff.shift, offset = 1))
SP500.diff.shift <- ts(Oarray(SP500.diff.shift, offset = 1))
SP500_MA.diff.shift <- ts(Oarray(SP500_MA.diff.shift, offset = 1))

### Remove the first entry in VIX and Date to make all data the same length

VIX <- ts(VIX[-c(1)])

### All the data is now ready to be modeled

model1 <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA.diff.shift + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model1)

### VLO_MA has the highest p-value. Test its interaction term first

reduced.model <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model1, test = "F")

### Remove interaction

model2 <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model2)

### WTI has high p-value

reduced.model <-tslm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                         SP500_MA.diff.shift + VIX + 
                         SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model2, test = "F")

### Remove

model3 <- tslm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model3)

### Intercept has the highest p-value

reduced.model <- tslm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

anova(reduced.model,model3, test = "F")

### Remove
### We can remove the intercept because the difference between two VLO values can be zero.

model4 <- tslm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model4)

### All variables are significant. Plot model

par(mfrow=c(2,2))
plot(model4)

### Varaince is debatable. Try transforming.

X <- cbind(VLO_MA.diff.shift, SP500.diff.shift, SP500_MA.diff.shift, VIX)
trans.x <- powerTransform(X)
summary(trans.x)

### Transform

model5 <- tslm(VLO.diff.shift ~ I((VLO_MA.diff.shift)^(-1)) + I((SP500.diff.shift)^(1.5)) + 
                 I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) + 
                 I((SP500.diff.shift)^(1.5)): I((SP500_MA.diff.shift)^(1.5)) + 
                 I((VIX)^(-0.8)):I((SP500_MA.diff.shift)^(1.5)) - 1)
summary(model5)

### All variables are significant. Plot model

plot(model5)

### See if VLO needs transformation

trans.y <- powerTransform(model5)
summary(trans.y)

### Transform

model6 <- tslm(I((VLO.diff.shift)^(1.5)) ~ I((VLO_MA.diff.shift)^(-1)) + 
                 I((SP500.diff.shift)^(1.5)) + I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) + 
                 I((SP500.diff.shift)^(1.5)): I((SP500_MA.diff.shift)^(1.5)) + 
                 I((VIX)^(-0.8)):I((SP500_MA.diff.shift)^(1.5)) - 1)
summary(model6)

### VLO_MA has high p-value.

reduced.model <- tslm(I((VLO.diff.shift)^(1.5)) ~ I((SP500.diff.shift)^(1.5)) +
                        I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) + 
                        I((SP500.diff.shift)^(1.5)): I((SP500_MA.diff.shift)^(1.5)) + 
                        I((VIX)^(-0.8)):I((SP500_MA.diff.shift)^(1.5)) - 1)

anova(reduced.model,model6, test = "F")

### Remove

model7 <-  tslm(I((VLO.diff.shift)^(1.5)) ~ I((SP500.diff.shift)^(1.5)) +
                  I((SP500_MA.diff.shift)^(1.5)) + I((VIX)^(-0.8)) + 
                  I((SP500.diff.shift)^(1.5)): I((SP500_MA.diff.shift)^(1.5)) + 
                  I((VIX)^(-0.8)):I((SP500_MA.diff.shift)^(1.5)) - 1)
summary(model7)

### All variables significant. Plot model

plot(model7)

#### Variance is worse. Go back to model 4.

model4 <- tslm(VLO.diff.shift ~ VLO_MA.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model4)
plot(model4)

### See if VLO needs transformation

trans.y.a <- powerTransform(model4)
summary(trans.y.a)

### Transform

model4.a <- tslm(I((VLO.diff.shift)^(1.25)) ~ VLO_MA.diff.shift + SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model4.a)

### All variables significant. Plot model

plot(model4.a)

### Much better variance

### Look at marginal model plot

par(mfrow=c(1,1))
mmp(model4.a)

### Data is modeled pretty well

### Look at added variable plots

avPlots(model4.a)

### There are problems with co-linearity

### Remove VLO_MA first

model4.b <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model4.b)

### All variables significant

par(mfrow=c(2,2))
plot(model4.b)
par(mfrow=c(1,1))
mmp(model4.b)

### Data is still modeled well

avPlots(model4.b)

### There still may or may not be problems with co-linearity
### Remove the interaction between SP500 and SP500_MA

model4.c <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + VIX:SP500.diff.shift - 1)
summary(model4.c)

### SP500 no longer significant. Remove interaction term first

model4.d <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX - 1)
summary(model4.d)

### All variables significant

par(mfrow=c(2,2))
plot(model4.d)
par(mfrow=c(1,1))
mmp(model4.d)

### Unconstant variance and data is not modeled well. Go back to model 4.b

model4.b <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
avPlots(model4.b)

### Remove interaction between SP500 and VIX

model4.e <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift - 1)
summary(model4.e)

### Remove VIX

model4.f <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + 
                   SP500.diff.shift:SP500_MA.diff.shift - 1)
summary(model4.f)

### All variables significant

par(mfrow=c(2,2))
plot(model4.f)
par(mfrow=c(1,1))
mmp(model4.f)

### Unconstant variance and data is not modeled well. Go back to model 4.b

model4.b <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
avPlots(model4.b)
par(mfrow = c(2,2))
plot(model4.b)

### Model 4.b is the best model we have

final.model <- tslm(I((VLO.diff.shift)^(1.25)) ~ SP500.diff.shift + 
                      SP500_MA.diff.shift + VIX + 
                      SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

### Next Steps
#### Use data from 2010-2017 to build a model
#### Use that model to make prediction for 2018
#### Use 2018 data to overlay raw data with prediction graphically

###################################################################################################
###################################################################################################
###################################################################################################

### This time, we are going to use data from 2010 - 2017 to make a model to predict values for 2018

### Call pdfetch

library(pdfetch)

### Call other packages
library(data.table)
library(MASS)
library(car)
library(forecast)
library(tseries)

### Import VLO from Yahoo. Fetch close starting at the beginning of 2010 - end of 2017
VLO.data <- pdfetch_YAHOO("VLO",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = "2017-12-31",
                          interval = "1d")

### Import S&P 500 from Yahoo. Fetch close starting at the beginning of 2010 - end of 2017
SP500.data <- pdfetch_YAHOO("^GSPC",
                            fields = c("close"),
                            from = "2010-01-01",
                            to = "2017-12-31",
                            interval = "1d")

### Import VIX from Yahoo. Fetch close starting at the beginning of 2010 - end of 2017
VIX.data <- pdfetch_YAHOO("^VIX",
                          fields = c("close"),
                          from = "2010-01-01",
                          to = "2017-12-31",
                          interval = "1d")


### Use Quandl to get WTI price

library(Quandl)
Quandl.api_key("kshysppmy7gzkZ-MqEKG")

WTI.price <- Quandl("EIA/PET_RWTC_D",type = "xts", start_date = "2010-01-01", 
                    end_date = "2017-12-31",
                    force_irregular = FALSE)


### Create workable data set, only include dates with full data
Data <- merge.xts(VLO.data,WTI.price,VIX.data,SP500.data, all = FALSE)
View(Data)

### Only a few dates have been removed

Data <- as.data.frame(Data)
Data <- setDT(Data, keep.rownames = TRUE)
View(Data)

colnames(Data) <- c("Date", "VLO", "WTI", "VIX", "SP500")
Data$Date <- as.Date(Data$Date)


View(Data)


### Calculate 50 Day Moving Average for VLO and S&P500
VLO_MA <- ts(TTR::SMA(Data$VLO, n = 50))
SP500_MA <- ts(TTR::SMA(Data$SP500, n = 50))



### Assign variables
VLO <- ts(Data$VLO)
WTI <- ts(Data$WTI)
VIX <- ts(Data$VIX)
SP500 <- ts(Data$SP500)

### The Dickey-Fuller Test won't work with NAs.

### However, we can remove the 49 entries for all variables because the 50 MA
### Does nothing to model the first 49 days

VLO <- ts(VLO[-c(1:49)])
WTI <- ts(WTI[-c(1:49)])
VLO_MA <- ts(VLO_MA[(-c(1:49))])
SP500 <- ts(SP500[(-c(1:49))])
SP500_MA <- ts(SP500_MA[-c(1:49)])
VIX <- ts(VIX[-c(1:49)])

### Percentage of data removed
data.orig.len <- nrow(Data)
data.new.len <- length(VLO)
percent.rem <- 1 - (data.new.len/data.orig.len)
percent.rem

### Now perform the Dickey-Fuller Test

adf.test(VLO)
adf.test(VLO_MA)
adf.test(WTI)
adf.test(SP500)
adf.test(SP500_MA)
adf.test(VIX)


### Every variable but VIX and VLO_MA is unstationary
### Take Differences and shift data
### Don't take difference and shift Date because it will result in a vector of the same constant

VLO.diff.shift <- diff(VLO,1) + abs(min(diff(VLO,1))) + 1
WTI.diff.shift <- diff(WTI,1) + abs(min(diff(WTI,1))) + 1
SP500.diff.shift <- diff(SP500,1) + abs(min(diff(SP500,1))) + 1
SP500_MA.diff.shift <- diff(SP500_MA,1) + abs(min(diff(SP500_MA,1))) + 1

### Re-test

adf.test(VLO.diff.shift)
adf.test(WTI.diff.shift)
adf.test(SP500.diff.shift)
adf.test(SP500_MA.diff.shift)

### All data is now stationary

### However, the differenced data starts at an index of 2, not 1.
### This will cause problems further down the road.
### Use the Oarray() function to set the first index as 1

library(Oarray)
VLO.diff.shift <- ts(Oarray(VLO.diff.shift, offset = 1))
WTI.diff.shift <- ts(Oarray(WTI.diff.shift, offset = 1))
SP500.diff.shift <- ts(Oarray(SP500.diff.shift, offset = 1))
SP500_MA.diff.shift <- ts(Oarray(SP500_MA.diff.shift, offset = 1))

### Remove the first entry in VIX and VLO_MA to make all data the same length

VIX <- ts(VIX[-c(1)])
VLO_MA <- ts(VLO_MA[-c(1)])

### All the data is now ready to be modeled

model1 <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + WTI.diff.shift:VLO_MA + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model1)

### WTI has the highest p-value. Test its interaction term first

reduced.model <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA + SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model1, test = "F")

### Remove interaction

model2 <- tslm(VLO.diff.shift ~ WTI.diff.shift + VLO_MA + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model2)

### VLO_MA has highest p-value. 

reduced.model <- tslm(VLO.diff.shift ~ WTI.diff.shift + SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model2, test = "F")

### Remove VLO_MA

model3 <- tslm(VLO.diff.shift ~ WTI.diff.shift + SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model3)

### WTI has highest p-value

reduced.model <- tslm(VLO.diff.shift ~  SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)

anova(reduced.model,model3, test = "F")

### Remove WTI

model4 <- tslm(VLO.diff.shift ~  SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model4)

### All variables are significant. Plot model

par(mfrow=c(2,2))
plot(model4)

### Variance is not constant. Look at transformations

X <- cbind(SP500.diff.shift, SP500_MA.diff.shift, VIX)
trans.x <- powerTransform(X)
summary(trans.x)  

### Transform

model5 <- tslm(VLO.diff.shift ~  I((SP500.diff.shift)^(1.35)) + 
                 I((SP500_MA.diff.shift)^(1.6)) + I((VIX)^(-0.75)) + 
                 I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)) + 
                 I((VIX)^(-0.75)):I((SP500.diff.shift)^(1.35)))
summary(model5)  
 
### VIX has high p-value. Test to remove its interaction term.
  
reduced.model <- tslm(VLO.diff.shift ~  I((SP500.diff.shift)^(1.35)) + 
                        I((SP500_MA.diff.shift)^(1.6)) + I((VIX)^(-0.75)) + 
                        I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)))

anova(reduced.model,model5, test = "F")

### Remove

model6 <- tslm(VLO.diff.shift ~  I((SP500.diff.shift)^(1.35)) + 
                 I((SP500_MA.diff.shift)^(1.6)) + I((VIX)^(-0.75)) + 
                 I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)))
summary(model6)  

### VIX has high p-value

reduced.model <- tslm(VLO.diff.shift ~  I((SP500.diff.shift)^(1.35)) + 
                        I((SP500_MA.diff.shift)^(1.6)) + 
                        I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)))

anova(reduced.model,model6, test = "F")

### Remove

model7 <- tslm(VLO.diff.shift ~  I((SP500.diff.shift)^(1.35)) + 
                 I((SP500_MA.diff.shift)^(1.6)) + 
                 I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)))
summary(model7)

### All variables significant. Plot model

plot(model7)

### See if VLO needs transformation

trans.y <- powerTransform(model7)
summary(trans.y)

### Transform

model8 <- tslm(I((VLO.diff.shift)^(1.5)) ~  I((SP500.diff.shift)^(1.35)) + 
                 I((SP500_MA.diff.shift)^(1.6)) + 
                 I((SP500.diff.shift)^(1.35)):I((SP500_MA.diff.shift)^(1.6)))
summary(model8)

### Interaction has highest p-value.

reduced.model <- tslm(I((VLO.diff.shift)^(1.5)) ~  I((SP500.diff.shift)^(1.35)) + 
                        I((SP500_MA.diff.shift)^(1.6)))

anova(reduced.model,model8, test = "F")

### Remove

model9 <- tslm(I((VLO.diff.shift)^(1.5)) ~  I((SP500.diff.shift)^(1.35)) + 
                 I((SP500_MA.diff.shift)^(1.6)))
summary(model9)

### This model is only going to get worse.
### For prediction models, the more variables the better.
### Go back to model 4

model4 <- tslm(VLO.diff.shift ~  SP500.diff.shift + 
                 SP500_MA.diff.shift + VIX + 
                 SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model4)
plot(model4)

### See if VLO needs transformation

trans.y.a <- powerTransform(model4)
summary(trans.y.a)

### Transform

model4.a <- tslm(I((VLO.diff.shift)^(1.5)) ~  SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift)
summary(model4.a)

### Intercept has high p-value

reduced.model <- tslm(I((VLO.diff.shift)^(1.5)) ~  SP500.diff.shift + 
                        SP500_MA.diff.shift + VIX + 
                        SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

anova(reduced.model,model4.a, test = "F")

### Remove Intercept
### This means that the difference between two VLO values is zero, which it can be.

model4.b <- tslm(I((VLO.diff.shift)^(1.5)) ~  SP500.diff.shift + 
                   SP500_MA.diff.shift + VIX + 
                   SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)
summary(model4.b)

### All variables significant. Plot model

plot(model4.b)

### Variance is debatable. Look at marginal model plot

par(mfrow=c(1,1))
mmp(model4.b)

### Data is modeled decently well
### Look at marginal model plots

avPlots(model4.b)

### There are no problems with co-linearity

### It looks like model 4.b is the best model

final.model <- tslm(I((VLO.diff.shift)^(1.5)) ~  SP500.diff.shift + 
                      SP500_MA.diff.shift + VIX + 
                      SP500.diff.shift:SP500_MA.diff.shift + VIX:SP500.diff.shift - 1)

### Now that we have a model, lets predict values for 2018 and compare them to the actual values

### Fetch VLO data starting at the beginning of 2018

VLO.2018 <- pdfetch_YAHOO("VLO",
                          fields = c("close"),
                          from = "2018-01-01",
                          to = Sys.Date(),
                          interval = "1d")

### set data as time series

VLO.2018 <- ts(VLO.2018)
View(VLO.2018)

### Make prediction

df <- data.frame(SP500.diff.shift, SP500_MA.diff.shift, VIX)
pred <- data.frame(predict(final.model, df, interval = "prediction"))

### Raise prediction by the inverse of the power of VLO

pred <- data.frame(pred^(1/1.5))

### Grab the fitted values, and the 95% prediction interval bounds

fit <- pred$fit
lwr <- pred$lwr
upr <- pred$upr


### Create a list of date indexs

dates.n <- nrow(pred)
dates.n <- seq(c(dates.n:(dates.n*2 - 1)))

par(mfrow=c(4,1))

### Make graph showing prediction intervals for the first 50 days
### Plot predicted values
plot(dates.n[1:50],
     fit[1:50],
     pch = 16, col = "black",
     xlab = "Date Index", ylab = "Shifted Differenced Values", 
     main = "Predicted Shifted Difference of VLO",
     sub = "First 50 Stock Market Days",
     ylim = range((min(lwr[1:50])-1):
                    (max(upr[1:50])+1)), cex = 1.3)
### Plot 95% interval bounds
points(dates.n[1:50], 
       upr[1:50], pch = 15, col = "green2")
points(dates.n[1:50],
       lwr[1:50], pch = 15, col = "green2")
### Connect points with segments
segments(dates.n[1:50]
         , fit[1:50],
         dates.n[1:50],
         upr[1:50], col = "green2", lwd = 3)
segments(dates.n[1:50],
         fit[1:50], 
         dates.n[1:50],
         lwr[1:50], col = "green2", lwd = 3)

### Plot real shifted differenced data
points(dates.n[1:50],
       VLO.diff.shift[1:50],
       pch = 18, col = "red", cex = 1.3)


### Make graph showing prediction intervals for the middle 50 days
### Grab the indexs for the middle 50 days

j <- length(dates.n)
k <- round(j/2, digits = 0)
lwr.50 <- k - 25
upr.50 <- k + 24
        
### Plot predicted values
plot(dates.n[lwr.50:upr.50],
     fit[lwr.50:upr.50],
     pch = 16, col = "black",
     xlab = "Date Index", ylab = "Shifted Differenced Values", 
     main = "Predicted Shifted Difference of VLO",
     sub = "Middle 50 Stock Market Days",
     ylim = range((min(lwr[lwr.50:upr.50])-1):
                    (max(upr[lwr.50:upr.50])+1)), cex = 1.3)
### Plot 95% interval bounds
points(dates.n[lwr.50:upr.50], 
       upr[lwr.50:upr.50], pch = 15, col = "green2")
points(dates.n[lwr.50:upr.50],
       lwr[lwr.50:upr.50], pch = 15, col = "green2")
### Connect points with segments
segments(dates.n[lwr.50:upr.50]
         , fit[lwr.50:upr.50],
         dates.n[lwr.50:upr.50],
         upr[lwr.50:upr.50], col = "green2", lwd = 3)
segments(dates.n[lwr.50:upr.50],
         fit[lwr.50:upr.50], 
         dates.n[lwr.50:upr.50],
         lwr[lwr.50:upr.50], col = "green2", lwd = 3)

### Plot real shifted differenced data
points(dates.n[lwr.50:upr.50],
       VLO.diff.shift[lwr.50:upr.50],
       pch = 18, col = "red", cex = 1.3)

### Make graph showing prediction intervals for the final 50 days
### Plot predicted values
plot(dates.n[(length(dates.n) - 50):length(dates.n)],
     fit[(length(fit)-50):(length(fit))],
     pch = 16, col = "black",
     xlab = "Date Index", ylab = "Shifted Differenced Values", 
     main = "Predicted Shifted Difference of VLO",
     sub = "Last 50 Stock Market Days of 2017",
     ylim = range((min(lwr[(length(lwr) - 50):length(lwr)])-1):
                    (max(upr[(length(upr) - 50):length(upr)])+1)), cex = 1.3)
### Plot 95% interval bounds
points(dates.n[(length(dates.n) - 50):length(dates.n)], 
       upr[(length(upr) - 50):length(upr)], pch = 15, col = "green2")
points(dates.n[(length(dates.n) - 50):length(dates.n)],
       lwr[(length(lwr) - 50):length(lwr)], pch = 15, col = "green2")
### Connect points with segments
segments(dates.n[(length(dates.n) - 50):length(dates.n)]
         , fit[(length(fit)-50):(length(fit))],
         dates.n[(length(dates.n) - 50):length(dates.n)],
         upr[(length(upr) - 50):length(upr)], col = "green2", lwd = 3)
segments(dates.n[(length(dates.n) - 50):length(dates.n)],
         fit[(length(fit)-50):(length(fit))], 
         dates.n[(length(dates.n) - 50):length(dates.n)],
         lwr[(length(lwr) - 50):length(lwr)], col = "green2", lwd = 3)

### Plot real shifted differenced data
points(dates.n[(length(dates.n) - 50):length(dates.n)],
       VLO.diff.shift[(length(VLO.diff.shift) - 50):length(VLO.diff.shift)],
       pch = 18, col = "red", cex = 1.3)

plot.new()
### add legend
legend("center",legend = c("Predicted Value","95% Prediction Interval","Real Value"),
       col = c("black","green2","red"), pch = c(16,15,18), horiz = TRUE, bty = "n", 
       lty = c(0,1,0))

### These preidictions and graphs show 2010 - 2017. How to get 2018?

