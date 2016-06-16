library("data.table")
library("zoo")
library("forecast")
library(ggplot2)
library("lmtest")
library("plyr")
library('tseries')

train <- fread("../data/train.csv",stringsAsFactors = T)
test  <- fread("../data/test.csv",stringsAsFactors = T)
store <- fread("../data/store.csv",stringsAsFactors = T)

train[,Date:=as.Date(Date)]
test[,Date:=as.Date(Date)]

train <- train[order(Store,Date)]
test <- test[order(Store,Date)]

setkey(train, Store)

ggplot(train[.(1)][900:942], aes(Date, Sales)) + 
  geom_line() +
  xlab("") +
  theme_bw()

# y = ts(train[.(1)], frequency = 7)
# y1 = ts(train[.(1),Sales], frequency = 7)
# plot.ts(y1)
# plot(train[.(1),Date], train[.(1),Sales])

#store1 = train[.(1)]
#store1.tr = subset(store1, Date <= store1$Date[nrow(store1)] - 48)
#store1.val = subset(store1, Date > store1$Date[nrow(store1)] - 48)

#=========================================================================
## tslm with covariates

#x = store1.tr

x = get_subtrain_val(1)
tr = x$tr
val = x$val

#Sales <- ts(x$Sales, frequency = 7)
# Sales <- ts(x$Sales, frequency = 14)
# DayOfWeek <- factor(x$DayOfWeek)
# Open <- factor(x$Open)
# Promo <- factor(x$Promo)
# StateHoliday <- x$StateHoliday
# SchoolHoliday <- x$SchoolHoliday

fit <- tslm(Sales ~ trend + season + Open + Promo + StateHoliday + SchoolHoliday)
plot(fit)
summary(fit)
plot(residuals(fit))
Acf(residuals(fit))
hist(residuals(fit))
dwtest(fit, alt="two.sided")

store1.val <- within(store1.val, {
  DayOfWeek <- factor(DayOfWeek)
  Open <- factor(Open)
  Promo <- factor(Promo)
})

fc <- forecast.lm(fit, newdata = store1.val)

# plot(store1.val$Date, store1.val$Sales, type='line')
# lines(store1.val$Date, fc$mean, col="red")
# 
# store1.ts = ts(store1$Sales, frequency = 7)
# fit <- stl(store1.ts, t.window = 15, s.window="periodic", robust=TRUE)
# fcast <- forecast(fit, method="naive")
# plot(fcast)

#=========================================================================
## STL decomposition with method = 'ets' for forecasting the seasonally
## adjusted series

# not fit actually, just stl decomposition
fit.stl = stl(Sales, s.window="periodic", robust=T)
plot(fit.stl)
plot(seasadj(fit.stl)[800:894],type='line')
lines(Sales[800:894],col="red")
lines(as.numeric(Open[800:894])*1000,col="green")

# stl decomposition, then fit to seasadjusted sales
fit.stlm = stlm(Sales, s.window="periodic", robust=T, method="ets",
                etsmodel="ZZN", allow.multiplicative.trend = T)
summary(fit.stlm)
fit.stlm$model$components 
# ETS(A,N,N) => simple exponential smoothing with additive errors
# no trend, no seasonality

plot(residuals(fit.stlm$model))
Acf(residuals(fit.stlm$model))

plot(seasadj(fit.stlm$stl)[800:894],type='line')
lines(fitted(fit.stlm$model)[800:894], col='red')

# add in seasonality
plot(seasadj(fit.stlm$stl)[800:894],type='line', main="STLM add in seasonality to fit")
lines((fitted(fit.stlm$model)+fit.stlm$stl$time.series[,"seasonal"])[800:894], col='red')

plot(seasadj(fit.stlm$stl),type='line', main="STLM add in seasonality to fit")
lines((fitted(fit.stlm$model)+fit.stlm$stl$time.series[,"seasonal"]), col='red')

## check, yes the stl plots are identical
plot(seasadj(fit.stl)[800:894],type='line')
lines(seasadj(fit.stlm$stl)[800:894], col='red')

#=========================================================================
# Holt-Winters seasonal method

hw.fit1 <- hw(Sales,seasonal="additive")
#hw.fit2 <- hw(Sales,seasonal="multiplicative")

plot(hw.fit1, ylab="Sales", plot.conf=FALSE, fcol="white", main="Holt-Winters")
lines(fitted(hw.fit1), col="red", lty=2)

plot(hw.fit1$x[800:894], ylab="Sales", type='line', main="Holt-Winters")
lines(fitted(hw.fit1)[800:894], col="red", lty=2)

Acf(residuals(hw.fit1))

#=========================================================================
## spectral analysis

specvalues = spec.pgram(x$Sales)
spec.ar(x$Sales)

# get frequencies corresponding to peaks
str(specvalues)
peakfreq = head(specvalues$freq[order(specvalues$spec, decreasing=T)], 20)
peakperiods = 1/peakfreq
peakperiods # 3.5, 2.3, 7.0...
findfrequency(x$Sales) #4

######################################################
## actually, obfuscated by regular closure dates.
## remove closed dates and check spectrum again

specvalues6 = spec.pgram(x$Sales[Open==1])
spec.ar(x$Sales[Open==1])

# get frequencies corresponding to peaks
str(specvalues6)
peakfreq6 = head(specvalues6$freq[order(specvalues6$spec, decreasing=T)], 20)
peakperiods6 = 1/peakfreq6
peakperiods6 # 12.1, 250.0, 150.0...
findfrequency(x$Sales[Open==1]) # 12

######################################################

freq_per_store = train[, findfrequency(Sales), by=Store]
hist(freq_per_store$V1)
freqs = merge(freq_per_store, store, by="Store")

ggplot(freqs, aes(V1, fill=StoreType)) + geom_histogram()
ggplot(freqs, aes(V1, fill=Assortment)) + geom_histogram()

## how much do continuous closed days affect frequencies calculated? e.g.
## for lengthy store closures in bavaria in 2014

# return vector: run lengths of closed days
rle_closed = function(x) {
  rl = rle(x)
  return(rl$lengths[rl$values==0])
}

rle_per_store = dlply(train, .(Store), function(x) rle_closed(x$Open))
rles = unlist(rle_per_store)
summary(rles)
hist(rles[rles>2], right=F, breaks=seq(186), 
     main="lengths of contiguous store closures \n (closures longer than 2 days)",
     xlab="store closure length")

#=========================================================================
# remove dates when store is closed, does model improve?



#=========================================================================
# Arima

tsdisplay(Sales)
# spikes in ACF at PACF for lag 7 support frequency = 7
sales.adj = seasadj(stl(Sales, s.window="periodic"))
plot(sales.adj)
tsdisplay(sales.adj)
tsdisplay(diff(sales.adj))
# sales appears to be stationary already for just a first difference
#tsdisplay(diff(sales.adj, differences= 2))
fit.arima=auto.arima(Sales)
tsdisplay(residuals(fit.arima))
# ARIMA(2,0,2)(2,0,0)

fit.arima2=auto.arima(Sales, 
                      xreg=cbind(as.numeric(levels(Open))[Open], DayOfWeek))
tsdisplay(residuals(fit.arima2))
## fit.arima2 has better AICc
## ARIMA(3,0,3)(2,0,2)

#=========================================================================
# For example, if holiday contains some dummy variables associated with public holidays and holidayf contains the corresponding variables for the first 100 forecast periods, then the following code can be used:
#  http://robjhyndman.com/hyndsight/dailydata/
#   y <- msts(x, frequency=c(7,365.25))
# z <- fourier(y, K=c(5,5))
# zf <- fourierf(y, K=c(5,5), h=100)
# fit <- auto.arima(y, xreg=cbind(z,holiday), seasonal=FALSE)
# fc <- forecast(fit, xreg=cbind(zf,holidayf), h=100)
# The main disadvantage of the ARIMA approach is that the seasonality is forced to be periodic, whereas a TBATS model allows for dynamic seasonality.

salesf = seasonaldummy(ts(x$Sales, frequency=365.25))

#=========================================================================
Sales6 = x$Sales[Open==1]
Sales6 = ts(Sales6, frequency=12)
lx6 = log(Sales6); dlx6 = diff(lx6); ddlx6 = diff(dlx6, 12)
plot.ts(cbind(Sales6, lx6, dlx6, ddlx6), main="")
#Acf(dlx6)
#Acf(ddlx6)
#plot.ts(diff(lx6, 12))
#plot.ts(ddlx6, ylim=c(-.5,.5))
tsdisplay(lx6)
tsdisplay(dlx6)
tsdisplay(ddlx6)
plot.ts(lx6[600:740])
tsdisplay(diff(diff(lx6,lag=12, differences=2)))
tsdisplay(diff(dlx6, 6))

plot.ts(x$Sales[1:365],ylim=c(2000,9000),main="2013")
points(x$Sales[1:365], pch=c("2","3","4","5","6","7","1"), cex=1.25, font=3, col=1:7)

plot.ts(log1p(x$Sales[366:731]),ylim=c(8,9),main="2014")
points(log1p(x$Sales[366:731]), pch=c("3","4","5","6","7","1","2"), cex=1.25, font=3, col=1:7)

temp2013 = store1.tr[(store1.tr$Date > as.Date("2013-08-01")) & (store1.tr$Date < as.Date("2013-09-17")),]
plot.ts(log1p(temp2013$Sales),ylim=c(8,9), main="fall 2013")
points(log1p(temp2013$Sales), pch=c("5","6","7","1","2","3","4"), cex=1.25, font=3, col=1:7)

temp2014 = store1.tr[(store1.tr$Date > as.Date("2014-08-01")) & (store1.tr$Date < as.Date("2014-09-17")),]
plot.ts(log1p(temp2014$Sales),ylim=c(8,9),main="fall 2014")
points(log1p(temp2014$Sales), pch=c("6","7","1","2","3","4","5"), cex=1.25, font=3, col=1:7)


fit.mArima = Arima(lx6, order=c(5,1,2), seasonal=list(order=c(2,2,2), period=12),
                    xreg=xreg[Open==1,c(-1,-7)])
summary(fit.mArima)
tsdisplay(residuals(fit.mArima))
plot(exp(lx6[640:740]), ylab="Sales", type='line')
lines(exp(fitted(fit.mArima)[640:740]), col="red", lty=2)

lx = log1p(Sales); dlx = diff(lx); ddlx = diff(dlx, 7)
plot.ts(cbind(Sales,lx,dlx,ddlx), main="")
#plot.ts(cbind(Sales,lx,diff(lx, 7),diff(diff(lx, 7))), main="")
tsdisplay(ddlx)
plot.ts(ddlx[700:894], ylim=c(-1,1))
tsdisplay(dlx)
plot.ts(lx[700:894], ylim=c(8,9))

ndiffs(lx6)
nsdiffs(lx6)

fit.mArima = Arima(lx, order=c(2,1,1), seasonal=list(order=c(2,0,2), period=7))
tsdisplay(residuals(fit.mArima))
tsdisplay(diff(ddlx, lag=7))

fit.mArima2 = Arima(lx, order=c(1,1,2), seasonal=list(order=c(1,1,1), period=7),
                    xreg=cbind(as.numeric(levels(Open))[Open], z))
summary(fit.mArima2)
tsdisplay(residuals(fit.mArima2))

z <- fourier(log1p(ts(x$Sales, frequency=365.25)), K=5)

DOW.matrix = model.matrix(~ as.factor(DayOfWeek), store1.tr)
DOW.matrix = DOW.matrix[,-1]  # remove intercept
xreg = cbind(Open=as.numeric(levels(Open))[Open], DOW.matrix)
             
fit.mArima3 = Arima(lx, order=c(2,0,1), seasonal=list(order=c(4,0,0), period=7),
                    xreg=xreg)
summary(fit.mArima3)
tsdisplay(residuals(fit.mArima3))
plot(expm1(lx[800:894]), ylab="Sales", type='line')
lines(expm1(fitted(fit.mArima3)[800:894]), col="red", lty=2)

temp = auto.arima(ts(log1p(x$Sales), frequency=14), xreg=xreg)
summary(temp)
tsdisplay(residuals(temp))
plot(x$Sales[800:894], ylab="Sales", type='line')
lines(expm1(fitted(temp))[800:894], col="red", lty=2)

plot(expm1(lx[700:894]), ylab="Sales", type='line')
lines(expm1(fitted(fit.mArima2)[700:894]), col="red", lty=2)
