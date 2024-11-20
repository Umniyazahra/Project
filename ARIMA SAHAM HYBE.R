# memanggil packages
library("prettydoc")
library("rmdformats")
library(fGarch)
library(aTSA)
library(FinTS)
library(lmtest)
library(forecast)
library(TSA)
library(tseries)
library(xts)
library(readxl)
library(tidyverse)
library("dygraphs")
library(ggplot2)

data <- `HYBE`
data.ts <- ts(data[, 5])
head(data.ts)
plot(data.ts, type = "l", main = "Saham HYBE Co., Ltd")

# membagi data (data terdiri dari 248, sehingga dibagi menjadi 2)
data.ts_train <- data.ts[1:155]
data.ts_test <- data.ts[151:248]

# plot data training dan testing
plot(data.ts_train, type = "l", main = "Data Training Saham HYBE Co., Ltd 20/10/2022 - 29/07/2023", xlab = "Hari ke-", ylab = "Close")
plot(data.ts_test, type = "l", main = "Data Testing Saham HYBE Co., Ltd 20/10/2022 - 29/07/2023", xlab = "Hari ke-", ylab = "Close")

# model tentatif ARIMA

# uji stasioner data training
ggtsdisplay(data.ts)
adf.test(data.ts_train, alternative = "stationary", k = trunc((length(data.ts_train) - 1)^(1/3)))
# Uji Dickey-Fuller
adf.test(data.ts_train)

#Pembedaan d=1
diff.1 <- diff(data.ts_train, differences = 1)
plot(diff.1, type = "l", main = "Data Training Saham HYBE Co., Ltd 20/10/2022 - 29/07/2023", xlab = "Hari ke-", ylab = "Close")
ggtsdisplay(diff.1)
adf.test(diff.1, alternative = "stationary", k = trunc((length(diff.1) - 1)^(1/3)))

# model
ARIMA011 <- arima(diff.1, order = c(0, 1, 1), method = "ML")
ARIMA101 <- arima(diff.1, order = c(1, 0, 1), method = "ML")
ARIMA111 <- arima(diff.1, order = c(1, 1, 1), method = "ML")

#pengujian parameter
# ARIMA(0,1,1)
coeftest(ARIMA011)
#ARIMA(1,0,1)
coeftest(ARIMA101)
#ARIMA(1,1,1)
coeftest(ARIMA111)

# Fungsi mencari MAD, MSD dan MAPE
findError = function(x, x_fit){
  e = x - x_fit
  n = length(x)
  
  MAD = sum(abs(e))/n
  MSD = sum(e^2)/n
  MAPE = (sum(abs(e)/x)/n)*100
  
  result = matrix(c(MAD, MSD, MAPE), nrow=1)
  colnames(result) = c("MAD", "MSD", "MAPE")
  rownames(result) = "nilai"
  return(result)
}

# Error ARIMA(1,0,0)
findError(data_test, fitted(ARIMA110))

forecast.test101 = forecast(data.ts_train, model = ARIMA101, h = 10)
forecast.test101
accuracy(forecast.test101, data.ts_test)
