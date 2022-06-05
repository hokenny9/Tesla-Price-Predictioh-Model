#import the required packages
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)

#import the data from yahoo finance 
getSymbols('TSLA', from = '2019-01-01', to = '2022-06-01')
View(TSLA)
#class(TSLA)

chartSeries(TSLA, subset = 'last 6 months', type = 'auto')
addBBands()

##Assigning columns of dataset  
Open_prices = TSLA[,1]
High_prices = TSLA[,2]
Low_prices = TSLA[,3]
Close_prices = TSLA[, 4]
Volume_prices = TSLA[,5]
Adjusted_prices = TSLA[,6]

par(mfrow = c(2,3))

plot(Open_prices, main = 'Opening Price of Stocks (Over a given period)')
plot(High_prices, main = 'Highest Price of Stocks (Over a given period)')
plot(Low_prices, main = 'Lowest Price of Stocks (Over a given period)')
plot(Close_prices, main = 'Closing Price of Stocks (Over a given period)')
plot(Volume_prices, main = 'Volume of Stocks (Over a given period)')
plot(Adjusted_prices, main = 'Adjusted Price of Stocks (Over a given period)')

Predic_Price = Adjusted_prices
#class(Predic_Price)

#Finding the Linear Relation between observations

par(mfrow = c(1,2))
Acf(Predic_Price, main = 'ACF for differenced Series')
Pacf(Predic_Price, main = 'PACF for differenced Series ', col = '#cc0000')
Auto_cf = Acf(Predic_Price, plot = FALSE)
Auto_cf
PAuto_cf = Pacf(Predic_Price, plot = FALSE)
PAuto_cf

print(adf.test(Predic_Price))

#Prediction of price Return

return_TSLA<- 100*diff(log(Predic_Price))

TSLA_return_train <- return_TSLA[1:(0.9*length(return_TSLA))]

TSLA_return_test <- return_TSLA[(0.9*length(return_TSLA)+1):length(return_TSLA)]

auto.arima(TSLA_return_train, seasonal = FALSE)

fit <- Arima(TSLA_return_train, order = c(1,0,0))

preds <- predict(fit, n.ahead = (length(return_TSLA) - (0.9*length(return_TSLA))))$pred
preds


##forecast predictive result

test_forecast <- forecast(fit,h = 15)
test_forecast

par(mfrow = c(1,1))
plot(test_forecast, main = "Potential forecast for TSLA Stock")

accuracy(preds, TSLA_return_test)

