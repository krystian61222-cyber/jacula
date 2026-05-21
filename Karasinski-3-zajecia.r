install.packages("lubridate")
install.packages("lmtest")
install.packages("forecast")

library(lubridate)
library(lmtest)
library(forecast)
library(readxl)

# pobieramy Data3, Sheet: returns

returns <- read_excel("3. R - Karasiński/Data3.xlsx", 
                      sheet = "Returns")

training <- returns[returns$Date > ymd("2006-01-01") & returns$Date < ymd("2019-12-31"),]
test <- returns[returns$Date > ymd("2006-01-01") & returns$Date < ymd("2019-12-31"),]

Pacf(training$PZU, lag.max = 20)  # ile opóźnień maksymalnie ma uwzględniać ta funkcja?

# powyższa funckaj wyrzuca wykres, ostatni wystający słupek poza bandy to ilość maksymalnego rzędu opóźnień
# w tym konkretnym przypadku to 14

# funkcja ARIMA potrafi oszacować 3 modele: Ar, Arma, Arima 
# -> jesli poda się pozycje 1 to oszacuje model AR, 
# jesli wpiszemy w ostatnim nawiasie pozycję 1 i 3 to Arma a jesli wszytskie 3 do Arima

ar_model <- Arima(na.omit(training$PZU), order = c( 14, 0 , 0 )) 

coeftest(ar_model) # z wyniku wyszło, że najsitotniejsze jest opóźnienie na poziomie 3

# prognoza modelem na danych testowych:

ar_forecast <- forecast(ar_model, h = length(test$PZU))

# sprawdzamy dokładnośc tej prognozy:

accuracy(ar_forecast, test$PZU)

# drugi rząd opóźnien analizujemy na posdatawie funkcji ACF

acf(training$PZU, lag.max = 20)

# cięzki do intepretacji wykres: wybiera się z tego rysunku taki rząd opóźnień, przy którym słupki wyraxnie wygasają, 
# w tym przypadku powiedzmy że 7, bo po siódmym zauważamy wygasanie słupków 

arma_model <- Arima(na.omit(training$PZU), order = c( 14, 0 , 7 )) 

# robimy dalej forecast