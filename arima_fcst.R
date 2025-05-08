setwd("C:/Users/victo/OneDrive/Documents/CIDE/8vo semestre/Time Series/forecast/arima")

library("moments")
library("quantmod")
library("lmtest")
library("sandwich")
library("skedastic")
library("nlme")
library("lmtest")
library("ivreg")
library("strucchange")
library("ARDL")
library("forecast")
library("readxl")
library("tidyverse")
library("tseries")
library("tibble")

infl_var_men <- read_excel("infl_var_men.xls", 
                           sheet = "data")
infl_var_men$...1 <- as.Date(infl_var_men$...1)
#Rerotar data desde 2007 del 07
infl_var_men <- infl_var_men[31:243,]

#prepare data
infl<-infl_var_men$inflation
infl<-ts(infl)

# análisis de autocorrelación
acf(infl)
adf.test(infl)

#ajustar arima
infl_adj<-auto.arima(infl)
summary(infl_adj)

# forecast abril
fcast_abr <- forecast(infl_adj, h = 1, level = 90)

#análisis de forecast error con rolling window
pseudo_oos_rmsfe <- function(series_ts, train_window = 36, horizon = 1) {
  T <- length(series_ts)
  start_idx <- T - train_window        # índice del 1er mes a evaluar
  errs <- numeric(train_window)        # almacena errores h=1
  
  for (i in 0:(train_window-1)) {
    #  Serie disponible hasta t = start_idx + i - 1
    train_end <- start_idx + i - 1
    train_ts  <- window(series_ts, end = time(series_ts)[train_end])
    
    fit_i  <- auto.arima(train_ts, seasonal = TRUE)
    fc_i   <- forecast(fit_i, h = horizon)$mean[horizon]
    
    real_i <- series_ts[train_end + horizon]
    errs[i + 1] <- real_i - fc_i
  }
  
  rmsfe <- sqrt(mean(errs^2))
  list(rmsfe = rmsfe,
       errors = errs,
       summary = summary(errs))
}

# Ejecuta la función
oos_eval <- pseudo_oos_rmsfe(infl, train_window = 36, horizon = 1)
oos_eval$rmsfe      # RMSFE “pseudo” (h=1) #ESTE ES EL VALOR A COMPARAR CON EL OTRO MODELO
