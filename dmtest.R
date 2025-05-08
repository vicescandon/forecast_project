#DMtest
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

errors <- read_excel("infl_var_men.xls", 
                           sheet = "DM_test")

err_arima<-errors$fcst_error_arima
err_fm<-errors$fcst_error_fm

dm.test(err_arima,err_fm,h=1)
