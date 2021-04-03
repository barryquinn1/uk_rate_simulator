library(shiny)
library(tychobratools)
library(highcharter)
library(DT)
library(shinythemes)
library(sde)
library(readr)
library(xts)
library(dplyr)
library(lubridate)
library(ggfortify)

# UK base rate from bank of england
# library(Quandl)
# base_rate<-Quandl("BOE/IUDBEDR")
base_rate<-read_csv("base_rate.csv")
xts(base_rate$Value,order.by = base_rate$Date)->base_rate_xts
base_rate_xts %>% to.monthly()->base_rate_xts_mth
min<-min(index(base_rate_xts_mth))
max<-max(index(base_rate_xts_mth))
datelist<-index(base_rate_xts_mth)
base_rate_df<-fortify(base_rate_xts_mth)

# x <- log(rgamma(150,5))
# df <- approxfun(density(x))
# plot(density(x))
# xnew <- c(0.45,1.84,2.3)
# points(xnew,df(xnew),col=2)