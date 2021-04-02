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

# UK base rate from bank of england
# library(Quandl)
# base_rate<-Quandl("BOE/IUDBEDR")
base_rate<-read_csv("base_rate.csv")
date_min<-min(base_rate$Date)
date_max<-max(base_rate$Date)
xts(base_rate$Value,order.by = base_rate$Date)->base_rate_xts
base_rate_xts %>% to.monthly()->base_rate_xts_mth
