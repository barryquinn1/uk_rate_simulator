library(shiny)
library(tychobratools)
library(highcharter)
library(DT)
library(shinythemes)
library(sde)
library(readr)
library(xts)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggfortify)

# UK base rate from bank of england
# library(Quandl)
# base_rate<-Quandl("BOE/IUDBEDR")
base_rate<-read_csv("base_rate.csv")
fixed5yr<-read_csv("fiveyr.csv")
names(fixed5yr)<-c("Date","LTV95_bsoc","LTV95_banks","UK_95","UK_75")
fixed5yr$Date<-parse_date_time(fixed5yr$Date,"dmy")
fixed5yr %>%
  transmute(Date=Date,
            UK_95=as.numeric(UK_95),
            UK_75=UK_75) %>%
  arrange(Date)->fixed5yr
xts(base_rate$Value,order.by = base_rate$Date) %>% 
  to.monthly()->base_rate_mth
base_rate_df<-fortify(base_rate_mth)
xts(fixed5yr$UK_75,order.by = fixed5yr$Date) %>% 
  to.monthly() ->fixed5yr_75_mth
xts(fixed5yr$UK_95,order.by = fixed5yr$Date) %>% 
  to.monthly() ->fixed5yr_95_mth
fixed5yr_75_mth_df<-fortify(fixed5yr_75_mth) %>% transmute(Index=Index,UK_75=..Close)
fixed5yr_95_mth_df<-fortify(fixed5yr_95_mth) %>% transmute(Index=Index,UK_95=..Close)
fixed5yr_df<-fixed5yr_95_mth_df %>%
  left_join(fixed5yr_75_mth_df,by="Index")

all_rates<-base_rate_df %>% 
  transmute(Index=Index,base_rate=..Close) %>%
  left_join(fixed5yr %>% mutate(Index=as.yearmon(Date)),by="Index") %>%
  select(-Date)

min<-min(index(fixed5yr_75_mth))
max<-min(max(fixed5yr_df$Index),max(base_rate_df$Index))

datelist<-base_rate_df$Index

#base_rate_df<-fortify(base_rate_xts_mth)

# x <- log(rgamma(150,5))
# df <- approxfun(density(x))
# plot(density(x))
# xnew <- c(0.45,1.84,2.3)
# points(xnew,df(xnew),col=2)
# P=ecdf(x)
# plot(P)
