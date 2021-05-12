---
title: "backlog_eda_series_3"
author: "JINMUN PARK"
date: "11/17/2020"
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyselect') # data wrangling
library('tidyr') # data wrangling
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('stringr') # string manipulation
library('purrr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('forecast') # time series analysis
library('zoo') #Data conversion
library('tseries') # time series analysis
library(forecast)
library(xts)
library(vars)
library(randomForest)
library(nnfor)
#devtools::install_github('ryanbieber/Time-Series-Catch-All')
library(TimeSeriesCatchAll)
library(tidyverse)


# RAW DATA IMPORT
raw_contract = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_finance = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_backlog = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_backlog$Date = as.yearmon(paste(raw_backlog$Year, raw_backlog$Month), "%Y %m")

raw_update = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_update$Date = as.yearmon(paste(raw_update$Year, raw_update$Month), "%Y %m")
raw_update = raw_update %>% subset(select = -c(Year, Month))
raw_update =  raw_update[,c(5,1,2,3,4)] 

# CREATE : ROW % DATA (DATE)
agr_geo_rev = raw_backlog %>%
  group_by(Geo, Date) %>%
  summarise(Revenue = sum(Revenue)) %>%
  dcast(Date~Geo, fill = 0) %>%
  rename(
    "AM_REV" = "AM",
    "AP_REV" = "AP",
    "EM_REV" = "EM",
    "JN_REV" = "JN"
  )

agr_geo_rev = agr_geo_rev[1:12,]
agr_geo_rev = rbind(agr_geo_rev, raw_update)

agr_geo_cost = raw_backlog %>%
  group_by(Geo, Date) %>%
  summarise(Cost = sum(Cost)) %>%
  dcast(Date~Geo, fill = 0) %>%
  rename(
    "AM_COST" = "AM",
    "AP_COST" = "AP",
    "EM_COST" = "EM",
    "JN_COST" = "JN"
  )

```

<br />
<br />

```{r, echo=FALSE, include=FALSE, result='hide'}
# TS CONVERSIOn
ts_geo_rev = ts(agr_geo_rev[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ts_geo_cost = ts(agr_geo_cost[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))  

lambda_setup = rbind(ts_geo_rev, ts_geo_cost)

# BOXCOX
am_lambda = BoxCox.lambda(lambda_setup[,1])
ap_lambda = BoxCox.lambda(lambda_setup[,2])
em_lambda = BoxCox.lambda(lambda_setup[,3])
jn_lambda = BoxCox.lambda(lambda_setup[,4])

am_rev_boxcox = BoxCox(ts_geo_rev[,1], am_lambda)
ap_rev_boxcox = BoxCox(ts_geo_rev[,2], ap_lambda)
em_rev_boxcox = BoxCox(ts_geo_rev[,3], em_lambda)
jn_rev_boxcox = BoxCox(ts_geo_rev[,4], jn_lambda)

rev_boxcox = cbind(am_rev_boxcox, ap_rev_boxcox, em_rev_boxcox, jn_rev_boxcox)

am_cost_boxcox = BoxCox(ts_geo_cost[,1], am_lambda)
ap_cost_boxcox = BoxCox(ts_geo_cost[,2], ap_lambda)
em_cost_boxcox = BoxCox(ts_geo_cost[,3], em_lambda)
jn_cost_boxcox = BoxCox(ts_geo_cost[,4], jn_lambda)

cost_boxcox = cbind(am_cost_boxcox, ap_cost_boxcox, em_cost_boxcox, jn_cost_boxcox)

```

<br />
<br />

```{r, echo=FALSE, include=FALSE, result='hide'}
# STATIONARY TEST
adf.test(am_rev_boxcox) #0.4346
adf.test(ap_rev_boxcox) #0.2274
adf.test(em_rev_boxcox) #0.0408 ##
adf.test(jn_rev_boxcox) #0.3368

adf.test(am_cost_boxcox) #0.3473
adf.test(ap_cost_boxcox) #0.3114
adf.test(em_cost_boxcox) #0.284
adf.test(jn_cost_boxcox) #0.3567

adf.test(diff(diff(am_rev_boxcox, 12))) #0.0388
adf.test(diff(diff(ap_rev_boxcox, 12))) #0.01
adf.test(diff(diff(em_rev_boxcox, 12))) #0.01
adf.test(diff(diff(jn_rev_boxcox, 12))) #0.2962 ##

adf.test(diff(diff(am_cost_boxcox, 12))) #0.1113
adf.test(diff(diff(ap_cost_boxcox, 12))) #0.01
adf.test(diff(diff(em_cost_boxcox, 12))) #0.01
adf.test(diff(diff(jn_cost_boxcox, 12))) #0.1396 ##

# JAPAN NEEDS DOUBLD DF
adf.test(diff(diff(jn_rev_boxcox))) #0.01 
adf.test(diff(diff(jn_cost_boxcox))) #0.01 

``` 

<br />
<br />

## SIMPLE EDA - GEO 
```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
eda_am = merge(agr_geo_rev[,c(1,2)], agr_geo_cost[,c(1,2)], by = "Date", , all.x = TRUE) %>%
  mutate(rev_change = (AM_REV - lag(AM_REV))/lag(AM_REV) * 100) %>%
  mutate(cost_change = (AM_COST - lag(AM_COST))/lag(AM_COST) * 100) %>%
  subset(select = c(Date, rev_change, cost_change))

eda_ap = merge(agr_geo_rev[,c(1,3)], agr_geo_cost[,c(1,3)], by = "Date", , all.x = TRUE) %>%
  mutate(rev_change = (AP_REV - lag(AP_REV))/lag(AP_REV) * 100) %>%
  mutate(cost_change = (AP_COST - lag(AP_COST))/lag(AP_COST) * 100) %>%
  subset(select = c(Date, rev_change, cost_change))

eda_em = merge(agr_geo_rev[,c(1,4)], agr_geo_cost[,c(1,4)], by = "Date", , all.x = TRUE) %>%
  mutate(rev_change = (EM_REV - lag(EM_REV))/lag(EM_REV) * 100) %>%
  mutate(cost_change = (EM_COST - lag(EM_COST))/lag(EM_COST) * 100) %>%
  subset(select = c(Date, rev_change, cost_change))

eda_jn = merge(agr_geo_rev[,c(1,5)], agr_geo_cost[,c(1,5)], by = "Date", , all.x = TRUE) %>%
  mutate(rev_change = (JN_REV - lag(JN_REV))/lag(JN_REV) * 100) %>%
  mutate(cost_change = (JN_COST - lag(JN_COST))/lag(JN_COST) * 100) %>%
  subset(select = c(Date, rev_change, cost_change))

eda_am %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("AM (Rev & Cost) Percentage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) 

eda_ap %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("AP (Rev & Cost) Percentage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) 

eda_em %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("EM (Rev & Cost) Percentage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) 

eda_jn %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("JN (Rev & Cost) Percentage Change") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) 


```

<br />
<br />

## SIMPLE EDA - PERCENTAGE CHANGE GEO 
```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
eda_am_diff = eda_am %>% 
  mutate(diff = rev_change - cost_change) %>%
  subset(select = c(Date, diff))

eda_ap_diff = eda_ap %>% 
  mutate(diff = rev_change - cost_change) %>%
  subset(select = c(Date, diff))

eda_em_diff = eda_em %>% 
  mutate(diff = rev_change - cost_change) %>%
  subset(select = c(Date, diff))

eda_jn_diff = eda_jn %>% 
  mutate(diff = rev_change - cost_change) %>%
  subset(select = c(Date, diff))

eda_am_diff %>%
  ggplot(aes(x = Date, y = diff, label = round(diff, digits = 2))) +
  geom_line(size = 1.5) +
  geom_point(size = 4.5) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("AM (Rev&Cost) Percentage Change Difference") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "solid", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("June 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Sep 2020")), size = 1, linetype = "dashed", color = "red")

eda_ap_diff %>%
  ggplot(aes(x = Date, y = diff, label = round(diff, digits = 2))) +
  geom_line(size = 1.5) +
  geom_point(size = 4.5) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("AP (Rev&Cost) Percentage Change Difference") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_smooth(method = "loess", se = FALSE) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "solid", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2020")), size = 1, linetype = "dashed", color = "red") 

eda_em_diff %>%
  ggplot(aes(x = Date, y = diff, label = round(diff, digits = 2))) +
  geom_line(size = 1.5) +
  geom_point(size = 4.5) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("EM (Rev&Cost) Percentage Change Difference") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_smooth(method = "loess", se = FALSE)

eda_jn_diff %>%
  ggplot(aes(x = Date, y = diff, label = round(diff, digits = 2))) +
  geom_line(size = 1.5) +
  geom_point(size = 4.5) +
  scale_x_yearmon(format="%Y %m", n=36) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "% Change") +
  ggtitle("JN (Rev&Cost) Percentage Change Difference") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_smooth(method = "loess", se = FALSE)
```

<br />
<br />

```{r, echo=FALSE, include=FALSE, result='hide'}
# TRAIN & TEST 
am_rev_train = window(am_rev_boxcox, start = c(2017,1), end = c(2020,6))
am_rev_test = window(am_rev_boxcox, start = c(2020,7), end = c(2020,9))
ap_rev_train = window(ap_rev_boxcox, start = c(2017,1), end = c(2020,6))
ap_rev_test = window(ap_rev_boxcox, start = c(2020,7), end = c(2020,9))
em_rev_train = window(em_rev_boxcox, start = c(2017,1), end = c(2020,6))
em_rev_test = window(em_rev_boxcox, start = c(2020,7), end = c(2020,9))
jn_rev_train = window(jn_rev_boxcox, start = c(2017,1), end = c(2020,6))
jn_rev_test = window(jn_rev_boxcox, start = c(2020,7), end = c(2020,9))

am_cost_train = window(am_cost_boxcox, start = c(2017,1), end = c(2020,6))
am_cost_test = window(am_cost_boxcox, start = c(2020,7), end = c(2020,9))
ap_cost_train = window(ap_cost_boxcox, start = c(2017,1), end = c(2020,6))
ap_cost_test = window(ap_cost_boxcox, start = c(2020,7), end = c(2020,9))
em_cost_train = window(em_cost_boxcox, start = c(2017,1), end = c(2020,6))
em_cost_test = window(em_cost_boxcox, start = c(2020,7), end = c(2020,9))
jn_cost_train = window(jn_cost_boxcox, start = c(2017,1), end = c(2020,6))
jn_cost_test = window(jn_cost_boxcox, start = c(2020,7), end = c(2020,9))

```


```{r, echo=FALSE, include=FALSE, result='hide'}
#all_in_one_time_series(agr_geo_cost[,1:2])
```

<br />
<br />


```{r, echo=FALSE, include=FALSE, result='hide'}
library(knitr)

manual_function = function(starty, startm, testy, testm){
  
  trainy = ifelse(testm-1 == 0, testy-1, testy)
  trainm = ifelse(testm-1 == 0, 12, testm-1) 
  test_endm = testm + 2
  splity = testy - 1
  split_startm = 1
  
  # REV SPLIT
  am_rev_train = window(am_rev_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  am_rev_test = window(am_rev_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  ap_rev_train = window(ap_rev_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  ap_rev_test = window(ap_rev_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  em_rev_train = window(em_rev_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  em_rev_test = window(em_rev_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  jn_rev_train = window(jn_rev_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  jn_rev_test = window(jn_rev_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  
  # COST SPLIT
  am_cost_train = window(am_cost_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  am_cost_test = window(am_cost_boxcox, start = c(testy,testm), end = c(testy, test_endm))
  ap_cost_train = window(ap_cost_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  ap_cost_test = window(ap_cost_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  em_cost_train = window(em_cost_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  em_cost_test = window(em_cost_boxcox, start = c(testy,testm), end = c(testy,test_endm))
  jn_cost_train = window(jn_cost_boxcox, start = c(starty,startm), end = c(trainy,trainm))
  jn_cost_test = window(jn_cost_boxcox, start = c(testy,testm), end = c(testy,test_endm)) 

  # GGPLOT SPLIT
  am_cost_split = window(am_cost_boxcox, start = c(splity,split_startm), end = c(testy,test_endm))
  ap_cost_split = window(ap_cost_boxcox, start = c(splity,split_startm), end = c(testy,test_endm))
  em_cost_split = window(em_cost_boxcox, start = c(splity,split_startm), end = c(testy,test_endm))
  jn_cost_split = window(jn_cost_boxcox, start = c(splity,split_startm), end = c(testy,test_endm))
  
  # MULTIVARIATE : TSLM
  am_tslm = tslm(am_cost_train ~ am_rev_train + trend + season)
  ap_tslm = tslm(ap_cost_train ~ ap_rev_train + trend + season)
  em_tslm = tslm(em_cost_train ~ em_rev_train + trend + season)
  jn_tslm = tslm(jn_cost_train ~ jn_rev_train + trend + season)
  
  tslm_am_fcst = forecast(am_tslm, newdata = am_rev_test)
  tslm_ap_fcst = forecast(ap_tslm, newdata = ap_rev_test) 
  tslm_em_fcst = forecast(em_tslm, newdata = em_rev_test) 
  tslm_jn_fcst = forecast(jn_tslm, newdata = jn_rev_test)

  # MULTIVARIATE : SARIMAX (SETTING : AUTO)
  am_sarimx_auto = auto.arima(am_cost_train, xreg = am_rev_train)
  ap_sarimx_auto = auto.arima(ap_cost_train, xreg = ap_rev_train)
  em_sarimx_auto = auto.arima(em_cost_train, xreg = em_rev_train)
  jn_sarimx_auto = auto.arima(jn_cost_train, xreg = jn_rev_train)

  am_sarimx_autofcst = forecast(am_sarimx_auto, xreg = am_rev_test)
  ap_sarimx_autofcst = forecast(ap_sarimx_auto, xreg = ap_rev_test)
  em_sarimx_autofcst = forecast(em_sarimx_auto, xreg = em_rev_test)
  jn_sarimx_autofcst = forecast(jn_sarimx_auto, xreg = jn_rev_test)
                          
  # MULTIVARIATE : UNIVARIATE : ETS
  ets_am_model = ets(am_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)
  ets_ap_model = ets(ap_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)
  ets_em_model = ets(em_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)
  ets_jn_model = ets(jn_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)

  ets_am_fcst = forecast(ets_am_model, h = 3)
  ets_ap_fcst = forecast(ets_ap_model, h = 3)
  ets_em_fcst = forecast(ets_em_model, h = 3)
  ets_jn_fcst = forecast(ets_jn_model, h = 3)

  # UNIVARIATE : ARIMA (SETTING : AUTO)
  am_arima = auto.arima(am_cost_train)
  ap_arima = auto.arima(ap_cost_train)
  em_arima = auto.arima(em_cost_train)
  jn_arima = auto.arima(jn_cost_train)

  am_arima_fcst = forecast(am_arima, h=3)
  ap_arima_fcst = forecast(ap_arima, h=3)
  em_arima_fcst = forecast(em_arima, h=3)
  jn_arima_fcst = forecast(jn_arima, h=3)

  # UNIVARIATE : MLP (SETTING : D)
  am_mlp = mlp(am_cost_train)
  ap_mlp = mlp(ap_cost_train)
  em_mlp = mlp(em_cost_train)
  jn_mlp = mlp(jn_cost_train)

  am_mlp_fcst = forecast(am_mlp, h = 3)
  ap_mlp_fcst = forecast(ap_mlp, h = 3)
  em_mlp_fcst = forecast(em_mlp, h = 3)
  jn_mlp_fcst = forecast(jn_mlp, h = 3)

  # ACCURACY
  ac_am_tslm = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(tslm_am_fcst$mean, am_lambda))
  ac_am_sarimax = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_sarimx_autofcst$mean, am_lambda))
  ac_am_ets = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(ets_am_fcst$mean, am_lambda))
  ac_am_arima = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_arima_fcst$mean, am_lambda))
  ac_am_mlp = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_mlp_fcst$mean, am_lambda))
  ac_am = rbind(ac_am_tslm[1,1:5], ac_am_sarimax[1,1:5], ac_am_ets[1,1:5], ac_am_arima[1,1:5], ac_am_mlp[1,1:5])
  rownames(ac_am) = c("AM_TSLM", "AM_SARIMAX", "AM_ETS", "AM_ARIMA", "AM_MLP")
  ac_am_best = ac_am[order("RMSE"),,drop = FALSE]

  ac_ap_tslm = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(tslm_ap_fcst$mean, ap_lambda))
  ac_ap_sarimax = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_sarimx_autofcst$mean, ap_lambda))  
  ac_ap_ets = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ets_ap_fcst$mean, ap_lambda))
  ac_ap_arima = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_arima_fcst$mean, ap_lambda)) 
  ac_ap_mlp = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_mlp_fcst$mean, ap_lambda))
  ac_ap = rbind(ac_ap_tslm[1,1:5], ac_ap_sarimax[1,1:5], ac_ap_ets[1,1:5], ac_ap_arima[1,1:5], ac_ap_mlp[1,1:5])
  rownames(ac_ap) = c("AP_TSLM", "AP_SARIMAX", "AP_ETS", "AP_ARIMA", "AP_MLP")
  ac_ap_best = ac_ap[order("RMSE"),,drop = FALSE]

  ac_em_tslm = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(tslm_em_fcst$mean, em_lambda))
  ac_em_sarimax = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_sarimx_autofcst$mean, em_lambda))
  ac_em_ets = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(ets_em_fcst$mean, em_lambda))    
  ac_em_arima = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_arima_fcst$mean, em_lambda))
  ac_em_mlp = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_mlp_fcst$mean, em_lambda))
  ac_em = rbind(ac_em_tslm[1,1:5], ac_em_sarimax[1,1:5], ac_em_ets[1,1:5], ac_em_arima[1,1:5], ac_em_mlp[1,1:5])
  rownames(ac_em) = c("EM_TSLM", "EM_SARIMAX", "EM_ETS", "EM_ARIMA", "EM_MLP")
  ac_em_best = ac_em[order("RMSE"),,drop = FALSE]
  
  
  ac_jn_tslm = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(tslm_jn_fcst$mean, jn_lambda))
  ac_jn_sarimax = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_sarimx_autofcst$mean, jn_lambda))
  ac_jn_ets = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(ets_jn_fcst$mean, jn_lambda))    
  ac_jn_arima = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_arima_fcst$mean, jn_lambda)) 
  ac_jn_mlp = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_mlp_fcst$mean, jn_lambda))
  ac_jn = rbind(ac_jn_tslm[1,1:5], ac_jn_sarimax[1,1:5], ac_jn_ets[1,1:5], ac_jn_arima[1,1:5], ac_jn_mlp[1,1:5])
  rownames(ac_jn) = c("JN_TSLM", "JN_SARIMAX", "JN_ETS", "JN_ARIMA", "JN_MLP")
  ac_em_best = ac_em[order("RMSE"),,drop = FALSE]

  # ACCURACY PRINT
  ac_summary = rbind(ac_am_best, ac_ap_best, ac_em_best, ac_em_best)
  print(ac_summary)
  
  # PLOT 
  am_plot = autoplot(InvBoxCox(am_cost_split, am_lambda), title = "AM Time Series Forecast") + 
    autolayer(InvBoxCox(tslm_am_fcst$mean, am_lambda), series = "am_tslm") + 
    autolayer(InvBoxCox(am_sarimx_autofcst$mean, am_lambda), series = "am_sarimax(auto)") +
    autolayer(InvBoxCox(ets_am_fcst$mean, am_lambda), series = "am_ets") + 
    autolayer(InvBoxCox(am_arima_fcst$mean, am_lambda), series = "am_arima(auto)") +
    autolayer(InvBoxCox(am_mlp_fcst$mean, am_lambda), series = "am_mlp")
  

  ap_plot = autoplot(InvBoxCox(ap_cost_split, ap_lambda), title = "AP Time Series Forecast") + 
    autolayer(InvBoxCox(tslm_ap_fcst$mean, ap_lambda), series = "ap_tslm") + 
    autolayer(InvBoxCox(ap_sarimx_autofcst$mean, ap_lambda), series = "ap_sarimax(auto)") +
    autolayer(InvBoxCox(ets_ap_fcst$mean, ap_lambda), series = "ap_ets") + 
    autolayer(InvBoxCox(ap_arima_fcst$mean, ap_lambda), series = "ap_arima(auto)") +
    autolayer(InvBoxCox(ap_mlp_fcst$mean, ap_lambda), series = "ap_mlp")

  em_plot = autoplot(InvBoxCox(em_cost_split, em_lambda), title = "EM Time Series Forecast") + 
    autolayer(InvBoxCox(tslm_em_fcst$mean, em_lambda), series = "em_tslm") + 
    autolayer(InvBoxCox(em_sarimx_autofcst$mean, em_lambda), series = "em_sarimax(auto)") +
    autolayer(InvBoxCox(ets_em_fcst$mean, em_lambda), series = "em_ets") + 
    autolayer(InvBoxCox(em_arima_fcst$mean, em_lambda), series = "em_arima(auto)") +
    autolayer(InvBoxCox(em_mlp_fcst$mean, em_lambda), series = "em_mlp")

  jn_plot = autoplot(InvBoxCox(jn_cost_split, jn_lambda), title = "JN Time Series Forecast") + 
    autolayer(InvBoxCox(tslm_jn_fcst$mean, jn_lambda), series = "jn_tslm") + 
    autolayer(InvBoxCox(jn_sarimx_autofcst$mean, jn_lambda), series = "jn_sarimax(auto)") +
    autolayer(InvBoxCox(ets_jn_fcst$mean, jn_lambda), series = "jn_ets") + 
    autolayer(InvBoxCox(jn_arima_fcst$mean, jn_lambda), series = "jn_arima(auto)") +
    autolayer(InvBoxCox(jn_mlp_fcst$mean, jn_lambda), series = "jn_mlp")
  
  plot(am_plot)
  plot(ap_plot)
  plot(em_plot)
  plot(jn_plot)
}



```

<br />
<br />

## 2017.01 ~ 2020.01
```{r, echo=FALSE, results='asis', fig.width=13, fig.height=7, warning=FALSE}
manual_function(2017, 1, 2020, 1)

```

<br />
<br />

## 2017.01 ~ 2020.04
```{r, echo=FALSE, results='asis', fig.width=13, fig.height=7, warning=FALSE}
manual_function(2017, 1, 2020, 4)
```

<br />
<br />

## 2017.01 ~ 2020.07
```{r, echo=FALSE, results='asis', fig.width=13, fig.height=7, warning=FALSE}
manual_function(2017, 1, 2020, 7)

```

<br />
<br />


## TSLM IMPROVEMENT
```{r}
#TSLM NORMAL
am_tslm = tslm(am_cost_train ~ am_rev_train + trend + season)
CV(am_tslm)
```


<br />
<br />

```{r}
am_t = time(am_rev_train) #2019.03 = 2019.167
am_time = as.numeric(am_t == as.numeric(am_t)[27])

am_tslm_2 = tslm(am_cost_train ~ am_rev_train + trend + am_time + season)
CV(am_tslm_2) # POLY 2 ## BEST
```

<br />
<br />

```{r}
am_tslm_3 = tslm(am_cost_train ~ am_rev_train + poly(trend,2) + season)
CV(am_tslm_3) # POLY 2 ## BEST

```

<br />
<br />

```{r}
am_tslm_4 = tslm(am_cost_train ~ am_rev_train + poly(trend,3) + season)
CV(am_tslm_4) # POLY 3 

```
