---
title: "backlog_eda_series_2"
author: "JINMUN PARK"
date: "11/4/2020"
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
library(TSstudio)
#library(reticulate)
#devtools::install_github("rstudio/tensorflow")
#library(tensorflow)
#install_tensorflow(conda="tensorenviron")
#library(keras)



# RAW DATA IMPORT
raw_contract = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_finance = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_backlog = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")


# RAW DATA : ADD <DATE>
raw_backlog$Date = as.yearmon(paste(raw_backlog$Year, raw_backlog$Month), "%Y %m")


# CREATE : ROW % DATA (DATE)
raw_perchange_setting = raw_backlog %>%
  group_by(Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) 

raw_perchange_date = raw_backlog %>%
  group_by(Date) %>%
  mutate(Revenue = sum(Revenue)) %>%
  mutate(Cost = sum(Cost)) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date) %>%
  subset(select = c(Year, Month, Quarter))

raw_perchange = cbind(raw_perchange_setting, raw_perchange_date)


# CREATE : ROW % DATA (MONTHLY)
raw_perchange_m_setting = 
  raw_perchange %>%
  group_by(Year, Month) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) %>%
  ungroup() %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) 


# CREATE : ROW % DATA (GEO + MONTHLY)
raw_perchange_geo = raw_backlog %>%
  group_by(Geo, Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  mutate(gp_diff = (GP - lag(GP))) %>%
  mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) 

raw_perchange_geo$Year = as.numeric(format(raw_perchange_geo$Date, "%Y"))
raw_perchange_geo$Month = as.numeric(format(raw_perchange_geo$Date, "%m"))


```


<br />
<br />


## OVERVIEW 


Description : I have two missions in this project.

  
  1. Building Multivariate Time Series Forecast using Revenue variable. 
  
  
  2. Building Univariate Time Series Forecast using Cost itself.


I have very simple hypthesis here that multivariate time series would have a better accuracy than the univariate as i have seen such a great correlate movement between Revenue and Cost. 

I will try building simple regression and time series models that only consider trend and seasonality. These models can be improved if we go through several steps to adjust models, but i will 

not go through that steps here. 


<br />
<br />


## PREVIOUS EDA

This is the summary in the last week



### DATA OVERVIEW


Look closer for Rev/Cost


```{r, fig.width=15, fig.height=5, warning=FALSE, echo=FALSE}
p1 = raw_backlog %>%
  group_by(Year, Month) %>%
  summarise(Revenue = sum(Revenue)) %>%
  ungroup() %>%
  ggplot(aes(x = Month, y = Revenue, color = factor(Year), label = round(Revenue, digits = 0))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Month", y = "Revenue") +
  ggtitle("Aggregate Revenue Graph (Monthly View)") +
  theme(plot.title = element_text(hjust = 0.5))

p2 = raw_backlog %>%
  group_by(Year, Month) %>%
  summarise(Cost = sum(Cost)) %>%
  ungroup() %>%
  ggplot(aes(x = Month, y = Cost, color = factor(Year), label = round(Cost, digits = 0))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Month", y = "Cost") +
  ggtitle("Aggregate Cost Graph (Monthly View)") +
  theme(plot.title = element_text(hjust = 0.5))

p3 = raw_backlog %>%
  group_by(Year, Month) %>%
  summarise(GP = sum(GP)) %>%
  ungroup() %>%
  ggplot(aes(x = Month, y = GP, color = factor(Year), label = round(GP, digits = 0))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Month", y = "GP") +
  ggtitle("Aggregate GP Graph (Monthly View)") +
  theme(plot.title = element_text(hjust = 0.5))

p1
p2
p3

```

<br />
<br />


### REV/COST OVERVIEW


Quarter view may be a bit of different. Lets look into Monthly view.


```{r, fig.width=20, fig.height=12, warning=FALSE, echo=FALSE}
rev_1 = raw_backlog %>%
  group_by(Date, Year, Month) %>%
  summarise(Revenue = sum(Revenue)) %>%
  ungroup() %>%
  ggplot(aes(x = Date, y = Revenue, label = round(Revenue, digits = 0))) +
  geom_line(size = 1.2) +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  labs(x = "Month", y = "Revenue") +
  ggtitle("Aggregate Rev (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

rev_2 = raw_backlog %>%
  group_by(Year, Quarter) %>%
  summarise(Revenue = sum(Revenue)) %>%
  ungroup() %>%
  ggplot(aes(x = Quarter, y = Revenue, color = factor(Year), label = round(Revenue, digits = 0))) +
  geom_line(size = 1.2) +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  labs(x = "Quarter", y = "Rev") +
  ggtitle("Aggregate Rev (Quarter View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) +
  geom_vline(aes(xintercept=as.numeric(3)), size = 1.5, linetype = "dotted")

cost_1 = raw_backlog %>%
  group_by(Date, Year, Month) %>%
  summarise(Cost = sum(Cost)) %>%
  ungroup() %>%
  ggplot(aes(x = Date, y = Cost, label = round(Cost, digits = 0))) +
  geom_line(size = 1.2) +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  labs(x = "Month", y = "Cost") +
  ggtitle("Aggregate Cost (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18))

cost_2 = raw_backlog %>%
  group_by(Year, Quarter) %>%
  summarise(Cost = sum(Cost)) %>%
  ungroup() %>%
  ggplot(aes(x = Quarter, y = Cost, color = factor(Year), label = round(Cost, digits = 0))) +
  geom_line(size = 1.2) +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  geom_point(size = 3) +
  theme_bw() +
  geom_text(size = 4, vjust = -1.2) +
  labs(x = "Quarter", y = "Cost") +
  ggtitle("Aggregate Cost (Quarter View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 18)) + 
  geom_vline(aes(xintercept=as.numeric(3)), size = 1.5, linetype = "dotted")

grid.arrange(rev_1, rev_2, cost_1, cost_2, ncol=2, nrow=2)
```

<br />
<br />


### CLOSE UP : REV/COST 


They are really having very realtive movement each other.

Take a look at <March 2019> and <March 2020>.


```{r, fig.width=20, fig.height=12, warning=FALSE, echo=FALSE}

revcost_1 = raw_backlog %>%
  group_by(Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost)) %>%
  ungroup() %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 0))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Month", y = "Rev & Cost") +
  ggtitle("Aggregate Rev & Cost (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2017")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2018")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2020")), size = 1, linetype = "dashed", color = "red") 
  

raw_backlog_change = raw_backlog %>%
  group_by(Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost)) %>%
  ungroup() %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100)  %>%
  dplyr::select(Date, rev_change, cost_change) %>%
  gather(key = "variable", value = "value", -Date) 

  
revcost_2 = ggplot(raw_backlog_change, aes(x = Date, y = value, group = variable, label = round(value, digits = 0))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=36) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Month", y = "Rev & Cost") +
  ggtitle("Aggregate Rev & Cost Percentage Change (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2017")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2018")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2020")), size = 1, linetype = "dashed", color = "red") 


grid.arrange(revcost_1, revcost_2, nrow=2)


```

<br />
<br />


### CLOSE UP : REV/COST 



When we look inot Geo view, it seems the Geo forecast using multivariate time-seris is not positive.



```{r, fig.width=20, fig.height=15, warning=FALSE, echo=FALSE}
raw_perchange_geo_2 = raw_perchange_geo %>%  
  dplyr::select(Date, Geo, rev_change, cost_change) %>%
  gather(key = "variable", value = "value", -Date, -Geo) 


raw_perchange_geo_2 %>% 
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  facet_wrap(~Geo, ncol = 1, scales="free") +
  geom_line(size = 1.7, aes(color=factor(variable))) +
  geom_point(size = 4.5, aes(color=factor(variable))) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 4, vjust = -1.2) +
  labs(x = "Month", y = "GP%") +
  ggtitle("GEO Rev/Cost Percentage Change (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=18), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Mar 2020")), size = 1.2, linetype = "dashed", color = "red") 


 
```




```{r, fig.width=20, fig.height=12, warning=FALSE, echo=FALSE}
raw_perchange_geo %>% 
  ggplot(aes(x = Month, y = gp_change, group = factor(Year), label = round(gp_change, digits = 2))) +
  facet_wrap(~Geo, ncol = 1, scales="free") +
  geom_line(size = 1.7, aes(color=factor(Year))) +
  geom_point(size = 4.5, aes(color=factor(Year))) +
  geom_text(size = 5.5, vjust = -1.2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Month", y = "GP%") +
  geom_vline(aes(xintercept=as.integer(3)), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.integer(6)), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.integer(7)), size = 1, linetype = "dashed", color = "red") +
  ggtitle("GEO GP Percentage Change (Monthly View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=18), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))  

```

### TS CONNVERSION : BOX-COX and First Differenciate


Two conversionts are made : Box-Cox Transformation


1. BOX-COX Transformation without Seasonality adjusted

2. BOX-COX Transformation with Seasonality adjusted



```{r, warning=FALSE, message=FALSE, echo=FALSE}
# CREATE : TIME-SERIES CONVERTION
df_date = raw_backlog %>% group_by(Date) %>% summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) 
ts_date = ts(df_date[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))

ts_rev = ts_date[,1]
ts_cost = ts_date[,2]
ts_gp = ts_date[,3]

# CREATE : BOX-COX TRANSFORMATION 
lambda_rev = BoxCox.lambda(ts_rev) #InvBoxCox(ts_lambda_rev, lambda_rev)
lambda_cost = BoxCox.lambda(ts_cost)
lambda_gp = BoxCox.lambda(ts_gp)

# CREATE : FIRST DIFFERENCIATED 
ts_lambda_rev = BoxCox(ts_rev, lambda_rev)
ts_lambda_cost = BoxCox(ts_cost, lambda_cost)
ts_lambda_gp = BoxCox(ts_gp, lambda_gp)

ts_box_rev = diff(ts_lambda_rev)
ts_box_cost = diff(ts_lambda_cost)
ts_box_gp = diff(ts_lambda_gp)

#adf.test(ts_box_rev) # P-VALUE : 0.01
#adf.test(ts_box_cost) # P-VALUE : 0.01 
#adf.test(ts_box_gp) # P-VALUE : 0.01

# CREATE : SEASONALY ADJUSTED - FIRST DIFFERENCIATED 
ts_box_db_rev = diff(diff(ts_lambda_rev, 12))
ts_box_db_cost = diff(diff(ts_lambda_cost, 12))
ts_box_db_gp = diff(diff(ts_lambda_gp, 12))

adf.test(ts_box_db_rev) # P-VALUE : 0.01
adf.test(ts_box_db_cost) # P-VALUE : 0.02 
adf.test(ts_box_db_gp) # P-VALUE : 0.04

```


<br />
<br />


### TS CONNVERSION : BOX-COX and First Differenciate

```{r, fig.width=15, fig.height=12, warning=FALSE, echo=FALSE}
par(mfrow=c(3,1))
plot(ts_rev, main = "Time Series Revenue")
plot(ts_box_rev, main = "Time Series Seasonally Difference after Box-Cox")
plot(ts_box_db_rev, main = "Time Series first differenced")


```


<br />
<br />


## TIME-SERIES MODEL FOR AGGREGATE VIEW 


This is the list of model that i would like to try for multivariate TS. These models are built at the simplest way, minumum effort is made to improve model fit/accuracy. I will select few models that have best accuracy to continue forecasting across GEO. Model improvement will be made at GEO forecast stage.

<br />
<br />


### MULTIVARIATE 

  * VAR 
  * TSLM (Linear Regression)
  * Random Forest
  * SARIMAX

  
<br />


### TRAIN & TEST


TRAIN : start= c(2017,2), end= c(2020,6)

TEST : start= c(2020,7), end= c(2020,9)


```{r, echo=FALSE, include=FALSE, result='hide'}
train_box_rev = window(ts_box_rev, start= c(2017,2), end= c(2020,6))
test_box_rev = window(ts_box_rev, start= c(2020,7), end= c(2020,9))
train_db_rev = window(ts_box_db_rev, start= c(2017,2), end= c(2020,6))
test_db_rev = window(ts_box_db_rev, start= c(2020,7), end= c(2020,9))

train_box_cost = window(ts_box_cost, start= c(2017,2), end= c(2020,6))
test_box_cost = window(ts_box_cost, start= c(2020,7), end= c(2020,9))
train_db_cost = window(ts_box_db_cost, start= c(2017,2), end= c(2020,6))
test_db_cost = window(ts_box_db_cost, start= c(2020,7), end= c(2020,9))

train_box_gp = window(ts_box_gp, start= c(2017,2), end= c(2020,6))
test_box_gp = window(ts_box_gp, start= c(2020,7), end= c(2020,9))
train_db_gp = window(ts_box_db_cost, start= c(2017,2), end= c(2020,6))
test_db_gp = window(ts_box_db_cost, start= c(2020,7), end= c(2020,9))
```


<br />
<br />

###  SIMPLE VAR


Description : Various test after VAR Modelling, seasonally adjusted time series gives a better model fitting.


Conclusion : MAPE : 16.19 (I did not even expect good result and its not good at all.)


```{r, fig.width=12, fig.height=5, warning=FALSE, echo=FALSE}

# TEST : VAR SELECTION (OPT 1 : SEASONALITY)
col_revcost = cbind(train_box_rev, train_box_cost)
#VARselect(col_revcost, lag.max = 9, type = "const") # Lag : 2
var_revcost = VAR(col_revcost, p = 2, type = "const", season = 12, exog = NULL)

#summary(var_revcost) # LOOKS GOOD

# TEST : VAR SELECTION (OPT 2 : SEASONALLY ADJUSTED)
col_revcost_2 = cbind(train_db_rev, train_db_cost)
VARselect(col_revcost_2, lag.max = 4, type = "const") # Lag : 2
var_revcost_2 = VAR(col_revcost_2, p = 2, type = "const", exog = NULL)

#summary(var_revcost_2) # LOOKS GOOD TOO ! BETTER THAN OPTION 1


# TEST : RESIDUAL TEST
serial.test(var_revcost, lags.pt = 12, type = "PT.asymptotic") # P VALUE : 0.36 ~ absence of serial correlation
normality.test(var_revcost, multivariate.only = TRUE) # Normally distributed
#causality(var_revcost, cause = "train_box_rev") #It implies past REVENUE does not influence to predict today’s COST.

#serial.test(var_revcost_2, lags.pt = 12, type = "PT.asymptotic") # P VALUE : 0.64 ~ absence of serial correlation
#normality.test(var_revcost_2, multivariate.only = TRUE) # Normally distributed
#causality(var_revcost_2, cause = "train_db_rev") #It implies past REVENUE influences to predict today’s COST.


# PLOT : FEVD PLOT
plot(fevd(var_revcost, n.ahead = 3)) # But it still worth to predict
#plot(fevd(var_revcost_2, n.ahead = 3)) # But it still worth to predict 


# FORECAST 
var_forecast = predict(var_revcost, n.ahead=3) # Model without Seasonally Adjusted
var_forecast_2 = predict(var_revcost_2, n.ahead=3) # Model Seasonally Adjusted

ac_1 = accuracy(test_box_cost, var_forecast$fcst$train_box_cost[,1])
accuracy(test_box_cost, var_forecast$fcst$train_box_cost[,1]) #MAPE : 16.19

#accuracy(test_db_cost, var_forecast_2$fcst$train_db_cost[,1]) #MAPE : 66.19 #EXPECTED

```


<br />
<br />

### SIMPLE REGRESSION : MODELLING


Conclusion : GOOD MODEL FIT 


```{r, fig.width=15, fig.height=5, message=FALSE, warning=FALSE, echo=FALSE}

# TIME-SERIES CONVERSION
ts_lambda_rev_train = window(ts_lambda_rev, start = c(2017,1), end = c(2020,6))
ts_lambda_rev_test = window(ts_lambda_rev, start = c(2020,7), end = c(2020,9))

ts_lambda_cost_train = window(ts_lambda_cost, start = c(2017,1), end = c(2020,6))
ts_lambda_cost_test = window(ts_lambda_cost, start = c(2020,7), end = c(2020,9))

ts_lambda_gp_train = window(ts_lambda_gp, start = c(2017,1), end = c(2020,6))
ts_lambda_gp_test = window(ts_lambda_gp, start = c(2020,7), end = c(2020,9))

simple_tslm = tslm(ts_lambda_cost_train ~ ts_lambda_rev_train + trend + season)
summary(simple_tslm)

# PLOT
autoplot(ts_lambda_cost_train, series="Cost Data") +
  autolayer(fitted(simple_tslm), series="Fitted") +
  xlab("Year") + ylab("") +
  ggtitle("TSLM Cost Model Fit") +
  guides(colour=guide_legend(title=" "))

cbind(Data = ts_lambda_cost_train, Fitted = fitted(simple_tslm)) %>%
  as.data.frame() %>%
  ggplot(aes(x=Data, y=Fitted)) +
  geom_point() +
  ylab("Fitted (predicted values)") +
  xlab("Data (actual values)") +
  ggtitle("TSLM Cost (Fitted vs Actual") +
  geom_abline(intercept=0, slope=1)


# PLOT
checkresiduals(simple_tslm)

```

<br />

### SIMPLE REGRESSION : FORECAST


CONCLUSION : MAPE 0.68


```{r, fig.width=15, fig.height=5, warning=FALSE, echo=FALSE}
simple_tslm_fcst = forecast(simple_tslm, newdata = ts_lambda_rev_test)

accuracy(ts_lambda_cost_test, simple_tslm_fcst$mean) #MAPE : 0.0018

ac_2 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(simple_tslm_fcst$mean, lambda_cost))
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(simple_tslm_fcst$mean, lambda_cost)) # MAPE : 0.68

```


<br />
<br />


### Random Forest

Conclusion : MAPE 6.3771


```{r, warning=FALSE, message=FALSE, echo=FALSE}
set.seed(2020)

rf_model = randomForest(ts_lambda_cost_train ~ ts_lambda_rev_train, ntree = 1000, mtry = 3, nodesize = 5, importance = TRUE)
rf_fcst = predict(rf_model)[1:3]
rf_fcst = ts(rf_fcst, frequency = 12, start=c(2020,7), end= c(2020,9))

ac_3 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(predict(rf_model)[1:3], lambda_cost)) 
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(predict(rf_model)[1:3], lambda_cost)) # MAPE : 6.3771

```


<br />
<br />


### SARIMAX


Conclusion : MAPE 0.80


```{r, fig.width=15, fig.height=5, warning=FALSE, message=FALSE, echo=FALSE}
sarimx_model = auto.arima(ts_lambda_cost_train, D = 1, xreg = ts_lambda_rev_train)
sarimx_forecast = forecast(sarimx_model, xreg = ts_lambda_rev_test)

ac_4 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(sarimx_forecast$mean, lambda_cost)) 
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(sarimx_forecast$mean, lambda_cost)) # MAPE : 0.80

```


<br />
<br />


### MULTIVARIATE TIME-SERIES


```{r, echo=FALSE, results='asis', fig.width=15, fig.height=5, warning=FALSE}
library(knitr)
sm_1 = rbind(ac_1[1,1:5], ac_2[1,1:5], ac_3[1,1:5], ac_4[1,1:5])
rownames(sm_1) = c("VAR", "TSLM", "RF", "SARIMAX")

kable(sm_1)
```

<br />
<br />

```{r, echo=FALSE, results='asis', fig.width=15, fig.height=8, warning=FALSE}
autoplot(ts_cost, series = "Cost Original Data") +
  autolayer(InvBoxCox(simple_tslm_fcst$mean, lambda_cost), series="TSLM", size = 1.2) +
  autolayer(InvBoxCox(rf_fcst, lambda_cost), series="RF", size = 1.2) +
  autolayer(InvBoxCox(sarimx_forecast$mean, lambda_cost), series="SARIMAX", size = 1.2) +
  xlab("Year") + ylab("") +
  ggtitle("Multivariate Time Series Forecast") +
  guides(colour=guide_legend(title=" ")) +
  geom_line(size = 0.8) +
  geom_point()
```

<br />
<br />
<br />


### UNI-VARIATE TIME-SERIES


  * CLASSICAL TS Forecast 
  
  * TSLM (Linear Regression)
  
  * Random Forest
  
  * SARIMAX
  

<br />
<br />

### CLASSICAL APPROACH

 * ETS : MAPE 1.099144
 
 * BEG_ETS : MAPE 1.321723
 
 * HOT_WINTER : MAPE 1.140164
 
 * NEURAL_NT : MAPE 0.6281316
 
 * TBATS :  MAPE 1.970271


<br />


```{r, warning=FALSE, message=FALSE, echo=FALSE}
ets_model = ets(ts_lambda_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)
ets_fcst = forecast(ets_model, h = 3)

begets_model = baggedETS(ts_lambda_cost_train, model = "ZZZ", allow.multiplicative.trend = TRUE, damped=TRUE)
begets_fcst = forecast(begets_model, h = 3)

hw_model = hw(ts_lambda_cost_train, damped = TRUE, seasonal = "multiplicative", h=3)
hw_fcst = forecast(hw_model, h = 3)

nn_model = nnetar(ts_lambda_cost_train)
nn_fcst = forecast(nn_model, h = 3)

tbats_model = tbats(ts_lambda_cost_train, biasadj=TRUE)
tbats_fcst = forecast(tbats_model, h = 3)

ac5 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(ets_fcst$mean, lambda_cost)) # ETS
ac6 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(begets_fcst$mean, lambda_cost)) # BEG_ETS
ac7 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(hw_fcst$mean, lambda_cost)) # HOT_WINTER
ac8 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(nn_fcst$mean, lambda_cost)) # NEURAL_NT
ac9 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(tbats_fcst$mean, lambda_cost)) # TBATS 

accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(ets_fcst$mean, lambda_cost)) # MAPE : 1.099144
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(begets_fcst$mean, lambda_cost)) # MAPE : 1.321723 
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(hw_fcst$mean, lambda_cost)) # MAPE : 1.140164 
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(nn_fcst$mean, lambda_cost)) # MAPE : 0.6281316 
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(tbats_fcst$mean, lambda_cost)) # MAPE : 1.970271 

```


<br />
<br />

### ARIMA

Conclusion : MAPE 1.99


```{r, warning=FALSE, message=FALSE, echo=FALSE}
arima_model = auto.arima(ts_lambda_cost_train, D = 1)
arima_forecast = forecast(arima_model, h = 3)

ac10 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(arima_forecast$mean, lambda_cost)) # ARIMA
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(arima_forecast$mean, lambda_cost)) # MAPE : 1.95
```

<br />
<br />

### RANDOM FOREST

Conclusion : MAPE 1.63


```{r, warning=FALSE, message=FALSE, echo=FALSE}
lag_order = 6
horizon = 3

rf_unimodel = embed(ts_lambda_cost, lag_order + 1)

y_train = rf_unimodel[, 1] # the target
X_train = rf_unimodel[, -1] # everything but the target
y_test = ts_lambda_cost_test # Test
X_test = rf_unimodel[nrow(rf_unimodel), c(1:lag_order)]

forecasts_rf = numeric(horizon)


for (i in 1:horizon){
  # set seed
  set.seed(2020)
  # fit the model
  fit_rf = randomForest(X_train, y_train)
  # predict using the test set
  forecasts_rf[i] = predict(fit_rf, X_test)
  # here is where we repeatedly reshape the training data to reflect the time distance
  # corresponding to the current forecast horizon.
  y_train = y_train[-1] 
  X_train = X_train[-nrow(X_train), ] 
}

ac11 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(forecasts_rf, lambda_cost)) # RANDOM FOREST
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(forecasts_rf, lambda_cost)) # MAPE : 1.63

```


<br />
<br />

### LSTM MODELLING (X)

CONCLUSION : Tensorflow() library has some error.


```{r, warning=FALSE, message=FALSE, echo=FALSE, include=FALSE}
#lag_transform = function(x, k= 1){
#    
#      lagged =  c(rep(NA, k), x[1:(length(x)-k)])
#      DF = as.data.frame(cbind(lagged, x))
#      colnames(DF) = c( paste0('x-', k), 'x')
#      DF[is.na(DF)] = 0
#      return(DF)
#}
```


```{r, warning=FALSE, message=FALSE, echo=FALSE, include=FALSE}
#supervised = lag_transform(ts_lambda_cost, 1)

#lstm_n = nrow(supervised)
#lstm_n_qtr = nrow(supervised)-3

#lstm_train = supervised[1:lstm_n_qtr, ]
#lstm_test = supervised[(lstm_n_qtr+1):lstm_n, ]

# SHAPING
#dim(lstm_test) = c(length(lstm_test), 1, 1)

#X_shape2 = dim(lstm_test)[2]
#X_shape3 = dim(lstm_test)[3]
#batch_size = 1
#tsteps = 1

#model = keras_model_sequential()

#model%>%
  #layer_lstm(units = 50, batch_input_shape = c(batch_size, X_shape2, X_shape3), stateful= TRUE) %>%
  #layer_dense(units = 1)
```

<br />
<br />

### MLP NNF


Conclusion : MAPE 2.63

```{r, warning=FALSE, message=FALSE, echo=FALSE}
mlp_model = mlp(ts_lambda_cost_train, difforder=1)
#plot(mlp_model)

mlp_fcst = forecast(mlp_model, h = horizon)

ac12 = accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(mlp_fcst$mean, lambda_cost)) # MLP
accuracy(InvBoxCox(ts_lambda_cost_test, lambda_cost), InvBoxCox(mlp_fcst$mean, lambda_cost)) # MAPE : 2.63
```


<br />
<br />


### SUMMARY (UNIVARIATE TIME-SERIES)

```{r, echo=FALSE, results='asis', fig.width=15, fig.height=8, warning=FALSE}
library(knitr)
sm_2 = rbind(ac5[1,1:5], ac6[1,1:5], ac7[1,1:5], ac8[1,1:5], ac9[1,1:5], ac10[1,1:5], ac11[1,1:5], ac12[1,1:5])
rownames(sm_2) = c("ETS", "BEG_ETS", "HOT_WINTER", "NEURAL_NT" ,"TBATS", "ARIMA", "R.FOREST", "MLP")

kable(sm_2)

```

<br />
<br />

```{r, echo=FALSE, results='asis', fig.width=15, fig.height=8, warning=FALSE}
autoplot(ts_cost, series = "Cost Original Data") +
  autolayer(InvBoxCox(ets_fcst$mean, lambda_cost), series="ETS", size = 1.2) +
  autolayer(InvBoxCox(hw_fcst$mean, lambda_cost), series="Hot-Winter", size = 1.2) +
  autolayer(InvBoxCox(nn_fcst$mean, lambda_cost), series="Neural N", size = 1.2) +
  autolayer(InvBoxCox(mlp_fcst$mean, lambda_cost), series="MLP", size = 1.2) +
  xlab("Year") + ylab("") +
  ggtitle("Univariate Time Series Forecast(Selected Model Only)") +
  guides(colour=guide_legend(title=" ")) +
  geom_line(size = 0.8) +
  geom_point()
```


<br />
<br />
<br />


##  AGGREGATR COST FORECAST SUMMARY

```{r, echo=FALSE, results='asis', fig.width=15, fig.height=8, warning=FALSE}
sm_3 = rbind(ac_2[1,1:5], ac_4[1,1:5], ac5[1,1:5], ac10[1,1:5], ac12[1,1:5])
rownames(sm_3) = c("TSLM", "SARIMAX", "ETS", "ARIMA", "MLP")
sm_3_index = data.frame("type" = c("MULTIVARIATE", "MULTIVARIATE", "UNIVARIATE", "UNIVARIATE", "UNIVARIATE"))
sm_3 = cbind(sm_3_index, sm_3)

kable(sm_3)

```

<br />
<br />
