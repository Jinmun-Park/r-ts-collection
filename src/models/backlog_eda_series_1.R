---
title: "backlog_eda_series_1"
output: output: rmarkdown::github_document
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

raw_contract = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_finance = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")
raw_backlog = read.csv("C:/Users/JINMUNPARK/Desktop/***.csv")

```




```{r, echo=FALSE, include=FALSE, result='hide'}
#Check Columns
c(ncol(raw_backlog), nrow(raw_backlog))
raw_backlog %>% colnames() %>% head(10)

#Check Missing Values
sum(is.na(raw_backlog))/(ncol(raw_backlog)*nrow(raw_backlog))

#Add Date
raw_backlog$Date = as.yearmon(paste(raw_backlog$Year, raw_backlog$Month), "%Y %m")

```
<br />
<br />


## OVERVIEW 

Description : Check each monthly data for Revenue, cost and GP.


Observation : 


 1. The peak in every end of quarter for both Revenue and cost are well known pattern for us.


 2. Revenue and cost have very similar patterns in every month. 


 3. GP is therefore having controlled up and down patterns. GP variation in every month is entirely due to the gap between revenue and cost. 


Next Step : 


 1. Find the relationship between Revenue and Cost 


 2. Look into GP 

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

# Down trend in Revenue/Cost across years 
# 2020 seems to have different trend from 2nd quarter.

``` 


<br />
<br />


## REV & COST : OVERVIEW


Description : Check how much different pattern that Revenue and Cost have. 


Conclusion : Date view is obviously telling us that both Rev & Cost have very similar patterns. Quarter view in 2017 and 2018 at Q3 gives a different trend. However, Quarter view is generally giving us same opinion as Date View.



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


## REV & COST : % CHANGE (DATE)


Description : Looking at % Changes in Revenue and Cost to havea clear visualization for its gap.


Conclusion : We can check that every end of first quarter has relatively higher % gap. It looks end year quarter-closing causes a steep slope inlike there have up and down. 


```{r, fig.width=20, fig.height=15, warning=FALSE, echo=FALSE}

revcost_1 = raw_backlog %>%
  group_by(Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost)) %>%
  ungroup() %>%
  select(Date, Revenue, Cost) %>%
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
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  select(Date, rev_change, cost_change) %>%
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


## REV/COST/GP : % CHANGE (M)


Description : -


Conclusion : As stated previously, a change from Feb to Mar has the highest gap between Rev and Cost % change. Additionaly, June to July gap has 3~5% gap across years. 2019 had cyclical behavior on March and November. 2017 and 2018 have big gap on June and July.


Conclusion : %change in Revenue and Cost (Monthly View) does not give precise view. %change in GP shows unstable patterns across year. Its important to notice that it has fluctioation across years.  


```{r, fig.width=20, fig.height=18, warning=FALSE, echo=FALSE}
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

# monthly graph setting
raw_perchange_m_setting = 
  raw_perchange %>%
  group_by(Year, Month) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) %>%
  ungroup() %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) 

revcost_3 = raw_perchange_m_setting %>%
  ggplot(aes(x = Month, y = gp_change, group = factor(Year), label = round(gp_change, digits = 2))) +
  geom_line(size = 1.7, aes(color=factor(Year))) +
  geom_point(size = 4.5, aes(color=factor(Year))) +
  geom_text(size = 5.5, vjust = -1.2) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  labs(x = "Month", y = "GP%") +
  ggtitle("Aggregate GP Percentage Change (Monthly View)") +
  geom_vline(aes(xintercept=as.integer(3)), size = 1.2, linetype = "dashed", color = "steelblue") +
  geom_vline(aes(xintercept=as.integer(6)), size = 1.2, linetype = "dashed", color = "steelblue") +
  geom_vline(aes(xintercept=as.integer(7)), size = 1.2, linetype = "dashed", color = "steelblue") +
  geom_hline(aes(yintercept=as.integer(0)), size = 1, linetype = "dashed", color = "steelblue") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=18), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))  


grid.arrange(revcost_2, revcost_3, nrow=2)
  
```


<br />
<br />


## GP : GP ANALYSIS


### PART 1


Description : Lets look at GP($) and GP(%)


Conclusion : It has dramatic up and down on Dec 2018.


```{r, fig.width=20, fig.height=18, warning=FALSE, echo=FALSE}
raw_perchange_geo = raw_backlog %>%
  group_by(Geo, Date) %>%
  summarise(Revenue = sum(Revenue), Cost = sum(Cost), GP = sum(GP)) %>%
  mutate(rev_change = (Revenue - lag(Revenue))/lag(Revenue) * 100) %>%
  mutate(cost_change = (Cost - lag(Cost))/lag(Cost) * 100) %>%
  mutate(gp_diff = (GP - lag(GP))) %>%
  mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) 

raw_perchange_geo$Year = as.numeric(format(raw_perchange_geo$Date, "%Y"))
raw_perchange_geo$Month = as.numeric(format(raw_perchange_geo$Date, "%m"))

# TS() CONVERSTION
df_rev = raw_backlog %>% group_by(Date) %>% summarise(Revenue = sum(Revenue)) 
df_cost = raw_backlog %>% group_by(Date) %>% summarise(Cost = sum(Cost)) 
df_gp = raw_backlog %>% group_by(Date) %>% summarise(GP = sum(GP)) 
df_gp_diff = df_gp %>% mutate(gp_diff = (GP - lag(GP))) %>% select(c(Date, gp_diff))
df_change = df_gp %>% mutate(gp_change = (GP - lag(GP))/lag(GP) * 100) %>% select(c(Date, gp_change))

ts_rev = ts(df_rev[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ts_cost = ts(df_cost[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ts_gp = ts(df_gp[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ts_gp_diff = ts(df_gp_diff[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ts_gp_change = ts(df_change[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))

# GP Plot
gp_1 = raw_perchange %>%
  ggplot(aes(x = Date, y = GP, label = round(GP, digits = 2))) +
  geom_line(size = 1.7) +
  geom_point(size = 4.5) +
  geom_text(size = 5.5, vjust = -1.2) +
  scale_x_yearmon(format="%Y %m", n=36) +
  labs(x = "Month", y = "GP($)") +
  ggtitle("Aggregate GP ($) (Date View)") +
  geom_vline(aes(xintercept=as.yearmon("Dec 2018")), size = 1, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

gp_2 = raw_perchange %>%
  ggplot(aes(x = Date, y = gp_change, label = round(gp_change, digits = 2))) +
  geom_line(size = 1.7) +
  geom_point(size = 4.5) +
  geom_text(size = 5.5, vjust = -1.2) +
  scale_x_yearmon(format="%Y %m", n=36) +
  labs(x = "Month", y = "GP%") +
  ggtitle("Aggregate GP(%) Channge (Date View)") +
  geom_hline(aes(yintercept=as.integer(0)), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Dec 2018")), size = 1, linetype = "dashed", color = "red") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))  

grid.arrange(gp_1, gp_2, nrow=2)

```


<br />
<br />


### PART 2


Description :GP($) Decomposition


Conclusion : Up-Down Trend and Typical End-Year closing acitivity can be visualized. The bar-graph above tells me that GP % change has some fluctuation. Lets check ACF,PACF.


```{r, fig.width=14, fig.height=8, warning=FALSE, echo=FALSE}

ts_gp_decompose = decompose(ts_gp)
plot(ts_gp_decompose)
```


<br />
<br />


### PART 3


Description : Check ACF/PACF. I want to check seasonality of GP($).


Conclusion : geometric decay in ACF for lag 1, 12, 24. However no significant lag in both ACF & PACF at 12. Its hard to say there are significant seasonality in GP


```{r, fig.width=14, fig.height=8, warning=FALSE, echo=FALSE}
tsdisplay(ts_gp, lag=36)

#geometric decay in ACF for lag 1, 12, 24. However no significant lag in both ACF & PACF at 12
#Its hard to say there are significant seasonality in GP

```


<br />
<br />


### PART 4


Description : Having monthly value for GP% Change in Geo helps understanding Geo pattern.


Conclusion : EM has opposite behavior to the rest Geo. This does not give a clear view


```{r, fig.width=20, fig.height=18, warning=FALSE, echo=FALSE}
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


<br />
<br />


### PART 5


Conclusion : GP Changes($) in EM have dramatic up and down behavior. Something happened in AP from March 2019 to April 2019.



```{r, fig.width=20, fig.height=18, warning=FALSE, echo=FALSE}
revcost_4 = ggplot(raw_perchange_geo, aes(x = Date, y = gp_diff, group = Geo, label = round(gp_diff, digits = 0))) +
  geom_line(size = 1.5, aes(color=Geo)) +
  geom_point(size = 4.5, aes(color=Geo)) +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  scale_x_yearmon(format="%Y %m", n=30) +
  labs(x = "Date", y = "GP($)") +
  ggtitle("GEO GP Difference($) (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Apr 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Nov 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept=as.integer(0)), size = 1, linetype = "dashed", color = "black") 


revcost_5 = ggplot(raw_perchange_geo, aes(x = Date, y = gp_change, group = Geo, label = round(gp_change, digits = 0))) +
  geom_line(size = 1.5, aes(color=Geo)) +
  geom_point(size = 4.5, aes(color=Geo)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=30) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "GP%") +
  ggtitle("GEO GP Percentage Change(%)  (Date View)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7)) +
  geom_vline(aes(xintercept=as.yearmon("Mar 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Apr 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_vline(aes(xintercept=as.yearmon("Nov 2019")), size = 1, linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept=as.integer(0)), size = 1, linetype = "dashed", color = "black") 

grid.arrange(revcost_4, revcost_5, nrow=2)

```


<br />
<br />


## REV/COST/GP : RELATIONSHIP


### PART 1 : CCF


Description : Plot Cross Correlation Function.


Conclusion : CCF implies that Rev has signigicant positive relationshop to both Cost/GP at time lag 0, but it does not show a constant positive relationship. This conclusion could also be made in early ggplot EDA.


CCF REV - COST : At time lag 0, rev is likely to lead to above average value of cost. At time lag -3 to 0 seems to have up and down relationshop with cost. It is hard to say there have one side relationship to each other


CCF REV - GP : At time lag 0, rev is likely to lead to above average value of gp



```{r, fig.width=15, fig.height=8, warning=FALSE, message=FALSE, echo=FALSE}
library(xts)

#adf.test(ts_rev) # P-value : 0.35
#adf.test(ts_cost) # P-value : 0.3735
#adf.test(ts_gp) # P-value : 0.2571

lambda_ts_rev = BoxCox.lambda(ts_rev) 
lambda_ts_cost = BoxCox.lambda(ts_cost) 
lambda_ts_gp = BoxCox.lambda(ts_gp) 

ts_rev_lambda = BoxCox(ts_rev,lambda_ts_rev)
ts_cost_lambda = BoxCox(ts_cost,lambda_ts_cost)
ts_gp_lambda = BoxCox(ts_gp,lambda_ts_gp)

ts_rev_diff = diff(ts_rev_lambda,12)
ts_cost_diff = diff(ts_cost_lambda,12)
ts_gp_diff = diff(ts_gp_lambda,12)

#adf.test(ts_rev_diff) # P-vale : 0.816
#adf.test(ts_cost_diff) # P-vale : 0.6719
#adf.test(ts_gp_diff) # P-vale : 0.6723

ts_rev_difff = diff(diff(ts_rev_lambda,12))
ts_cost_difff = diff(diff(ts_cost_lambda,12))
ts_gp_difff = diff(diff(ts_gp_lambda,12))

adf.test(ts_rev_difff) # P-vale : 0.01
adf.test(ts_cost_difff) # P-vale : 0.02446
adf.test(ts_gp_difff) # P-vale : 0.04884

#TEST IS DONE. GO FOR DIFFERENT FORMAT FOR <CCF>
xts_rev = xts(ts_rev_difff, order.by=as.Date(ts_rev_difff))
xts_cost = xts(ts_cost_difff, order.by=as.Date(ts_cost_difff))
xts_gp = xts(ts_gp_difff, order.by=as.Date(ts_gp_difff))


ccf_rev_cost = ccf(xts_rev[, 1, drop = TRUE], xts_cost[, 1, drop = TRUE], main = "CCF(Rev - Cost)")
# at time lag 0, rev is likely to lead to above average value of cost
# at time lag -3 to 0 seems to have up and down relationshop with cost. It is hard to say there have one side relationship to each other
ccf_rev_gp =  ccf(xts_rev[, 1, drop = TRUE], xts_gp[, 1, drop = TRUE], main = "CCF(Rev - GP)")
# at time lag 0, rev is likely to lead to above average value of gp


# CCF implies that Rev has signigicant positive relationshop to both Cost/GP at time lag 0, but it does not show a constant positive relationship. This conclusion could also be made in early ggplot EDA.
```


<br />
<br />


### PART 2 : SIMPLE VAR

Conclusion : VAR seems not a good choice


Build simple(constant) VAR Regression between Revenue and Cost to check how much credibility we can have using VAR. First, we just build using ts(Revenue) and ts(Cost).


Resuls : All time Lags are insignificant


```{r, fig.width=15, fig.height=8, warning=FALSE, message=FALSE}
library(vars)
var_cbind = cbind(ts_rev, ts_cost)
var_lag = VARselect(var_cbind, lag.max = 24, type = "const")
var_lag$selection #lag 10 

var_revcost = VAR(var_cbind, p = 10, type = "const", season = NULL, exog = NULL)
summary(var_revcost) # All TIME-LAGs are insignificant

```


<br />
<br />


Build simple VAR Regression using First differenciated non-seasonal Revenue and Cost 


Result : Only Time Lag 1 and 2 are significant.


```{r, fig.width=15, fig.height=8, warning=FALSE, message=FALSE}
var_diff_cbind = cbind(ts_rev_difff, ts_cost_difff)
var_diff_lag = VARselect(var_diff_cbind, lag.max = 24, type = "const")
var_diff_lag$selection #lag 4

var_diff_revcost = VAR(var_diff_cbind, p = 4, type = "const", season = NULL, exog = NULL)
summary(var_diff_revcost) # Only TIME-LAG 2 is significant
```


<br />
<br />


Build simple VAR Regressionusing First differenciated non-seasonal Revenue and GP


Result : Only Time Lag 1 and 2 are significant.


```{r, fig.width=15, fig.height=8, warning=FALSE, message=FALSE}
var_diff_cbind_2 = cbind(ts_rev_difff, ts_gp_difff)
var_diff_lag_2 = VARselect(var_diff_cbind_2, lag.max = 24, type = "const")
var_diff_lag_2$selection #lag 4

var_diff_revcost_2 = VAR(var_diff_cbind_2, p = 4, type = "const", season = NULL, exog = NULL)
summary(var_diff_revcost_2) # Only TIME-LAG 2 is significant

```


<br />
<br />


## REV/COST/GP : GARCH-MODEL


### EDP - Seasonally Adjusted

NOPE


```{r, fig.width=12, fig.height=6, warning=FALSE, message=FALSE, echo=FALSE}
d_rev = density(ts_rev_diff)
d_cost = density(ts_cost_diff) 
d_gp = density(ts_gp_diff)

plot(d_rev$x, d_rev$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(Rev))", las=1)
plot(d_cost$x, d_cost$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(cost))", las=1)
plot(d_gp$x, d_gp$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(gp))", las=1)

```


### EDP - First Differenciated

YES


```{r, fig.width=12, fig.height=6, warning=FALSE, message=FALSE, echo=FALSE}
dd_rev = density(ts_rev_difff)
dd_cost = density(ts_cost_difff) 
dd_gp = density(ts_gp_difff)

plot(dd_rev$x, dd_rev$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(diff(Rev))", las=1)
plot(dd_cost$x, dd_cost$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(diff(Cost))", las=1)
plot(dd_gp$x, dd_gp$y, type="l", xlab="log returns", ylab="density", main="Empirical Density Plot (diff(diff(GP))", las=1)
```


### QQNORM 

I just realized there are not many time-series data. It has too low data to draw any GARCH PREDICTION.


```{r, fig.width=12, fig.height=6, warning=FALSE, message=FALSE, echo=FALSE}
qqnorm(ts_rev_diff, main="Normal Q-Q Plot of revenue return", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(ts_rev_diff, col=2, lty=5, lwd=2) # normal but low time-series ata

qqnorm(ts_cost_diff, main="Normal Q-Q Plot of cost return", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(ts_cost_diff, col=2, lty=5, lwd=2) # normal but low time-series ata

qqnorm(ts_gp_diff, main="Normal Q-Q Plot of gp return", xlab = "Theoretical Quantiles", 
       ylab = "Sample Quantiles", las=1)
qqline(ts_gp_diff, col=2, lty=5, lwd=2) # normal but low time-series ata
```
