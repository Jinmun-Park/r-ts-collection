---
title: "backlog_eda_series_4"
author: "JINMUN PARK"
date: "11/23/2020"
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
raw_update = raw_update[,c(5,1,2,3,4)] 

raw_bcklg_update = read.csv("C:/Users/JINMUNPARK/Desktop/Temporary/10_GTS_RUNOUT_EDA/Update_bcklg.csv")
raw_bcklg_update$DateQtr = as.character(raw_bcklg_update$DateQtr)


# CREATE : REV QUARTER VIEW
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

agr_geo_rev$Year = year(agr_geo_rev$Date)
agr_geo_rev$Month = month(agr_geo_rev$Date)
agr_geo_rev$Quarter = quarter(agr_geo_rev$Month)
agr_geo_rev$QuarterW = paste("Q", agr_geo_rev$Quarter, sep="")
agr_geo_rev$DateQtr = paste(agr_geo_rev$Year, agr_geo_rev$QuarterW, sep="")
agr_geo_rev[is.na(agr_geo_rev)] = 0

agr_geoqtr_rev = agr_geo_rev[,c(2:8,10)]
agr_geoqtr_rev = agr_geoqtr_rev %>% group_by(DateQtr, Year, Quarter) %>% summarise_each(funs(sum))


# CREATE : COST QUARTER VIEW
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
agr_geo_cost$Year = year(agr_geo_cost$Date)
agr_geo_cost$Month = month(agr_geo_cost$Date)
agr_geo_cost$Quarter = quarter(agr_geo_cost$Month)
agr_geo_cost$QuarterW = paste("Q", agr_geo_cost$Quarter, sep="")
agr_geo_cost$DateQtr = paste(agr_geo_cost$Year, agr_geo_cost$QuarterW, sep="")
agr_geo_cost[is.na(agr_geo_cost)] = 0

agr_geoqtr_cost = agr_geo_cost[,c(2:8,10)]
agr_geoqtr_cost = agr_geoqtr_cost %>% group_by(DateQtr, Year, Quarter) %>% summarise_each(funs(sum))

```


```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
eda_am_geoqtr = merge(agr_geoqtr_rev[,c(1:3,4)], raw_bcklg_update[,c(1,4)], by = "DateQtr", all.x = TRUE)
eda_am_geoqtr = merge(eda_am_geoqtr, agr_geoqtr_cost[,c(1,4)], by = "DateQtr", all.x = TRUE)
eda_am_geoqtr$yearqtr = as.yearqtr(paste(eda_am_geoqtr$Year, eda_am_geoqtr$Quarter, sep="-"))
eda_am_geoqtr = eda_am_geoqtr %>% subset(select = -c(DateQtr, Year, Quarter))

eda_ap_geoqtr = merge(agr_geoqtr_rev[,c(1:3,5)], raw_bcklg_update[,c(1,5)], by = "DateQtr", all.x = TRUE)
eda_ap_geoqtr = merge(eda_ap_geoqtr, agr_geoqtr_cost[,c(1,5)], by = "DateQtr", all.x = TRUE)
eda_ap_geoqtr$yearqtr = as.yearqtr(paste(eda_ap_geoqtr$Year, eda_ap_geoqtr$Quarter, sep="-"))
eda_ap_geoqtr = eda_ap_geoqtr %>% subset(select = -c(DateQtr, Year, Quarter))

eda_em_geoqtr = merge(agr_geoqtr_rev[,c(1:3,6)], raw_bcklg_update[,c(1,6)], by = "DateQtr", all.x = TRUE)
eda_em_geoqtr = merge(eda_em_geoqtr, agr_geoqtr_cost[,c(1,6)], by = "DateQtr", all.x = TRUE)
eda_em_geoqtr$yearqtr = as.yearqtr(paste(eda_em_geoqtr$Year, eda_em_geoqtr$Quarter, sep="-"))
eda_em_geoqtr = eda_em_geoqtr %>% subset(select = -c(DateQtr, Year, Quarter))

eda_jn_geoqtr = merge(agr_geoqtr_rev[,c(1:3,7)], raw_bcklg_update[,c(1,7)], by = "DateQtr", all.x = TRUE)
eda_jn_geoqtr = merge(eda_jn_geoqtr, agr_geoqtr_cost[,c(1,7)], by = "DateQtr", all.x = TRUE)
eda_jn_geoqtr$yearqtr = as.yearqtr(paste(eda_jn_geoqtr$Year, eda_jn_geoqtr$Quarter, sep="-"))
eda_jn_geoqtr = eda_jn_geoqtr %>% subset(select = -c(DateQtr, Year, Quarter))

eda_am_geoqtr %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 1))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Amount($)") +
  ggtitle("AM Rev/Cost/Backlog ($) Amount") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_ap_geoqtr %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 1))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Amount($)") +
  ggtitle("AP Rev/Cost/Backlog ($) Amount") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_em_geoqtr %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 1))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Amount($)") +
  ggtitle("EM Rev/Cost/Backlog ($) Amount") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_jn_geoqtr %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 1))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Amount($)") +
  ggtitle("JN Rev/Cost/Backlog ($) Amount") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

```



```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
eda_am_geoqtr$BCKLGREV = eda_am_geoqtr$AM_REV/eda_am_geoqtr$AM_BCKLG
eda_am_geoqtr$BCKLGCOST = eda_am_geoqtr$AM_COST/eda_am_geoqtr$AM_BCKLG
eda_am_ratio = eda_am_geoqtr %>% subset(select = c(yearqtr, BCKLGREV, BCKLGCOST))

eda_ap_geoqtr$BCKLGREV = eda_ap_geoqtr$AP_REV/eda_ap_geoqtr$AP_BCKLG
eda_ap_geoqtr$BCKLGCOST = eda_ap_geoqtr$AP_COST/eda_ap_geoqtr$AP_BCKLG
eda_ap_ratio = eda_ap_geoqtr %>% subset(select = c(yearqtr, BCKLGREV, BCKLGCOST))

eda_em_geoqtr$BCKLGREV = eda_em_geoqtr$EM_REV/eda_em_geoqtr$EM_BCKLG
eda_em_geoqtr$BCKLGCOST = eda_em_geoqtr$EM_COST/eda_em_geoqtr$EM_BCKLG
eda_em_ratio = eda_em_geoqtr %>% subset(select = c(yearqtr, BCKLGREV, BCKLGCOST))

eda_jn_geoqtr$BCKLGREV = eda_jn_geoqtr$JN_REV/eda_jn_geoqtr$JN_BCKLG
eda_jn_geoqtr$BCKLGCOST = eda_jn_geoqtr$JN_COST/eda_jn_geoqtr$JN_BCKLG
eda_jn_ratio = eda_jn_geoqtr %>% subset(select = c(yearqtr, BCKLGREV, BCKLGCOST))

eda_am_ratio %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Ratio") +
  ggtitle("AM Rev/Bcklg and Cost/Bcklg Ratio") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_ap_ratio %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Ratio") +
  ggtitle("AP Rev/Bcklg and Cost/Bcklg Ratio") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_em_ratio %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Ratio") +
  ggtitle("EM Rev/Bcklg and Cost/Bcklg Ratio") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

eda_jn_ratio %>%
  gather(key = "variable", value = "value", -yearqtr) %>%
  ggplot(aes(x = yearqtr, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_x_yearqtr(format = "%YQ%q") +
  scale_color_brewer(palette="Paired") +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "YearQtr", y = "Ratio") +
  ggtitle("JN Rev/Bcklg and Cost/Bcklg Ratio") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

```



```{r, echo=FALSE, include=FALSE, result='hide'}
am_geo = agr_geo_rev[,c(1,2,6:10)]
ap_geo = agr_geo_rev[,c(1,3,6:10)]
em_geo = agr_geo_rev[,c(1,4,6:10)]
jn_geo = agr_geo_rev[,c(1,5,6:10)]

am_geo_cost = agr_geo_cost[,c(1,2)]
ap_geo_cost = agr_geo_cost[,c(1,3)]
em_geo_cost = agr_geo_cost[,c(1,4)]
jn_geo_cost = agr_geo_cost[,c(1,5)]

# Rev & Cost Merge
am_ratio_geo = merge(am_geo, am_geo_cost, by = "Date", all.x = TRUE)
ap_ratio_geo = merge(ap_geo, ap_geo_cost, by = "Date", all.x = TRUE)
em_ratio_geo = merge(em_geo, em_geo_cost, by = "Date", all.x = TRUE)
jn_ratio_geo = merge(jn_geo, jn_geo_cost, by = "Date", all.x = TRUE)

# Data & Bcklg Merge
am_ratio_geo = merge(am_ratio_geo, raw_bcklg_update[,c(1,4)], by = "DateQtr", all.x = TRUE)
ap_ratio_geo = merge(ap_ratio_geo, raw_bcklg_update[,c(1,5)], by = "DateQtr", all.x = TRUE)
em_ratio_geo = merge(em_ratio_geo, raw_bcklg_update[,c(1,6)], by = "DateQtr", all.x = TRUE)
jn_ratio_geo = merge(jn_ratio_geo, raw_bcklg_update[,c(1,7)], by = "DateQtr", all.x = TRUE)

am_ratio_geo$RevRatio = am_ratio_geo$AM_REV/am_ratio_geo$AM_BCKLG
am_ratio_geo$CostRatio = am_ratio_geo$AM_COST/am_ratio_geo$AM_BCKLG
  
ap_ratio_geo$RevRatio = ap_ratio_geo$AP_REV/ap_ratio_geo$AP_BCKLG
ap_ratio_geo$CostRatio = ap_ratio_geo$AP_COST/ap_ratio_geo$AP_BCKLG 
  
em_ratio_geo$RevRatio = em_ratio_geo$EM_REV/em_ratio_geo$EM_BCKLG
em_ratio_geo$CostRatio = em_ratio_geo$EM_COST/em_ratio_geo$EM_BCKLG 
  
jn_ratio_geo$RevRatio = jn_ratio_geo$JN_REV/jn_ratio_geo$JN_BCKLG
jn_ratio_geo$CostRatio = jn_ratio_geo$JN_COST/jn_ratio_geo$JN_BCKLG 
  
```



```{r, fig.width=18, fig.height=8, warning=FALSE, echo=FALSE}
am_ratio_geo %>%
  subset(select = c(Date, RevRatio, CostRatio)) %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=36, limits = as.yearmon(c('Jan 2017', 'Sep 2020'))) +
  ylim(0.2, 0.5) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "Amount") +
  ggtitle("AM Rev/Bcklg and Cost/Bcklg Ratio (Monthly)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

ap_ratio_geo %>%
  subset(select = c(Date, RevRatio, CostRatio)) %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=36, limits = as.yearmon(c('Jan 2017', 'Sep 2020'))) +
  ylim(0.2, 0.5) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "Amount") +
  ggtitle("AP Rev/Bcklg and Cost/Bcklg Ratio (Monthly)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

em_ratio_geo %>%
  subset(select = c(Date, RevRatio, CostRatio)) %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=36, limits = as.yearmon(c('Jan 2017', 'Sep 2020'))) +
  ylim(0.2, 0.5) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "Amount") +
  ggtitle("EM Rev/Bcklg and Cost/Bcklg Ratio (Monthly)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))

jn_ratio_geo %>%
  subset(select = c(Date, RevRatio, CostRatio)) %>%
  gather(key = "variable", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value, group = variable, label = round(value, digits = 2))) +
  geom_line(size = 1.5, aes(color=variable)) +
  geom_point(size = 4.5, aes(color=variable)) +
  scale_color_brewer(palette="Paired") +
  scale_x_yearmon(format="%Y %m", n=36, limits = as.yearmon(c('Jan 2017', 'Sep 2020'))) +
  ylim(0.2, 0.5) +
  geom_text(size = 5, vjust = -1.2) +
  labs(x = "Date", y = "Amount") +
  ggtitle("JN Rev/Bcklg and Cost/Bcklg Ratio (Monthly)") +
  theme(plot.title = element_text(hjust = 0.5, size = 26), text = element_text(size=13), axis.text.x = element_text(angle = 40, hjust = 1), legend.title = element_text(color = "black", size = 20),   legend.text = element_text(color = "black", size = 15), axis.line = element_line(color = "black", size = 0.7))


```


```{r, echo=FALSE, include=FALSE, result='hide'}
# Rev Cost Setting
am_rev_setting = am_ratio_geo %>% subset(select = c(Date, RevRatio))
ap_rev_setting = ap_ratio_geo %>% subset(select = c(Date, RevRatio))
em_rev_setting = em_ratio_geo %>% subset(select = c(Date, RevRatio))
jn_rev_setting = jn_ratio_geo %>% subset(select = c(Date, RevRatio))

am_cost_setting = am_ratio_geo %>% subset(select = c(Date, CostRatio))
ap_cost_setting = ap_ratio_geo %>% subset(select = c(Date, CostRatio))
em_cost_setting = em_ratio_geo %>% subset(select = c(Date, CostRatio))
jn_cost_setting = jn_ratio_geo %>% subset(select = c(Date, CostRatio))

# TS Conversion
am_rev_setting = ts(am_rev_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ap_rev_setting = ts(ap_rev_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
em_rev_setting = ts(em_rev_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
jn_rev_setting = ts(jn_rev_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))

am_cost_setting = ts(am_cost_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
ap_cost_setting = ts(ap_cost_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
em_cost_setting = ts(em_cost_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))
jn_cost_setting = ts(jn_cost_setting[,-1], frequency = 12, start=c(2017,1), end= c(2020,9))

# BOXCOX
am_lambda = BoxCox.lambda(am_rev_setting)
ap_lambda = BoxCox.lambda(ap_rev_setting)
em_lambda = BoxCox.lambda(em_rev_setting)
jn_lambda = BoxCox.lambda(jn_rev_setting)

am_rev_boxcox = BoxCox(am_rev_setting, am_lambda)
ap_rev_boxcox = BoxCox(ap_rev_setting, ap_lambda)
em_rev_boxcox = BoxCox(em_rev_setting, em_lambda)
jn_rev_boxcox = BoxCox(jn_rev_setting, jn_lambda)

am_cost_boxcox = BoxCox(am_cost_setting, am_lambda)
ap_cost_boxcox = BoxCox(ap_cost_setting, ap_lambda)
em_cost_boxcox = BoxCox(em_cost_setting, em_lambda)
jn_cost_boxcox = BoxCox(jn_cost_setting, jn_lambda)


```



```{r, echo=FALSE, include=FALSE, result='hide'}
library(knitr)
library(forecastHybrid)

test_revtrain = window(am_rev_boxcox, frequency = 12, start=c(2018,1), end= c(2020,6))
test_costtrain = window(am_cost_boxcox, frequency = 12, start=c(2018,1), end= c(2020,6))
  
test_revtest = window(am_rev_boxcox, frequency = 12, start=c(2020,7), end= c(2020,9))
test_costtest = window(am_cost_boxcox, frequency = 12, start=c(2020,7), end= c(2020,9))

test_nnetar = nnetar(test_costtrain, xreg = test_revtrain)
test_nnetar_fcst = forecast(test_nnetar, xreg = test_revtest)
test_nnetar_accuracy = accuracy(InvBoxCox(test_costtest, am_lambda), InvBoxCox(test_nnetar_fcst$mean, am_lambda))

test_tslm = tslm(test_costtrain ~ test_revtrain + trend + season)
test_tslm_fcst = forecast(test_tslm, newdata = test_revtest)
test_tslm_accuracy = accuracy(InvBoxCox(test_costtest, am_lambda), InvBoxCox(test_tslm_fcst$mean, am_lambda))

test_am = rbind(test_nnetar_accuracy[1,1:5], test_tslm_accuracy[1,1:5])
rownames(test_am) = c("AM_NNETAR", "AM_TSLM")
test_am = as.data.frame.matrix(test_am)

test_am[order("MAPE"),,drop = FALSE]
test_am[order(test_am$MAPE),] %>% head(1)

test_hybrid = hybridModel(test_costtrain, weights="equal")
forecast(test_hybrid, h = 3)


```



```{r, echo=FALSE, include=FALSE, result='hide'}
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
  
  # MULTIVARIATE : NEURAL NETWORK
  am_nnetar = nnetar(am_cost_train, xreg = am_rev_train)
  ap_nnetar = nnetar(ap_cost_train, xreg = ap_rev_train)
  em_nnetar = nnetar(em_cost_train, xreg = em_rev_train)
  jn_nnetar = nnetar(jn_cost_train, xreg = jn_rev_train)

  am_nnetar_fcst = forecast(am_nnetar, xreg = am_rev_test)
  ap_nnetar_fcst = forecast(ap_nnetar, xreg = ap_rev_test)
  em_nnetar_fcst = forecast(em_nnetar, xreg = em_rev_test)
  jn_nnetar_fcst = forecast(jn_nnetar, xreg = jn_rev_test)
                          
  # UNIVARIATE : UNIVARIATE : ETS
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

  # UNIVARIATE : FORECAST HYBRID
  am_hybrid = hybridModel(am_cost_train, weights="equal")
  ap_hybrid = hybridModel(ap_cost_train, weights="equal")
  em_hybrid = hybridModel(em_cost_train, weights="equal")
  jn_hybrid = hybridModel(jn_cost_train, weights="equal")

  am_hybrid_fcst = forecast(am_hybrid, h = 3)
  ap_hybrid_fcst = forecast(ap_hybrid, h = 3)
  em_hybrid_fcst = forecast(em_hybrid, h = 3)
  jn_hybrid_fcst = forecast(jn_hybrid, h = 3)
  
  # UNIVARIATE : STACK 
  am_stack = (tslm_am_fcst$mean + am_sarimx_autofcst$mean + am_nnetar_fcst$mean)/3
  ap_stack = (tslm_ap_fcst$mean + ap_sarimx_autofcst$mean + ap_nnetar_fcst$mean)/3
  em_stack = (tslm_em_fcst$mean + em_sarimx_autofcst$mean + em_nnetar_fcst$mean)/3
  jn_stack = (tslm_jn_fcst$mean + jn_sarimx_autofcst$mean + jn_nnetar_fcst$mean)/3
  
  # ACCURACY
  ac_am_tslm = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(tslm_am_fcst$mean, am_lambda))
  ac_am_sarimax = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_sarimx_autofcst$mean, am_lambda))
  ac_am_nnetar = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_nnetar_fcst$mean, am_lambda)) 
  ac_am_ets = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(ets_am_fcst$mean, am_lambda))
  ac_am_arima = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_arima_fcst$mean, am_lambda))
  ac_am_mlp = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_mlp_fcst$mean, am_lambda))
  ac_am_hybrid = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_hybrid_fcst$mean, am_lambda))
  ac_am_stack = accuracy(InvBoxCox(am_cost_boxcox, am_lambda), InvBoxCox(am_stack, am_lambda))
  
  ac_am = rbind(ac_am_tslm[1,1:5], ac_am_sarimax[1,1:5], ac_am_nnetar[1,1:5], ac_am_ets[1,1:5], ac_am_arima[1,1:5], ac_am_mlp[1,1:5], ac_am_hybrid[1,1:5], ac_am_stack[1,1:5])
  rownames(ac_am) = c("AM_TSLM", "AM_SARIMAX", "AM_NNETAR", "AM_ETS", "AM_ARIMA", "AM_MLP", "AM_HYBRID", "AM_STACK")
  ac_am = as.data.frame.matrix(ac_am)
  ac_am_order = ac_am[order(ac_am$MAPE),] 
  
  ac_ap_tslm = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(tslm_ap_fcst$mean, ap_lambda))
  ac_ap_sarimax = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_sarimx_autofcst$mean, ap_lambda))  
  ac_ap_nnetar = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_nnetar_fcst$mean, ap_lambda)) 
  ac_ap_ets = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ets_ap_fcst$mean, ap_lambda))
  ac_ap_arima = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_arima_fcst$mean, ap_lambda)) 
  ac_ap_mlp = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_mlp_fcst$mean, ap_lambda))
  ac_ap_hybrid = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_hybrid_fcst$mean, ap_lambda))
  ac_ap_stack = accuracy(InvBoxCox(ap_cost_boxcox, ap_lambda), InvBoxCox(ap_stack, ap_lambda))
  
  ac_ap = rbind(ac_ap_tslm[1,1:5], ac_ap_sarimax[1,1:5], ac_ap_nnetar[1,1:5], ac_ap_ets[1,1:5], ac_ap_arima[1,1:5], ac_ap_mlp[1,1:5], ac_ap_hybrid[1,1:5], ac_ap_stack[1,1:5])
  rownames(ac_ap) = c("AP_TSLM", "AP_SARIMAX", "AP_NNETAR","AP_ETS", "AP_ARIMA", "AP_MLP", "AP_HYBRID", "AP_STACK")
  ac_ap = as.data.frame.matrix(ac_ap)
  ac_ap_order = ac_ap[order(ac_ap$MAPE),]
  
  ac_em_tslm = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(tslm_em_fcst$mean, em_lambda))
  ac_em_sarimax = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_sarimx_autofcst$mean, em_lambda))
  ac_em_nnetar = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_nnetar_fcst$mean, em_lambda)) 
  ac_em_ets = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(ets_em_fcst$mean, em_lambda))    
  ac_em_arima = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_arima_fcst$mean, em_lambda))
  ac_em_mlp = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_mlp_fcst$mean, em_lambda))
  ac_em_hybrid = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_hybrid_fcst$mean, em_lambda))
  ac_em_stack = accuracy(InvBoxCox(em_cost_boxcox, em_lambda), InvBoxCox(em_stack, em_lambda))
  
  ac_em = rbind(ac_em_tslm[1,1:5], ac_em_sarimax[1,1:5], ac_em_nnetar[1,1:5], ac_em_ets[1,1:5], ac_em_arima[1,1:5], ac_em_mlp[1,1:5], ac_em_hybrid[1,1:5], ac_em_stack[1,1:5])
  rownames(ac_em) = c("EM_TSLM", "EM_SARIMAX", "EM_NNETAR","EM_ETS", "EM_ARIMA", "EM_MLP", "EM_HYBRID", "EM_STACK")
  ac_em = as.data.frame.matrix(ac_em)
  ac_em_order = ac_em[order(ac_em$MAPE),] 
  
  ac_jn_tslm = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(tslm_jn_fcst$mean, jn_lambda))
  ac_jn_sarimax = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_sarimx_autofcst$mean, jn_lambda))
  ac_jn_nnetar = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_nnetar_fcst$mean, jn_lambda)) 
  ac_jn_ets = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(ets_jn_fcst$mean, jn_lambda))    
  ac_jn_arima = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_arima_fcst$mean, jn_lambda)) 
  ac_jn_mlp = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_mlp_fcst$mean, jn_lambda))
  ac_jn_hybrid = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_hybrid_fcst$mean, jn_lambda))
  ac_jn_stack = accuracy(InvBoxCox(jn_cost_boxcox, jn_lambda), InvBoxCox(jn_stack, jn_lambda))
  
  ac_jn = rbind(ac_jn_tslm[1,1:5], ac_jn_sarimax[1,1:5], ac_jn_nnetar[1,1:5], ac_jn_ets[1,1:5], ac_jn_arima[1,1:5], ac_jn_mlp[1,1:5], ac_jn_hybrid[1,1:5], ac_jn_stack[1,1:5])
  rownames(ac_jn) = c("JN_TSLM", "JN_SARIMAX", "JN_NNETAR","JN_ETS", "JN_ARIMA", "JN_MLP", "JN_HYBRID", "JN_STACK")
  ac_jn = as.data.frame.matrix(ac_jn)
  ac_jn_order = ac_jn[order(ac_jn$MAPE),] 

  # PRINT
  print(ac_am_order)
  print(ac_ap_order)
  print(ac_em_order)
  print(ac_jn_order)

}
```

AM - STACK & NNETAR
AP - STACK & TSLM 
EM - STACK & SARIMAX
JN - STACK & SARIMAX

```{r, echo=FALSE, include=FALSE, result='hide'}
manual_function(2018, 1, 2020, 7)
```

```{r, echo=FALSE, include=FALSE, result='hide'}
manual_function(2018, 1, 2020, 4)
```

```{r, echo=FALSE, include=FALSE, result='hide'}
manual_function(2018, 1, 2020, 1)
```


```{r, echo=FALSE, include=FALSE, result='hide'}
manual_graph_function = function(starty, startm, testy, testm){
  
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
  
  # MULTIVARIATE : NEURAL NETWORK
  am_nnetar = nnetar(am_cost_train, xreg = am_rev_train)
  ap_nnetar = nnetar(ap_cost_train, xreg = ap_rev_train)
  em_nnetar = nnetar(em_cost_train, xreg = em_rev_train)
  jn_nnetar = nnetar(jn_cost_train, xreg = jn_rev_train)

  am_nnetar_fcst = forecast(am_nnetar, xreg = am_rev_test)
  ap_nnetar_fcst = forecast(ap_nnetar, xreg = ap_rev_test)
  em_nnetar_fcst = forecast(em_nnetar, xreg = em_rev_test)
  jn_nnetar_fcst = forecast(jn_nnetar, xreg = jn_rev_test)
                          
  # UNIVARIATE : UNIVARIATE : ETS
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

  # UNIVARIATE : FORECAST HYBRID
  am_hybrid = hybridModel(am_cost_train, weights="equal")
  ap_hybrid = hybridModel(ap_cost_train, weights="equal")
  em_hybrid = hybridModel(em_cost_train, weights="equal")
  jn_hybrid = hybridModel(jn_cost_train, weights="equal")

  am_hybrid_fcst = forecast(am_hybrid, h = 3)
  ap_hybrid_fcst = forecast(ap_hybrid, h = 3)
  em_hybrid_fcst = forecast(em_hybrid, h = 3)
  jn_hybrid_fcst = forecast(jn_hybrid, h = 3)
  
  # UNIVARIATE : STACK 
  am_stack = (tslm_am_fcst$mean + am_sarimx_autofcst$mean + am_nnetar_fcst$mean)/3
  ap_stack = (tslm_ap_fcst$mean + ap_sarimx_autofcst$mean + ap_nnetar_fcst$mean)/3
  em_stack = (tslm_em_fcst$mean + em_sarimx_autofcst$mean + em_nnetar_fcst$mean)/3
  jn_stack = (tslm_jn_fcst$mean + jn_sarimx_autofcst$mean + jn_nnetar_fcst$mean)/3
  
  # SUMMARY PRINT
  select = rbind(am_stack, am_nnetar_fcst$mean, ap_stack, tslm_ap_fcst$mean, em_stack, em_sarimx_autofcst$mean, jn_stack, jn_sarimx_autofcst$mean)
  rownames(select) = c("am_stack", "am_nnetar", "ap_stack", "ap_tslm", "em_stack", "em_sarimax", "jn_stack", "jn_sarimax")
  print(select)
}
```

```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
manual_graph_function(2018, 1, 2020, 1)
manual_graph_function(2018, 1, 2020, 4)
manual_graph_function(2018, 1, 2020, 7)
```


```{r, echo=FALSE, include=FALSE, result='hide'}
a = manual_graph_function(2018, 1, 2020, 1)

```

```{r, echo=FALSE, include=FALSE, result='hide'}
b = manual_graph_function(2018, 1, 2020, 4)
```


```{r, echo=FALSE, include=FALSE, result='hide'}
c = manual_graph_function(2018, 1, 2020, 7)
```

```{r, echo=FALSE, include=FALSE, result='hide'}
layer_am_1 = ts(c(a[1,1:3], b[1,1:3], c[1,1:3]),  frequency = 12, start=c(2020,1))
layer_am_2 = ts(c(a[2,1:3], b[2,1:3], c[2,1:3]),  frequency = 12, start=c(2020,1))

layer_ap_1 = ts(c(a[3,1:3], b[3,1:3], c[3,1:3]),  frequency = 12, start=c(2020,1))
layer_ap_2 = ts(c(a[4,1:3], b[4,1:3], c[4,1:3]),  frequency = 12, start=c(2020,1))

layer_em_1 = ts(c(a[5,1:3], b[5,1:3], c[5,1:3]),  frequency = 12, start=c(2020,1))
layer_em_2 = ts(c(a[6,1:3], b[6,1:3], c[6,1:3]),  frequency = 12, start=c(2020,1))

layer_jn_1 = ts(c(a[7,1:3], b[7,1:3], c[7,1:3]),  frequency = 12, start=c(2020,1))
layer_jn_2 = ts(c(a[8,1:3], b[8,1:3], c[8,1:3]),  frequency = 12, start=c(2020,1))
```


```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
autoplot(am_cost_setting, main = "AM FORECAST") +
  autolayer(InvBoxCox(layer_am_1, am_lambda), series = "am_stack") + 
  autolayer(InvBoxCox(layer_am_2, am_lambda), series = "am_nnetar") +
  scale_x_yearmon(limits = as.yearmon(c('Jan 2018', 'Sep 2020')))
  
```


```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
autoplot(ap_cost_setting, main = "AP FORECAST") +
  autolayer(InvBoxCox(layer_ap_1, ap_lambda), series = "ap_stack") + 
  autolayer(InvBoxCox(layer_ap_2, ap_lambda), series = "ap_tslm") +
  scale_x_yearmon(limits = as.yearmon(c('Jan 2018', 'Sep 2020')))
```


```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
autoplot(em_cost_setting, main = "EM FORECAST") +
  autolayer(InvBoxCox(layer_em_1, em_lambda), series = "em_stack") + 
  autolayer(InvBoxCox(layer_em_2, em_lambda), series = "em_sarimax") +
  scale_x_yearmon(limits = as.yearmon(c('Jan 2018', 'Sep 2020')))
```


```{r, fig.width=18, fig.height=10, warning=FALSE, echo=FALSE}
autoplot(jn_cost_setting, main = "EM FORECAST") +
  autolayer(InvBoxCox(layer_jn_1, jn_lambda), series = "jn_stack") + 
  autolayer(InvBoxCox(layer_jn_2, jn_lambda), series = "jn_sarimax") +
  scale_x_yearmon(limits = as.yearmon(c('Jan 2018', 'Sep 2020')))
```
