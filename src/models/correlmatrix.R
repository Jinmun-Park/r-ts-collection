---
title: "correlmatrix"
author: "Jinmun Park"
date: "October 8, 2019"
output: rmarkdown::github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(ggplot2)
library(httr)
library(dplyr)
library(tidyverse)
library(randomForest)
library(xgboost)
library(data.table)
library(corrplot)
library(Hmisc)
library(fastDummies)
```


Combine

```{r}
combine = read.csv("C:/Users/JinmunPark/***.csv", header = TRUE, sep = ",", stringsAsFactors = F, dec = ",")

# DATE
combine$Week = as.Date(combine$Week, "%m/%d/%Y")
combine$Yr=year(combine$Week)
combine$Qtr=quarter(combine$Week)
combine$Mth=month(combine$Week)
combine=subset(combine, select = -c(Year, Quarter, Month))

# Convert Figures
combine$Quote.BMC.GP = as.numeric(gsub(",","", combine$Quote.BMC.GP))
combine$Quote.TMC.GP = as.numeric(gsub(",","", combine$Quote.TMC.GP))
combine$Quote.TMC.GP. = as.numeric(gsub(",","", combine$Quote.TMC.GP.))
combine$Quote.BMC.GP. = as.numeric(gsub(",","", combine$Quote.BMC.GP.))

# Create new variable
combine$BTMC = combine$Quote.BMC.GP - combine$Quote.TMC.GP

# Change the name of variable
names(combine)[names(combine) == "Quote.TMC.GP."] = "GP Per"
names(combine)[names(combine) == "Week"] = "Date"
names(combine)[names(combine) == "Quote.Discount.."] = "Quote.Discount.Per"

df = subset(combine, select = -c(Geo, Region, List.Price.Known, Source, Machine.Model, Mktg.Category, Part.SEO.Number, Approval.code, Transaction.Status, Delegation.Approval, Fulfillment.Route, Channel, Coverage.Type, Coverage.Name, Specific.Sector, Account.Type, Level.of.Fulfilment, Quote.BMC.GP., Quote.BMC.GP, Quote.TMC.GP, Win.Coverage.Bid.Count, Win.Coverage.Bid.Revenue, X..of.Won.Bids, Won.Bid.List, Won.Bid.Revenue, Est..Won.Bid.TMC.GP, Weighted.Won.Bid.Discount., Est..Won.Bid.GP., Win.Rate.by.Volume, Won.Bid.Rev.Yield, Weighted.Quote.Discount.., Ship.Quantity, X..of.Bids, Sector.Group))

df.corr = df %>%
  select(Country, Won.Not.Won, Opportunity.ID, Date, Customer, Product.Family, Machine.Type, Sector) %>%
  filter(Won.Not.Won == "Y") 

```


Sector
```{r}
unique(df.corr$Sector)
```

Country
```{r}
unique(df.corr$Country)
```


Sector - Product Family
```{r}

correlation.plot = function(sector.name, ctry.name){

  # A. Selection

  df.corr.select = df.corr %>%
  select(Won.Not.Won, Opportunity.ID, Sector, Product.Family, Country) %>%
  filter(Sector == sector.name) %>%
  filter(Country == ctry.name)

  # B.1. Create Dummy and drops the rest variables

  df.corr.select.dummy = dummy_cols(df.corr.select, select_columns = c("Product.Family"))
  df.corr.select.dummy = subset(df.corr.select.dummy, select = -c(Won.Not.Won, Sector, Product.Family, Country))

  # B.2. Count number of columns
  ncol = ncol(df.corr.select.dummy)

  # B.3. Aggregate Dummies
  df.corr.select.dummy = aggregate(df.corr.select.dummy[,2:ncol], by = list(df.corr.select.dummy[,1]), FUN = sum)

  # C. Create Matrix
  df.corr.matrix = as.matrix(df.corr.select.dummy)
  df.corr.matrix = df.corr.matrix[,-1]
  df.corr.matrix = rcorr(df.corr.matrix)

  return(df.corr.matrix)

}

# D. PLOT
df.corr.matrix = correlation.plot("Computer Services All In Sector", "Indonesia")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Indonesia.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#

df.corr.matrix = correlation.plot("Computer Services All In Sector", "Malaysia")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Malaysia.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#

df.corr.matrix = correlation.plot("Computer Services All In Sector", "Singapore")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Singapore.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#

df.corr.matrix = correlation.plot("Computer Services All In Sector", "Philippines")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Philippines.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#

df.corr.matrix = correlation.plot("Computer Services All In Sector", "Australia")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Australia.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#
df.corr.matrix = correlation.plot("Computer Services All In Sector", "India")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-India.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#
df.corr.matrix = correlation.plot("Computer Services All In Sector", "Thailand")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-Thailand.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#
df.corr.matrix = correlation.plot("Computer Services All In Sector", "New Zealand")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-New Zealand.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")

#
df.corr.matrix = correlation.plot("Computer Services All In Sector", "South Korea")

png(height = 1500, width = 2500, pointsize = 12, file = "Computer Services All In Sector-South Korea.png")
corrplot(df.corr.matrix$r, type="upper", order="hclust", p.mat = df.corr.matrix$P, sig.level = 0.05, insig = "blank")
```

# Computer Services All In Sector - Malaysia 
# Computer Services All In Sector - Philippines 


Check - Computer Services All In Sector - Malaysia 
```{r}

```



```{r}
# B. Count Frequency of Product Family 
df.corr.select.count = df.corr.select %>%
  group_by(Product.Family) %>%
  summarise(Freq = n())

# C. Drops the Frequency less than 10. (10 is my random number)
df.corr.select.frequency = df.corr.select.count %>% 
  mutate(Check = Freq > 10) %>% 
  filter(Check == "TRUE") %>% 
  select(Product.Family)

# D. Merge 
df.corr.select = merge(df.corr.select, df.corr.select.frequency, by = "Product.Family")

```
