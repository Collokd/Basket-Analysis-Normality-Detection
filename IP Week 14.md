---
title: "Unsupervised Learning"
author : "Collins Kemboi"

#WEEK 14 IP

##Defining the Question


###a). Specifying the question

As Data analyst at Carrefour Kenya and I have been tasked to inform the marketing department on the most relevant marketing strategies that will result in the highest no. of sales (total price including tax). I will therefore use unsupervised techniques to unearth key insights the will result to data driven recommendations for the company.


###b). Defining the metrics of success

Uncover insights from the data provided to enable the marketing department make data driven decisions.


###c) Understanding the Context

You are a Data analyst at Carrefour Kenya and are currently undertaking a project that will inform the marketing department on the most relevant marketing strategies that will result in the highest no. of sales (total price including tax). Your project has been divided into four parts where you'll explore a recent marketing dataset by performing various unsupervised learning techniques and later providing recommendations based on your insights.



###d). Recording the Experimental Design


1.   Data Cleaning and Preparation

2.   Dimensionality Reduction

3.   Feature Selection

4.   Association Rules

5.   Anomaly Detection

6.   Make conclusions & Challenge the solution


###e) Data relevance/ Appropriateness of Data

Link to the dataset:http://bit.ly/CarreFourDataset

Link to the dataset:http://bit.ly/SupermarketDatasetII

Link to the dataset:http://bit.ly/CarreFourSalesDataset

# Loading libraries
```{r}
install.packages("tidyverse")

library(tidyverse)

install.packages("magrittr")

library(magrittr)

install.packages("corrplot")

library(corrplot)

install.packages("caret")

library(caret)

options(warn = -1)

install.packages("arules")

library(arules)

library(arulesViz)

library(readr)
```
# Implementation of the solution; 

## 1. Dimensionality Reduction using Principal Component Analysis
```{r}
# Loading dataset

Sales_Data <- read_csv("C:/Users/color/Downloads/Supermarket_Dataset_1 - Sales Data.csv")

# Data cleaning

# Tidying the Dataset

Sales_Data

head(Sales_Data)

# Data ttpe

sapply(Sales_Data, class)

dim(Sales_Data)

colSums(is.na(Sales_Data))

# Duplicate Identification

duplicates = Sales_Data[duplicated(Sales_Data),]

duplicates

Sales_Data = unique(Sales_Data)

nrow(Sales_Data)
```

```{r}
# Convert categorical data into numerical
Sales_Data$Branch_Num<-as.integer(as.factor(Sales_Data$Branch))

Sales_Data$Gender_Numc<-as.integer(as.factor(Sales_Data$Gender))

Sales_Data$Payment_Num<-as.integer(as.factor(Sales_Data$Payment))

Sales_Data$Customer_Type_Num<-as.integer(as.factor(Sales_Data$"Customer type"))

Sales_Data$Product_Line_Num<-as.integer(as.factor(Sales_Data$"Product line"))

names(Sales_Data)

#new data types

sapply(Sales_Data, class)

#Sub-setting numerical columns

install.packages("dplyr")

library("dplyr") 

data_num <- select_if(Sales_Data,is.numeric)

data_num

str(data_num)

names(data_num[, sapply(data_num, function(v) var(v, na.rm=TRUE)==0)])

# Rename column 
names(Sales_Data)[names(Sales_Data) == "gross margin percentage"] <- "gross_margin_percentage"

names(data_num)[names(data_num) == "gross margin percentage"] <- "gross_margin_percentage"

names

data_num <- subset(data_num, select = -c(gross_margin_percentage))

# current shape

dim(data_num)

pca <- prcomp(data_num, center = TRUE,scale. = TRUE)

summary(pca)

# Visualization

plot(pca, type="l")
```
### Conclusion

From our PCA analysis we noted our pca1 contributed highly to the variance of the data at 41.1%.


# 2. Feature Selection in Unsupervised Learning
```{r}
#Checking correlation
Sales_corr <- cor(data_num)

#sub-setting highly correlated features
highcorr<- findCorrelation(Sales_corr, cutoff=0.70)

highcorr

names(data_num[,highcorr])

new_df = data_num[-highcorr]

names(new_df)
```

### Conclusion
From our analysis the features that are of important and can be used for further analysis are;


# 3. Association Analysis
```{r}
Sales_Data2 <- read_csv("C:/Users/color/Downloads/Supermarket_Sales_Dataset II.csv")

head(Sales_Data2)

# Data Cleaning

## Tidying Up the Dataset

# Shape

dim(Sales_Data2)

# Data types

sapply(Sales_Data2, class)

# checking for null values

colSums(is.na(Sales_Data2))

# checking if duplicates present & removing

duplicated(Sales_Data2)

names(Sales_Data2)

# Sales_Data2 <- !duplicated(Sales_Data2)

Sales_Data2 <- distinct(Sales_Data2)

dim(Sales_Data2)

# Overview Statistics

summary(Sales_Data2)


# Def

library(arules)

rules <- apriori(Sales_Data2, parameter = list(supp = 0.5, conf = 0.8,target = "rules",minlen=2))

#rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.5)) # Min Support as 0.001, confidence as 0.8.
```

### Conclusion

The chances of a customer purchasing 
# 4. Anomaly Detection
```{r}
# loading data

library(readr)

Sales_Data3 <- read_csv("C:/Users/color/Downloads/Supermarket_Sales_Forecasting - Sales.csv")

View(Sales_Data3)

head(Sales_Data3)

dim(Sales_Data3)

# Package and Library Installation

install.packages("anomalize")

library(anomalize) #tidy anomaly detection

library(tidyverse) #tidyverse packages like dplyr, ggplot, tidyr

# library(coindeskr) #bitcoin price extraction from coindesk

tidyverse_cran_downloads %>%
  time_decompose(count) %>%
  anomalize(remainder) %>%
  time_recompose() %>%
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)

names(Sales_Data3)
Sales_Data3 %>% 
  time_decompose(Sales_Data3, method = "stl", frequency = "auto", trend = "auto") %>%
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.2) %>%
  plot_anomaly_decomposition(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)
```

# Follow up questions


a). Did we have the right data?

Yes
 
b). Do we need other data to answer our question?

no.

c). Did we have the right question?

yes