rm(mtcars)
rm(survey)
install.packages("corrplot")
install.packages("odbc")
install.packages("DBI")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tibbletime")
install.packages("factoextra")
install.packages("forecast")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("scales")
install.packages("magrittr")

options(scipen = 50)

library(corrplot)
library(odbc)
library(DBI)
library(dplyr)
library(ggplot2)
library(tibbletime)
library(factoextra)
library(forecast)
library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)

AllData_Joined <- NYC_TRANSACTION_DATA %>% 
  left_join(BUILDING_CLASS, by = c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>% 
  left_join(NEIGHBORHOOD, by = c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID"))

TSData <- filter(AllData_Joined) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  mutate(SALE_QUARTER = quarter(SALE_DATE)) %>%
  group_by(SALE_YEAR) %>%
  group_by(SALE_QUARTER) %>%
  filter(SALE_PRICE > 75000, GROSS_SQUARE_FEET > 350, SALE_YEAR >= 2009, TYPE =="RESIDENTIAL", NEIGHBORHOOD_NAME =="BAYSIDE") %>%
  select(NEIGHBORHOOD_NAME,RESIDENTIAL_UNITS,GROSS_SQUARE_FEET,SALE_PRICE,TYPE,SALE_YEAR,SALE_QUARTER)

#TASK 1

HistoricalTimeSeries <- mutate(TSData, Time=(SALE_YEAR-2009)*4+SALE_QUARTER) %>%
  group_by(Time) %>%
  summarise(TotalSales=sum(SALE_PRICE))

ggplot()+geom_line(data=HistoricalTimeSeries, size=1, aes(x=Time, y=TotalSales))

mutate(TSData, Time=(SALE_YEAR-2009)*4+SALE_QUARTER)

TimeSeriesSetup<- ts(HistoricalTimeSeries$TotalSales, start = c(2009,1), frequency=4)  
View(TimeSeriesSetup)

TimeSeriesSetup

?ets
?ts

#Testing all models to check

SmoothingModelMAA <- ets(y=TimeSeriesSetup, model = "MAA")
summary(SmoothingModelMAA)

SmoothingModelANN <- ets(y=TimeSeriesSetup, model = "ANN")
summary(SmoothingModelANN)

SmoothingModelMAN <- ets(y=TimeSeriesSetup, model = "MAN")
summary(SmoothingModelMAN)

SmoothingModelZZZ <- ets(y=TimeSeriesSetup, model = "ZZZ")
summary(SmoothingModelZZZ)

SmoothingModelMAM <- ets(y=TimeSeriesSetup, model = "MAM")
summary(SmoothingModelMAM)

ForecastSales <- forecast(SmoothingModelMAM, 8)
Task1Summary <- summary(ForecastSales)
View(Task1Summary)

ForecastSales
plot(ForecastSales)


## TASK 2

Filter4Reg<- filter(AllData_Joined) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  mutate(SALE_QUARTER = quarter(SALE_DATE)) %>%
  group_by(SALE_YEAR) %>%
  group_by(SALE_QUARTER) %>%
  filter(SALE_PRICE > 75000, GROSS_SQUARE_FEET > 350, SALE_YEAR >= 2009, TYPE =="RESIDENTIAL", NEIGHBORHOOD_NAME =="BAYSIDE")

HistoricalRegression <- mutate(Filter4Reg, TIME=(SALE_YEAR-2009)*4+SALE_QUARTER) %>%
  group_by(TIME) %>%
  summarise(TOTALSALES = sum(SALE_PRICE))

Reg1 <- cbind(HistoricalRegression, c(rep(c("Q1", "Q2", "Q3", "Q4"),length.out = 52, each = 1)))
names(Reg1)[3] <- "QUARTER"

Regression <- lm(data = Reg1, formula = TOTALSALES~TIME+QUARTER)
summary(Regression)
## Here we made sure that the P value is less than 0.05
## Adjusted R Square here is 59% which tells that 59% of sales value is affected by the time and which quarter sales takes place
## Q3 and Q4 has more higher price sales

SimpleRegression<- lm(data = Reg1, formula = TOTALSALES~TIME)
summary(SimpleRegression)

Reg.Forecast <- data.frame(TIME=c(53,54,55,56,57,58,59,60), TOTALSALES = c(0,0,0,0,0,0,0,0), QUARTER = c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4"))

Reg1.Prediction <- predict.lm(Regression,Reg.Forecast, interval="confidence")
View(Reg1.Prediction)

Reg1.Prediction <- predict.lm(SimpleRegression,Reg.Forecast, interval="confidence")
View(Reg1.Prediction)



## TASK 3

MultiReg<- filter(AllData_Joined) %>%
  mutate(SALE_YEAR = year(SALE_DATE)) %>%
  mutate(SALE_QUARTER = quarter(SALE_DATE)) %>%
  group_by(SALE_YEAR) %>%
  group_by(SALE_QUARTER) %>%
  filter(SALE_PRICE > 75000, GROSS_SQUARE_FEET > 350, SALE_YEAR >= 2009, TYPE =="RESIDENTIAL", NEIGHBORHOOD_NAME =="BAYSIDE", YEAR_BUILT > 0, YEAR_BUILT!="NA")


#Preparing Data
MultiRegPrep <- MultiReg %>%
  filter(SALE_YEAR>= "2009") %>%
  ungroup() %>%
  select(BUILDING_CLASS_FINAL_ROLL, SALE_DATE, YEAR_BUILT, GROSS_SQUARE_FEET, RESIDENTIAL_UNITS, SALE_PRICE)

#Model Implementation

MultiRegModel <- lm(formula = SALE_PRICE~. , data = MultiRegPrep)
summary(MultiRegModel)

#Residual Value

MultiRegPrep["RESIDUAL"] <- MultiRegModel$residuals

Address <- filter(MultiReg) %>%
  ungroup() %>%
  filter(SALE_YEAR>="2009") %>%
  select(ADDRESS)

# Connecting Residuals to Address

MultiRegPrep["ADDRESS"] <- Address 

#######################

MultiRegPrepPOSITIVE <- data.frame(SALE_DATE = as.Date(c("2013-11-22", "2015-07-02", "2018-04-30")),SALE_PRICE = c(6000000, 4030000, 4800000),RESIDUAL = c(3413457.3, 3346688.2, 2896967.6),ADDRESS = c("36-21 213TH STREET", "42-39 208TH ST", "39-30 214TH PLACE"))

MultiRegPrepPOSITIVE1 <- MultiRegPrepPOSITIVE %>%
  mutate(PERCENTAGE = RESIDUAL / sum(RESIDUAL) * 100)


ggplot(MultiRegPrepPOSITIVE1, aes(x = "", y = RESIDUAL, fill = ADDRESS)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(PERCENTAGE, 1), "%")), 
            position = position_stack(vjust = 0.5)) +
  labs(title = "TOP POSITIVE RESIDUAL VALUES") +
  theme_void() +
  theme(legend.title = element_blank())


######################
#Plotting Pie Chart for Displaying Top Negative Residual Values

MultiRegPrepTABLE <- data.frame(SALE_DATE = as.Date(c("2010-10-07", "2017-09-27", "2015-03-19")),SALE_PRICE = c(21500, 262667, 199000),RESIDUAL = c(-380809.1, -2788921.8, -261987.2),ADDRESS = c("202-20 18TH AVE", "38-12 213TH STREET", "18-75 CORPORAL KENNEDY STR"))


MultiRegPrepTABLE1 <- MultiRegPrepTABLE %>%
  mutate(PERCENTAGE = RESIDUAL / sum(RESIDUAL) * 100)


ggplot(MultiRegPrepTABLE1, aes(x = "", y = RESIDUAL, fill = ADDRESS)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(PERCENTAGE, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "TOP NEGATIVE RESIDUAL VALUES") +
  theme_void() +
  theme(legend.title = element_blank())
