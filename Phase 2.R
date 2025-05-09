##task 1

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

rm(mtcars)
rm(survey)

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

AllData_Joined <- NYC_TRANSACTION_DATA %>% 
  left_join(BUILDING_CLASS, by = c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>% 
  left_join(NEIGHBORHOOD, by = c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID"))

##Task 2

L1 <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", TYPE=="RESIDENTIAL") %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)

## Task 3 

CLEAN_L1 <- AllData_Joined %>% 
  mutate(SaleYear=year(SALE_DATE)) %>% 
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", TYPE=="RESIDENTIAL", SALE_PRICE>100000, GROSS_SQUARE_FEET>350) %>% 
  group_by(SaleYear) %>% 
  summarise(TotalSales=sum(SALE_PRICE), TotalSqft=sum(GROSS_SQUARE_FEET), Avg=TotalSales/TotalSqft)

summary(CLEAN_L1)
