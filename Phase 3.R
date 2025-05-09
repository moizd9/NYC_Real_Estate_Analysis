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

AllData_Joined <- NYC_TRANSACTION_DATA %>% 
  left_join(BUILDING_CLASS, by = c("BUILDING_CLASS_FINAL_ROLL"="X.BUILDING_CODE_ID")) %>% 
  left_join(NEIGHBORHOOD, by = c("NEIGHBORHOOD_ID" = "NEIGHBORHOOD_ID")) %>%
  filter(SALE_PRICE > 75000, GROSS_SQUARE_FEET>500)

summary(AllData_Joined)

#Ungrouped Data Frame

OriginalNoGroup <- AllData_Joined %>%
  select(NEIGHBORHOOD_NAME, TYPE, SALE_DATE, SALE_PRICE, GROSS_SQUARE_FEET, BUILDING_CLASS_FINAL_ROLL, RESIDENTIAL_UNITS) %>%
  mutate(SALE_YEAR=year(SALE_DATE)) %>%
  filter(NEIGHBORHOOD_NAME=="BAYSIDE")

#Grouped

OriginalGrouped <- AllData_Joined %>%
  select(NEIGHBORHOOD_NAME, TYPE, SALE_DATE, SALE_PRICE, GROSS_SQUARE_FEET, BUILDING_CLASS_FINAL_ROLL, RESIDENTIAL_UNITS) %>%
  mutate(SALE_YEAR=year(SALE_DATE)) %>%
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", SALE_PRICE>75000, GROSS_SQUARE_FEET>300)%>%
  group_by(SALE_YEAR)

#Task 1

BAYSIDE_RES <- OriginalGrouped %>%
  select(NEIGHBORHOOD_NAME, TYPE, SALE_YEAR, SALE_PRICE ,GROSS_SQUARE_FEET) %>%
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", SALE_PRICE>75000, GROSS_SQUARE_FEET>300, SALE_YEAR>=2009, TYPE=="RESIDENTIAL")%>%
  group_by(SALE_YEAR)

BAYSIDE_RESnoGROUP <- OriginalNoGroup %>%
  select(NEIGHBORHOOD_NAME, TYPE, SALE_YEAR, SALE_DATE, SALE_YEAR, SALE_PRICE ,GROSS_SQUARE_FEET) %>%
  mutate(SALE_YEAR=year(SALE_DATE)) %>%
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", SALE_PRICE>75000, GROSS_SQUARE_FEET>300, SALE_YEAR>=2009, TYPE=="RESIDENTIAL")

#Task 1a

summarise(BAYSIDE_RES, TOTAL_SALES=sum(SALE_PRICE))
summarise(BAYSIDE_RESnoGROUP, totalsales=sum(SALE_PRICE))

#Task 1b

summarise(BAYSIDE_RES, MEAN_SALES=mean(SALE_PRICE))
summarise(BAYSIDE_RESnoGROUP, MeanSales=mean(SALE_PRICE))

summarise(BAYSIDE_RES, MEAN_SQ_FEET=mean(GROSS_SQUARE_FEET))
summarise(BAYSIDE_RESnoGROUP, MeanSquareFeet=mean(GROSS_SQUARE_FEET))

#Task 1C

fivenum(BAYSIDE_RES$SALE_PRICE)
fivenum(BAYSIDE_RES$GROSS_SQUARE_FEET)


#Total Sales by STatus

PROP <- AllData_Joined %>%
  select(NEIGHBORHOOD_NAME, TYPE, SALE_DATE, SALE_PRICE ,GROSS_SQUARE_FEET) %>%
  mutate(SALE_YEAR=year(SALE_DATE)) %>%
  group_by(TYPE) %>%
  filter(NEIGHBORHOOD_NAME=="BAYSIDE", SALE_YEAR>=2009)

summarise(PROP, totalSales=sum(SALE_PRICE))  

#Getting their Ratios

PROPORTION <- prop.table(tapply(PROP$SALE_PRICE, list(PROP$TYPE), sum))
View(PROPORTION)

#Task 1d
PROPORTION*100

#Task 1e and 1f

BIND_BAYSIDE <- cbind(mean(BAYSIDE_RES$SALE_PRICE), mean(BAYSIDE_RES$GROSS_SQUARE_FEET), median(BAYSIDE_RES$SALE_PRICE), median(BAYSIDE_RES$GROSS_SQUARE_FEET), sd(BAYSIDE_RES$SALE_PRICE), sd(BAYSIDE_RES$GROSS_SQUARE_FEET), cor(BAYSIDE_RES$SALE_PRICE, BAYSIDE_RES$GROSS_SQUARE_FEET))

MATRIX <- matrix(BIND_BAYSIDE, ncol=8, byrow = T)
colnames(MATRIX)<- c("MEAN SALE PRICE", "MEAN GROSS SQ FEET", "MEDIA SALE PRICE", "MEDIAN GROSS SQ FEET", "SD SALE PRICE", "SD GROSS SQ FEET", "Cor Sale Price", "Cor Gross Sq Feet")
rownames(MATRIX)<- c("BAYSIDE")
View(MATRIX)




## TASK 2

CleanHistorical <- AllData_Joined %>%
  mutate(SALE_YEAR=year(SALE_DATE)) %>%
  filter(SALE_PRICE>75000, GROSS_SQUARE_FEET>300)

DataAfter2009 <- filter(.data = CleanHistorical, SALE_YEAR>=2009, TYPE=="RESIDENTIAL")%>%
  as.data.frame()


#Creating new data frame for creating groups

GroupedData <- group_by(DataAfter2009, NEIGHBORHOOD_ID)%>%
  filter(NEIGHBORHOOD_ID!="N/A")%>%
  summarise(MedianSalePrice=(median(SALE_PRICE)), SDSalePrice=(sd(SALE_PRICE)), UnitsSold=(sum(RESIDENTIAL_UNITS)), Sales = n())%>%
  as.data.frame()


zscores<- scale(GroupedData[c(-1)]) %>%
  as.data.frame()

GroupedData <- group_by(DataAfter2009, NEIGHBORHOOD_ID)%>%
  filter(NEIGHBORHOOD_ID!="N/A", NEIGHBORHOOD_ID!="142", NEIGHBORHOOD_ID!="150", NEIGHBORHOOD_ID!="214")%>%
  summarise(MedianSalePrice=log(median(SALE_PRICE)), SDSalePrice=(sd(SALE_PRICE)), UnitsSold=(sum(RESIDENTIAL_UNITS)), Sales = n())%>%
  as.data.frame()


#Elbow Method
library(factoextra)
fviz_nbclust(zscores,kmeans,method="silhouette")
fviz_nbclust(zscores,kmeans,method="wss")

ggplot(zscores)+geom_point(mapping=aes(x=MedianSalePrice, y=UnitsSold, size=SDSalePrice))

k<- kmeans(zscores, centers = 2)
DataClusterNum <- cbind(GroupedData, k$cluster)

ggplot(DataClusterNum)+geom_point(mapping=aes(x=MedianSalePrice, y=UnitsSold, size=SDSalePrice, colour = k$cluster))



# T TEST

PrepTTest<- AllData_Joined %>%
  mutate(SALE_YEAR=year(SALE_DATE))%>%
  filter(SALE_PRICE>75000, GROSS_SQUARE_FEET>300)

BaysideData <- filter(.data = PrepTTest, NEIGHBORHOOD_NAME=="BAYSIDE", SALE_YEAR>=2009, TYPE=="RESIDENTIAL")
AstoriaData <- filter(.data = PrepTTest, NEIGHBORHOOD_NAME=="ASTORIA", SALE_YEAR>=2009, TYPE=="RESIDENTIAL")

t.test(x=BaysideData$SALE_PRICE, y=AstoriaData$SALE_PRICE, alternative = "two.sided", conf.level = .95)
