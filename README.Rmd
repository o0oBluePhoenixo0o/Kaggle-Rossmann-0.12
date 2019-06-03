# The Running Rossmann Stores

## Summary

In this project, I first did some exploratory analysis with modified codes based on some kernels in Kaggle. Then I added "States" as a feature to the dataset after cross-referenced the data with holidays to identify stores locations. Finally, applied different algorithms and pick the best (H2O Random Forest is currently the best performer with RMSE at 0.12).

## Overview

The training data set for the Rossman Store Sales consists of sales data for a set of 1115 stores across 942 days. If we multiply those two numbers together we would would expect to find a data set with **1050330** observations, however, there are in fact only **1017209** observations in the training set.

When performed EDA on the dataset, I figured out that normal *timeseries forecast* cannot be done because the missing gap is too big in the dataset (there is at least 3 - 4 months period where only a small number of stores reported sales). Thus, the first part of this notebook will focus on how to deal with mising data imputation (feature by feature), then follow up with discussion on which route to take on (timeseries or different regression techniques)



## Initialize packages


```r
# load packages and set options
options(stringsAsFactors = FALSE)

# install packages if not available
packages <- c("readr","data.table", #read data
              "lubridate", "zoo", #date time conversion
              "tidyverse", # full set of pkgs
              "dplyr", #data exploratory + manipulation
              "caTools", # features engineering
              "VIM", # visualizing missing data
              "ggplot2","ggthemes", "corrplot", # plotting graphs
              "caret", # ML libs
              "Hmisc" # EDA
)

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, require, character.only = TRUE)
```

## Read data

Read train, test and submit sample data from input folder


```r
# set TryCatch for input folder (on kaggle kernel or local)

file2read.df <- c('test','train','store')

readinputs <- function(filename){
   assign(filename,
          tryCatch(fread(paste0("./input/",filename,".csv")),
                 error = function(e){
                   print('Detect working environment is not Kaggle kernel')
                   fread(paste0("../input/",filename,".csv"))
                 })
          ,envir = .GlobalEnv) 
}

for (i in 1:length(file2read.df)){
  readinputs(file2read.df[i])
}
```

As stated in this official discussion [Link](https://www.kaggle.com/c/rossmann-store-sales/discussion/17048#96921), the codes bellow fixes the missing data 


```r
test <- test %>% mutate(Open = replace(Open, which(Store == 622 &
                                             Date > as.Date('2015-09-04') &
                                             Date < as.Date('2015-09-18') &
                                             is.na(Open) == TRUE),0))
```

# Exploratory Data Analysis (EDA)
Merge train and test dataset to get an overview of all features. Since the test dataset does not have "Customers" and "Sales", I added dummy features to match the binding. Also removed the "Id" column.


```r
# Change type for "Date"
train$Date <- parse_date_time(train$Date,'%y-%m-%d') #lubridate pkg is fastest
test$Date <- parse_date_time(test$Date, '%y-%m-%d')

# Add and remove variables to test
tmp <- test %>%
  select(-Id) %>%
  mutate(Sales = NA,
         Customers = NA)

full <- rbind(train,tmp)
```

Some of the charts were taken from [Exploratory Analysis Rossmann Kernel](https://www.kaggle.com/thie1e/exploratory-analysis-rossmann) with modifications.
Explore histograms between sales and customers.


```r
# Sales histograms
hist(train$Sales, 100,
     main = "Sales per store",
     xlab = "Sales")
```

![](images/EDA%20-%20Charts-1.png)<!-- -->

```r
hist(aggregate(train[Sales != 0]$Sales, 
               by = list(train[Sales != 0]$Store), mean)$x, 100, 
     main = "Mean sales per store when store was not closed",
     xlab = "Sales")
```

![](images/EDA%20-%20Charts-2.png)<!-- -->

```r
# Customer histograms
hist(train$Customers, 100,
     main = "Customers per store",
     xlab = "Customers")
```

![](images/EDA%20-%20Charts-3.png)<!-- -->

```r
hist(aggregate(train[Sales != 0]$Customers, 
               by = list(train[Sales != 0]$Store), mean)$x, 100,
     main = "Mean customers per store when store was not closed",
     xlab = "Customers")
```

![](images/EDA%20-%20Charts-4.png)<!-- -->

```r
# Sales per day of the week
ggplot(train[Sales != 0],
       aes(x = factor(DayOfWeek), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA) + 
  labs(x = "Weekdays", title = "Sales per day of week")
```

![](images/EDA%20-%20Charts-5.png)<!-- -->

The plot below shows the differences in sales before and after competitors open in the same area.**147** stores
had a competitor move into their area during the available time span. The competition leaves a 'dent' in the sales which looks a little different depending on the chosen `timespan`.


```r
# Convert the CompetitionOpenSince... variables to one Date variable
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))
# Merge store and train 
train_store <- merge(train, store, by = "Store")

# Sales before and after competition opens
train_store$DateYearmon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]
timespan <- 100 # Days to collect before and after Opening of competition
beforeAndAfterComp <- function(s) {
    x <- train_store[Store == s]
    daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
    if (any(!daysWithComp)) {
        compOpening <- head(which(!daysWithComp), 1) - 1
        if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
           x <- x[(compOpening - timespan):(compOpening + timespan), ] 
            x$Day <- 1:nrow(x)
            return(x)
        }
    }
}
temp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)
temp <- do.call(rbind, temp)
# 147 stores first had no competition but at least 100 days before the end
# of the data set
length(unique(temp$Store))
```

```
## [1] 147
```

```r
ggplot(temp[Sales != 0], aes(x = Day, y = Sales)) + 
    geom_smooth() + 
    ggtitle(paste("Competition opening around day", timespan))
```

```
## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'
```

![](images/EDA%20-%20Sales%20b4%20and%20after%20competition-1.png)<!-- -->

```r
rm(temp)
```


The different store types and assortment types imply different overall levels of sales and seem to be exhibiting different trends:


```r
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
    geom_smooth(size = 2)
```

![](images/EDA%20-%20Diff%20Stores%20sales-1.png)<!-- -->

```r
ggplot(train_store[Customers != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(StoreType))) + 
    geom_smooth(size = 2)
```

![](images/EDA%20-%20Diff%20Stores%20sales-2.png)<!-- -->
```r
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(Assortment))) + 
    geom_smooth(size = 2)
```

![](images/EDA%20-%20Diff%20Stores%20sales-3.png)<!-- -->

```r
ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Customers, color = factor(Assortment))) + 
    geom_smooth(size = 2)
```
![](images/EDA%20-%20Diff%20Stores%20sales-4.png)<!-- -->

From the graph below, the relationship between 'Competitor distance' and 'Sales' is a little bit counterintuitive. We can see that the closer the competitors, the higher the sales. The reason might be that these shops are already in highly dense area (like shopping districts or main streets).


```r
salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
               by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)

colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
    geom_point() + geom_smooth()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](images/EDA%20-%20sales%20vs%20comp%20dist-1.png)<!-- -->

**Missing dates**

We can figure out how many sales date data is missing by counting the number of stores rows exist in a particular time period. As can be seen from the graph, the gap occurs from end of June '14 until end of the year '14 (the 2nd graph shows a smaller scale)


```r
by_Date <- train %>% group_by(Date) %>% 
  summarise(Num_Stores=n())

# Overview of missing stores values period
ggplot(by_Date, aes(Date,Num_Stores)) + geom_line()
```

![](images/EDA%20-%20Stores%20count%20by%20date-1.png)<!-- -->

Visualize missing data using **VIM** package. Seems like the data is free from missing values after the hot fix from official channel.


```r
aggr_plot <- VIM::aggr(full %>% select(-c("Sales","Customers")), 
                       col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3,
                       ylab=c("Histogram of missing data","Pattern"))
```

![](images/EDA%20-%20Missing%20data-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##       Variable Count
##          Store     0
##      DayOfWeek     0
##           Date     0
##           Open     0
##          Promo     0
##   StateHoliday     0
##  SchoolHoliday     0
```

# Features Engineering

## States as a feature
- Since the stores locations are scattered in EU, it is likely that multiple stores would have different *"StateHoliday"*. However, inspired from this repository in Python of @gereleth [Putting stores on map](https://gist.github.com/gereleth/781a92c97b58b07a11e2#file-putting_stores_on_map-ipynb), I decided to adopt the code and translate into R to add states as a feature to our dataset.

* External sources:
  + Public holidays data: http://www.timeanddate.com/holidays/germany/2013
  + School holidays data: http://www.holidays-info.com/School-Holidays-Germany/2015/school-holidays_2015.html
Start by grouping and cross-referencing stores with State Holidays data


```r
# Overview of public holiday dates and numbers of stores celebrating
holiday <- train %>% filter(StateHoliday == 'a')

## Saxony (SN) - Repentance Day (20-11-2013)
SN <- holiday %>% filter(Date == ymd("2013-11-20"))

## BW_BY_ST - Epiphany (06-01-2013)
BW_BY_ST <- holiday %>% filter(Date == ymd("2013-01-06"))

## BW_BY_HE_NW_RP_SL - Corpus Christi (30-05-2013) (also with SN)
BW_BY_HE_NW_RP_SL <- holiday %>% filter(Date == ymd("2013-05-30"))
# Remove SN from the list
BW_BY_HE_NW_RP_SL <- anti_join(BW_BY_HE_NW_RP_SL,SN, by = "Store")

## BY_SL - Assumption of Mary (15-08-2013)
BY_SL <- holiday %>% filter(Date == ymd("2013-08-15"))

## BB_MV_SN_ST_TH - Reformation day (31-10-2013)
BB_MV_SN_ST_TH <- holiday %>% filter(Date == ymd("2013-10-31"))

## BW_BY_NW_RP_SL- All Saint's Day (01-11-2013)
BW_BY_NW_RP_SL <- holiday %>% filter(Date == ymd("2013-11-01"))

################################
# Intersecting and Setxoring to split states
BW_BY <- semi_join(BW_BY_ST, BW_BY_HE_NW_RP_SL, by = "Store")
ST <- anti_join(BW_BY_ST, BW_BY, by = "Store")
BY <- semi_join(BW_BY, BY_SL, by = "Store")
SL <- anti_join(BY, BY_SL, by = "Store")
BW <- anti_join(BW_BY,BY, by = "Store")
HE <- anti_join(BW_BY_HE_NW_RP_SL, BW_BY_NW_RP_SL, by = "Store")
BB_MV_TH <- anti_join(anti_join(BB_MV_SN_ST_TH, SN, by = "Store"),ST, by = "Store")
NW_RP <- anti_join(BW_BY_NW_RP_SL, BW_BY, by = "Store")

allstores <- train %>% distinct(Store) # get all stores ID
BE_HB_HH_NI_SH <- anti_join(anti_join(allstores, BW_BY_HE_NW_RP_SL, by = "Store"), BB_MV_SN_ST_TH, by = "Store")
```

Now using school holiday to further intersecting and setoxring to cluster stores into states


```r
# 26-03-2015 is a school holiday in RP but now NW
RP <- train %>% 
  filter(Date == ymd("2015-03-26") & SchoolHoliday == 1) %>%
  semi_join(NW_RP, by = "Store")
NW <- anti_join(NW_RP, RP, by = "Store")

# Winter holidays not lasting till Feb 14th rule out MV. 
# Easter holidays starting on Mar 30th suggest TH. 
# Confirmed by summer holidays starting on Jul 13th.
TH <- BB_MV_TH

# Only HH has easter holidays on Mar 2nd. And on May 5th.
HH <- train %>%
  filter(Date ==ymd("2015-03-02") & SchoolHoliday == 1) %>%
  semi_join(BE_HB_HH_NI_SH, by = "Store")

BE_HB_NI_SH <- anti_join(BE_HB_HH_NI_SH, HH, by = "Store")
# Only SH has holidays on Apr 17th. And on Feb 2nd.
SH <- train %>%
  filter(Date == ymd("2015-04-17") & SchoolHoliday == 1) %>%
  semi_join(BE_HB_NI_SH, by = "Store")

BE_HB_NI <- anti_join(BE_HB_NI_SH, SH, by = "Store")

#  Mar 25th is not a holiday in BE.
BE <- train %>%
  filter(Date == ymd("2015-03-25") & SchoolHoliday == 1) %>%
  semi_join(BE_HB_NI, by = "Store")

HB_NI <- anti_join(BE_HB_NI, BE, by = "Store")
```

Save the states into **store** dataset and plot the results of store numbers in **train** and **test** datasets. We can see that the **test** dataset does not contain store data from 5 states: **HB, NI, SN, ST** and **TH**.

```r
#### Saving results to "store" df
store <-store %>%
  mutate(States = case_when(
    Store %in% intersect(store$Store, BW$Store) ~ "BW",
    Store %in% intersect(store$Store, BY$Store) ~ "BY",
    Store %in% intersect(store$Store, BE$Store) ~ "BE",
    Store %in% intersect(store$Store, HB_NI$Store) ~ "HB, NI",
    Store %in% intersect(store$Store, HH$Store) ~ "HH",
    Store %in% intersect(store$Store, HE$Store) ~ "HE",
    Store %in% intersect(store$Store, NW$Store) ~ "NW",
    Store %in% intersect(store$Store, RP$Store) ~ "RP",
    Store %in% intersect(store$Store, SN$Store) ~ "SN",
    Store %in% intersect(store$Store, ST$Store) ~ "ST",
    Store %in% intersect(store$Store, SH$Store) ~ "SH",
    Store %in% intersect(store$Store, TH$Store) ~ "TH"))

# Plot results
tmp1 <-store %>% 
  semi_join(train, by = "Store") %>%
  select(Store, States) %>%
  mutate(Dataset = "train") %>%
  group_by(States, Dataset) %>%
  tally()

tmp2 <-store %>% 
  semi_join(test, by = "Store") %>%
  select(Store, States) %>%
  mutate(Dataset = "test") %>%
  group_by(States, Dataset) %>%
  tally()

tmp <- rbind(tmp1,tmp2)

ggplot(tmp, aes(States, n, fill = Dataset)) +
  geom_bar(stat="identity", position = "dodge") + 
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Number of Stores")
```

![](images/FE%20-%20Save%20and%20plot%20results-1.png)<!-- -->

Next, I will split the time into 3 separate features: Year, Month, Week. Next, I perform log transformation on the **Sales** feature since the data spans accross a large timeline (rules of thumb). 


```r
train <- train %>%
  mutate(Week = lubridate::week(Date),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date),
         logSales = log1p(Sales)) 
test <- test %>%
  mutate(Week = lubridate::week(Date),
         Month = lubridate::month(Date),
         Year = lubridate::year(Date)) 
```

## Combine with store data

Perform some FE on stores dataset


```r
# impute Competition Values 
store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 75000 # Dealing with NA

store$CompetitionStrength <- cut(store$CompetitionDistance, breaks=c(0, 1500, 6000, 12000, 20000, Inf), labels=FALSE) 
# 15 min, 1/2/3 hours (or walking and 10/20/30 min driving)

store <- store %>%
  mutate(SundayStore = as.numeric(store$Store %in% unique(train$Store[train$DayOfWeek==7 & train$Open==1])), #detect Sunday-store
         StoreType = as.factor(StoreType),
         Assortment = as.factor(Assortment))
```


In this step, I combined both existing training and testing dataset with stores to form the final consolidation dataset.


```r
# Merge store with train and test datasets
train <- inner_join(train, store, by = "Store")
test <- inner_join(test, store, by = "Store")

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- 0
test[is.na(test)]   <- 0

# Set DayOfWeek, Week, Month, Year as factor (experimenting)
train <- train %>%
  mutate(Open = as.factor(Open),
         Promo = as.factor(Promo),
         SchoolHoliday = as.factor(SchoolHoliday),
         StateHoliday = as.factor(StateHoliday),
         DayOfWeek = as.factor(DayOfWeek),
         Week = as.factor(Week),
         Month = as.factor(Month),
         Year = as.factor(Year),
         CompetitionOpenSince = as.factor(CompetitionOpenSince),
         States = as.factor(States),
         Promo2 = as.factor(Promo2),
         SundayStore = as.factor(SundayStore))

test <- test %>%
  mutate(Open = as.factor(Open),
         Promo = as.factor(Promo),
         SchoolHoliday = as.factor(SchoolHoliday),
         StateHoliday = as.factor(StateHoliday),
         DayOfWeek = as.factor(DayOfWeek),
         Week = as.factor(Week),
         Month = as.factor(Month),
         Year = as.factor(Year),
         CompetitionOpenSince = as.factor(CompetitionOpenSince),
         States = as.factor(States),
         Promo2 = as.factor(Promo2),
         SundayStore = as.factor(SundayStore))
```

# Models Building

Setup k-fold cross validation with k = 5 (rule of thumb), also set seed for reproductability.
"Features" is a set of chosen features to train the model. Furthermore, after trying to turn **Week**, **DayOfWeek**, **Month** and **Year** variables into **factors**, the error rates reduce significantly case by case (minimum 0.01)


```r
train_control <- trainControl( ## 5-cross validation
  method = "cv",
  number = 5,
  seeds = set.seed(1908)
)
# Create "features" sets
## Set up variable to use all features other than those specified here
col2remove <- c("Id","Date",
                "Sales",
                #"logSales",
                "Customers", 
                "CompetitionDistance", 
                "CompetitionOpenSinceMonth", 
                "CompetitionOpenSinceYear", 
                "Promo2SinceWeek", 
                "Promo2SinceYear", 
                "PromoInterval", 
                "CompetitionEntrance")

# Filter only features for train dataset
features <- colnames(train)[!(colnames(train) %in% col2remove)]
train <- train %>% select(features)

# Filter only features for test dataset
features.test <- features[features !="logSales"]
test <- test %>% select(features.test)
```

Clean up and keep only datasets to free some memory


```r
# Split data to 70-30 (rule of thumb)
set.seed(1908)
intrain <- createDataPartition(y = train$logSales, 
                               p = 0.7, 
                               list = FALSE)
real.train <- train[intrain,] %>% select(features)
real.test <- train[-intrain,] %>% select(features)

# Clean up 
rm(list=ls()[! ls() %in% c("test", "real.test","real.train",
                           "features",
                           #"train","store",
                           "train_control")])
gc()
```

```
##            used  (Mb) gc trigger  (Mb)  max used   (Mb)
## Ncells  2612991 139.6    8114781 433.4   8114781  433.4
## Vcells 17129848 130.7  111800210 853.0 139716842 1066.0
```

### H2O Random Forest (best so far after testing with GLM, GBM, NB)

```r
library(h2o)

h2o.init(nthreads = -1,
         max_mem_size = '9G')
```

```
## 
## H2O is not running yet, starting it now...
## 
## Note:  In case of errors look at the following log files:
##     C:\Users\ngtrungn\AppData\Local\Temp\Rtmp8uheAZ/h2o_ngtrungn_started_from_r.out
##     C:\Users\ngtrungn\AppData\Local\Temp\Rtmp8uheAZ/h2o_ngtrungn_started_from_r.err
## 
## 
## Starting H2O JVM and connecting: . Connection successful!
## 
## R is connected to the H2O cluster: 
##     H2O cluster uptime:         5 seconds 397 milliseconds 
##     H2O cluster timezone:       Europe/Berlin 
##     H2O data parsing timezone:  UTC 
##     H2O cluster version:        3.22.1.1 
##     H2O cluster version age:    2 months and 19 days  
##     H2O cluster name:           H2O_started_from_R_ngtrungn_wxu457 
##     H2O cluster total nodes:    1 
##     H2O cluster total memory:   7.99 GB 
##     H2O cluster total cores:    12 
##     H2O cluster allowed cores:  12 
##     H2O cluster healthy:        TRUE 
##     H2O Connection ip:          localhost 
##     H2O Connection port:        54321 
##     H2O Connection proxy:       NA 
##     H2O Internal Security:      FALSE 
##     H2O API Extensions:         Algos, AutoML, Core V3, Core V4 
##     R Version:                  R version 3.5.0 (2018-04-23)
```

```r
trainHex <- as.h2o(real.train)
testHex <- as.h2o(real.test)

# run with default first
rfHex <- h2o.randomForest(x = features,
                          y = "logSales",
                          ntrees = 150, # default is 50 
                          max_depth = 30, # default 20
                          nbins_cats = 1115, ## allow it to fit store ID
                          training_frame = trainHex,
                          validation_frame = testHex)
```

```
## 
  |                                                                       
  |                                                                 |   0%
  |                                                                       
  |=================================================================| 100%
```

```r
summary(rfHex) # RMSE 0.13
```

```
## Model Details:
## ==============
## 
## H2ORegressionModel: drf
## Model Key:  DRF_model_R_1553006012574_1 
## Model Summary: 
##   number_of_trees number_of_internal_trees model_size_in_bytes min_depth
## 1             150                      150           591943322        30
##   max_depth mean_depth min_leaves max_leaves  mean_leaves
## 1        30   30.00000     210603     272947 247009.60000
## 
## H2ORegressionMetrics: drf
## ** Reported on training data. **
## ** Metrics reported on Out-Of-Bag training samples **
## 
## MSE:  0.01643395
## RMSE:  0.128195
## MAE:  0.07518717
## RMSLE:  0.01994319
## Mean Residual Deviance :  0.01643395
## 
## 
## H2ORegressionMetrics: drf
## ** Reported on validation data. **
## 
## MSE:  0.01788132
## RMSE:  0.133721
## MAE:  0.07459473
## RMSLE:  0.02211134
## Mean Residual Deviance :  0.01788132
## 
## 
## 
## 
## Scoring History: 
##             timestamp   duration number_of_trees training_rmse
## 1 2019-03-19 15:33:54  0.281 sec               0            NA
## 2 2019-03-19 15:34:01  7.499 sec               1       0.22863
## 3 2019-03-19 15:34:07 12.972 sec               2       0.20612
## 4 2019-03-19 15:34:12 18.653 sec               3       0.19447
## 5 2019-03-19 15:34:17 23.722 sec               4       0.18963
##   training_mae training_deviance validation_rmse validation_mae
## 1           NA                NA              NA             NA
## 2      0.13202           0.05227         0.23194        0.13239
## 3      0.11942           0.04248         0.18264        0.10508
## 4      0.11387           0.03782         0.16631        0.09618
## 5      0.11087           0.03596         0.15901        0.09193
##   validation_deviance
## 1                  NA
## 2             0.05380
## 3             0.03336
## 4             0.02766
## 5             0.02528
## 
## ---
##              timestamp          duration number_of_trees training_rmse
## 36 2019-03-19 15:43:47  9 min 53.270 sec             104       0.12912
## 37 2019-03-19 15:44:40 10 min 46.199 sec             113       0.12886
## 38 2019-03-19 15:45:37 11 min 42.789 sec             123       0.12878
## 39 2019-03-19 15:46:42 12 min 48.027 sec             135       0.12853
## 40 2019-03-19 15:47:52 13 min 57.806 sec             147       0.12828
## 41 2019-03-19 15:48:19 14 min 24.948 sec             150       0.12819
##    training_mae training_deviance validation_rmse validation_mae
## 36      0.07583           0.01667         0.13416        0.07490
## 37      0.07567           0.01660         0.13405        0.07483
## 38      0.07563           0.01658         0.13405        0.07487
## 39      0.07543           0.01652         0.13391        0.07477
## 40      0.07523           0.01646         0.13375        0.07462
## 41      0.07519           0.01643         0.13372        0.07459
##    validation_deviance
## 36             0.01800
## 37             0.01797
## 38             0.01797
## 39             0.01793
## 40             0.01789
## 41             0.01788
## 
## Variable Importances: (Extract with `h2o.varimp`) 
## =================================================
## 
## Variable Importances: 
##                variable relative_importance scaled_importance percentage
## 1                  Open    561425600.000000          1.000000   0.575647
## 2             DayOfWeek    320724064.000000          0.571267   0.328848
## 3          StateHoliday     59903248.000000          0.106698   0.061421
## 4                 Promo     17826004.000000          0.031751   0.018278
## 5  CompetitionOpenSince      2900898.750000          0.005167   0.002974
## 6                  Week      2886233.250000          0.005141   0.002959
## 7           SundayStore      2380979.750000          0.004241   0.002441
## 8             StoreType      1689140.250000          0.003009   0.001732
## 9                 Store      1370907.750000          0.002442   0.001406
## 10        SchoolHoliday      1163009.500000          0.002072   0.001192
## 11               States      1062228.625000          0.001892   0.001089
## 12                Month       581903.437500          0.001036   0.000597
## 13           Assortment       494627.343750          0.000881   0.000507
## 14  CompetitionStrength       473603.875000          0.000844   0.000486
## 15                 Year       220671.218750          0.000393   0.000226
## 16               Promo2       191552.062500          0.000341   0.000196
```

Saving results from H2O Random Forest with RMSE 0.13 as final result for submission

```r
finaltestHex <- as.h2o(test)
## Get predictions out; predicts in H2O, as.data.frame gets them into R
predictions<-as.data.frame(h2o.predict(rfHex,finaltestHex))

## Return the predictions to the original scale of the Sales data
pred <- expm1(predictions[,1])

summary(pred)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    4097    5719    5593    7434   28203
```

```r
submission <- data.frame(Id = test$Store, Sales=pred)
write.csv(submission,'H2O_RF.csv',
          row.names = FALSE)
```
