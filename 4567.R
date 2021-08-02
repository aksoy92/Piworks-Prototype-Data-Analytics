

zrequire('timeDate')
library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls) 
library(elasticnet)
library(broom) 
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix) 
library(kernlab) 
library(e1071) 
library(rpart) 
library(pgmm) 
library(dslabs)
library(rpart.plot) 
library(partykit) 
library(ipred) 
library(randomForest)
library(gbm)
library(nnet)
library(neuralnet)
library(GGally)
library(NeuralNetTools) 
library(FNN)
require(readxl)
require(ggplot2)
require(dplyr)
require(lubridate)
require(naniar)
require(caret)
require(lattice)
require(hexbin)
require(corrplot)
require(scales)
require(tidyverse)
library(Dict)
require(caTools)
require(gbm)
require(TTR)
library(reshape)
library(timeDate)
library(caret)
library(pspline)
library(plyr)
library(dplyr)
library(naniar)
library(caret)
library(tidyverse)
library(AppliedPredictiveModeling)
library(pls) 
library(elasticnet)
library(broom) 
library(glmnet)
library(MASS)
library(ISLR)
library(PerformanceAnalytics)
library(funModeling)
library(Matrix) 
library(kernlab)
library(e1071) 
library(rpart) 
library(pgmm)  
library(dslabs)
library(rpart.plot) 
library(partykit)  
library(ipred)
library(randomForest)
library(gbm)
library(nnet)
library(neuralnet)
library(GGally)
library(NeuralNetTools) 
library(FNN)


####QUESTION 3 

#### 

dfplot <-as.data.frame(read_xlsx("DailyActivities.xlsx"))

df1 = melt(dfplot, id.vars=c("Area of Interest"))
colnames(df1)=c("Area of Interest","Person","Hour")
str(df1)

p <- ggplot(df1, aes(x=`Area of Interest`, y=Hour)) + 
  geom_boxplot() + geom_point(aes(colour=Person))+ ylim(0,12)















###############   QUESTION 4


df <-as.data.frame(read_csv("country_vaccination_stats.csv"))
cdf  <-as.data.frame(read_csv("country_vaccination_stats.csv"))


# NA 

vis_miss(df)
str(df)

df$date <- as.Date(df$date, format =  "%m/%d/%Y")
df<- df %>% arrange(date)

df$dateindex <- c(1,1+cumsum(diff(df$date)!=0))


### create a datafrime which contains min values
#for each country (sil)

sil<- df %>% 
  group_by(country) %>% 
  slice(which.min(daily_vaccinations)) %>% 
  dplyr::select(country,daily_vaccinations)

colnames(sil)[2]<-"minvalue"

df <- left_join(df, sil, by = c("country"))

##assign 0 to na in minvalue column(no values as like kuwait )
df[is.na(df$minvalue),]$minvalue<-0

## final result daily_vaccinations

df<-  df %>%mutate(daily_vaccinations = case_when(
  !is.na(daily_vaccinations)~ daily_vaccinations,
  is.na(daily_vaccinations) ~ minvalue))



####### Question 5


df<-df %>% arrange(country, date)

df<-df %>%dplyr::group_by(country) %>% dplyr::mutate(median=median(daily_vaccinations))

##top 3 

dfmax <- as.data.frame(df %>% 
  group_by(country) %>% 
  slice(which.max(median)) %>% 
  dplyr::select(country,median)) 

###Answer dfmax 
dfmax %>% slice_max(median, n = 3)


####### Question 6
##YEAR-MM-DD 
df  %>% filter(date== "2021-01-06")   %>% summarise(total=sum(daily_vaccinations ))





#######################################    Question 6 #############################################


##library(sqldf)
install.packages('Rcpp')
library(Rcpp)
library(sqldf)

sqltbl <-as.data.frame(read_csv("country_vaccination_stats.csv"))

sqltbl$date <- as.Date(sqltbl$date, format =  "%m/%d/%Y")
sqltbl<- sqltbl %>% arrange(date)

### create table that stores median values per country
tblsql_median<- sqldf(
  "SELECT country,MEDIAN(daily_vaccinations) as median
   FROM sqltbl
   GROUP BY country")

tblsql_join<-  sqldf(
      "SELECT a.country as country ,a.date as date ,a.daily_vaccinations as daily_vaccinations,
       b.median as median from sqltbl as a  left join tblsql_median as b on a.country=b.country
      ")

test_sqltable <- sqldf("Select *, ifnull(daily_vaccinations ,median) as assignmedian From tblsql_join")


### ####################################  Question 6 Answer = finalsqltable

finalsqltable <-  sqldf("Select *, ifnull(assignmedian ,0) as updatedvaccitaions From test_sqltable")


#######################################    Question 7 #############################################
install.packages('gsubfn')
library('gsubfn')

#### I created excel file which demonstrates html tags 
dflist <-as.data.frame(read_xlsx("links.xlsx"))


##Xml tags and protocol parts is guaranteed to be lower case  
####Access link part that we are interested in can have alpha-numeric, case insensitive characters, 
#underscore ( _ ) character and dot ( . ) character only.

toreplace<-list("https://" = "", "http://"= "","<url>"="","</url>"="")
dflist[,2]<-gsubfn(paste(names(toreplace),collapse="|"),toreplace,dflist[,2])
dflist[,2]<-tolower(dflist[,2])

#Answer 
print(dflist)
