#####################################
#Bodo Gruetter, bodojeremy.gruetter@stud.hslu.ch
#Malik Sogukoglu, malik.sogukoglu@stud.hslu.ch

#####Import Packages#####
library(mice)
library(corrplot)
library(ggplot2)
library(ggpubr)
library(dplyr)

#####Prepare Environment#####
# Set seed for reproducability
set.seed(123)
# clear environment
rm(list = ls())
# set plotting window to default
par(mfrow = c(1, 1))

d.airquality <- read.csv("air_quality_taiwan.csv", header=TRUE)
head(d.airquality)
nrow(d.airquality)

##check datatype of columns
sapply(d.airquality, class)

##transform datatype of numeric values to numeric
num_predictors = c("AMB_TEMP","CH4","CO","NMHC","NO",
                   "NO2","NOx","O3","PH_RAIN","PM2.5","RAINFALL", "RAIN_COND",
                   "RH", "SO2", "THC", "UVB", "WD_HR", "WIND_DIREC",
                   "WIND_SPEED", "WS_HR")
d.airquality.notnum <- data.frame(as.Date(d.airquality$time), d.airquality$station)
colnames(d.airquality.notnum) <- c("date", "station")
d.airquality.num <- sapply(d.airquality[num_predictors], as.numeric)
d.airquality <- cbind(d.airquality.notnum, d.airquality.num)
sapply(d.airquality, class)

##replace NA from columns PH_RAIN, RAINFALL, RAIN_COND with 0
d.airquality$PH_RAIN[is.na(d.airquality$PH_RAIN)] <- 0
d.airquality$RAINFALL[is.na(d.airquality$RAINFALL)] <- 0
d.airquality$RAIN_COND[is.na(d.airquality$RAIN_COND)] <- 0

##get a better understanding for missing NAs
sum(is.na(d.airquality))
md.pattern(d.airquality)
d.airquality <- na.omit(d.airquality)
nrow(d.airquality)

#####Graphical Analysis#####
## Investigating the Correlation between all predictors
all_predictors = c("date", "station", "AMB_TEMP","CH4","CO","NMHC","NO",
                   "NO2","NOx","O3","PH_RAIN","PM2.5","RAINFALL", "RAIN_COND",
                   "RH", "SO2", "THC", "UVB", "WD_HR", "WIND_DIREC",
                   "WIND_SPEED", "WS_HR")
corrplot(cor(data.matrix(d.airquality[all_predictors])))

##Investigating temperature
hist(d.airquality$AMB_TEMP)

mean(d.airquality$AMB_TEMP)
sd(d.airquality$AMB_TEMP)

##Testing on normal distribution
ggqqplot(d.airquality$AMB_TEMP)

View(d.airquality)