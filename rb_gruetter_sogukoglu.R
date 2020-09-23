### Import packages

library(tidyverse)
library(corrplot)
library("ggplot2")


### Prepare Data

# CSV is read whereby zero values are explicitly marked as NAs
df.cars <- read.csv2(file="autos.csv", sep = ",", header = T, na.strings = c("", " ", "NA"))
# remove NA

df.cars <- na.omit(df.cars)
df.cars

# To predict depreciation of cars, it is useful to select "price" as the target variable. 
# "kilometres" and "yearOfRegistration" could be good independent variables.

# With the following plots it was shown how "kilometres" resp. "yearOfRegistration" stand to the variable "price".


plot(df.cars$price ~df.cars$kilometer)

plot(df.cars$price ~df.cars$yearOfRegistration)


# The unrealistic "price" and "yearOfRegistration" data in the plots above clearly show 
# that there are outliers which should be filtered. These unrealistic values are probably caused by crawling.



# filter outliers:
# to get rid of these unrealistic values, only cars with a 
# value up to 100000 Euros and only the models between 2010 and 2016 are considered 

df.cars <- df.cars %>% filter(yearOfRegistration > 2010, 
                                      yearOfRegistration < 2016, 
                                      price < 100000)

# the following example-boxplot shows that by filtering to the years 2010 - 2016 the graphic became much more visible
boxplot(df.cars$price ~df.cars$yearOfRegistration)

# columns lastSeen,nrOfPictures,dateCreated,dateCrawled are removed since they do not provide useful information
df.cars = subset(df.cars, select = -c(lastSeen,nrOfPictures,dateCreated,dateCrawled) )

# the "age" of the car would be an important value to calculate the depreciation. 
# Since it was not included in the data set, it is inserted here as a new column. 
# info: 2016 is the year of data collection

df.cars$age <- 2016 - df.cars$yearOfRegistration


# see if/how many "NA"s exist
anyNA(df.cars)
apply(df.cars, MARGIN = 2, 
      FUN = function(x) {sum(is.na(x))})





### Graphical Analysis


# correlation analysis

# we assumed that "yearOfRegistration" and "kilometer" strongly influence the "price". 
# with the following correlation analyis we want to see which numeric variables actually influence the price.




# df.cars.numeric <- df.cars[, sapply(df.cars, is.numeric)]
# 
# cars.cor = cor(df.cars.numeric)
# 
# corrplot(cars.cor)


predictors = c(colnames(df.cars))
corrplot(cor(data.matrix(df.cars[predictors]) ))

# Here we see that 
# - yearOfRegistration
# - powerPS
# - age (calculated from yearOfRegistration)
# - kilometer
# - name
# - brand
# - fuel type
# - gearbox
# - vehicle Type
# - notRepairedDamage
# - postalCode
# ...have a certain influence on the price. 
# So there are also variables that we did not see as having a strong effect previously.


# In the following analysis we will take a closer look at
# the effects of the variables that are influential according to the correlation analysis.


# ...lets start with the categorical values
# categorical variables are best explaint with a boxplot and anova-test. 

# see which are the categorical
sapply(df.cars, class)

# the categorical independent variables to be considered are
# - yearOfRegistration
# - name
# - brand
# - fuel type
# - gearbox
# - vehicle Type
# - notRepairedDamage
# - postalCode

## year of registration

ggplot(data = df.cars, aes(group=yearOfRegistration, y = price, x = as.factor(yearOfRegistration))) +
   geom_boxplot() + xlab("year of registration") + ylab("price")


lm.yearOfRegistration.1 <- lm(price ~ as.factor(yearOfRegistration), data = df.cars)
lm.yearOfRegistration.0 <- lm(price ~ 1, data = df.cars)
anova(lm.yearOfRegistration.0, lm.yearOfRegistration.1)


# the boxplot above clearly shows that the year of registration influences the price, because newer cars cost more. 
# the anova test shows that there is a strong evidence that at least one year-level influences the price (see p-value) .
# the anova test shows also that the model only having yearOfRegistration as independent variable 
# has smaller residuals than the complex model (see RSS).


## name
# because name has many levels it is not suitable for anova test or plotting



## brand

ggplot(data = df.cars, aes(group=brand, y = price, x = as.factor(brand))) +
   geom_boxplot() + xlab("brand") + ylab("price")


lm.brand.1 <- lm(price ~ as.factor(brand), data = df.cars)
lm.brand.0 <- lm(price ~ 1, data = df.cars)
anova(lm.yearOfRegistration.0, lm.brand.1)
# the boxplot above clearly shows that the brand influences the price. 
# the anova test shows that there is a strong evidence that at least one brand influences the price (see p-value) .
# the anova test shows also that the model only having brand as independent variable has smaller residuals than the complex model (see RSS).



## vehicleType
ggplot(data = df.cars, aes(y = price, x = vehicleType)) +
   geom_boxplot() + xlab("vehicle Type") + ylab("price")

lm.vehicleType.1 <- lm(price ~ as.factor(vehicleType), data = df.cars)
lm.vehicleType.0 <- lm(price ~ 1, data = df.cars)
anova(lm.vehicleType.0, lm.vehicleType.1)
# the boxplot above clearly shows that the vehicle type influences the price. 
# the anova test shows that there is a strong evidence that at least one vehicle-type influences the price (see p-value) .
# the anova test shows also that the model only having vehicleType as independent variable has smaller residuals than the complex model (see RSS).






## numeric variables
# - yearOfRegistration
# - powerPS
# - kilometer
# - age


plot_cars <- ggplot(
   data = df.cars, 
   mapping = aes(x = yearOfRegistration, 
                 y = price)
)  + geom_point(alpha = 0.4) +
   geom_smooth(method = "lm", se=FALSE) +
   scale_color_brewer(type = "qual", palette = "Dark2") +
   ggtitle("Price based on Kilometers (per vehicle type)")

plot_cars









# linear models
#summary(lm(df.cars$price ~df.cars$kilometer * df.cars$vehicleType))




## Modelling
lm.cars <-  lm(df.cars$price ~df.cars$kilometer 
           + df.cars$yearOfRegistration
           + df.cars$powerPS
           + df.cars$postalCode
           + df.cars$vehicleType

           + df.cars$gearbox
           + df.cars$brand

           + df.cars$nrOfPictures
           + df.cars$notRepairedDamage

           + df.cars$fuelType
       
           )
summary(lm.cars)

anova(lm.cars)

#kilometer seems to be an important predictor for price

qqnorm(df.cars$kilometer)

qqline(df.cars$kilometer)


qqnorm(df.cars$age)

qqline(df.cars$age)



lm.cars.kilometer <- lm(df.cars$price ~df.cars$kilometer)

summary(lm.cars.kilometer)


lm.cars.age <- lm(df.cars$price ~df.cars$age)

summary(lm.cars.age)





















#some further analyses

#costs per vehcile type
boxplot(df.cars$price ~ df.cars$vehicleType)
#t.test(df.cars$price ~ df.cars$vehicleType[df.cars$vehicleType =="cabrio" & "coupe"])


# additional section
dfPrizeMean <- df.cars %>%
   group_by(vehicleType) %>%
   summarize(meanPrice = mean(price))
dfPrizeMean

dfPrizeMean <- df.cars %>%
   group_by(brand) %>%
   summarize(meanPrice = mean(price))
dfPrizeMean



# "Price based on Kilometers (per vehicle type)"
plot_cars <- ggplot(
   data = df.cars, 
   mapping = aes(x = kilometer, 
                 y = price,
                 colour = vehicleType)
)  + geom_point(alpha = 0.4) +
   geom_smooth(method = "lm", se=FALSE) +
   scale_color_brewer(type = "qual", palette = "Dark2") +
   facet_wrap( ~ vehicleType) +
   scale_x_log10() +
   ggtitle("Price based on Kilometers (per vehicle type)")

#df.cars.suv <- df.cars[df.cars$vehicleType == "suv", ]




# "Price based on year of registration (per vehicle type)"

plot_cars <- ggplot(
   data = df.cars, 
   mapping = aes(x = yearOfRegistration, 
                 y = price,
                 colour = vehicleType)
)  + geom_point(alpha = 0.4) +
   geom_smooth(method = "lm", se=FALSE) +
   scale_color_brewer(type = "qual", palette = "Dark2") +
   facet_wrap( ~ vehicleType) +
   scale_x_log10() +
   ggtitle("Price based on year of registration (per vehicle type)")

plot_cars



plot_cars



# predictions

# 1. Add predictions 
pred.int <- predict(lm.cars.kilometer, interval = "prediction")
mydata <- cbind(df.cars, pred.int)
# 2. Regression line + confidence intervals

p <- ggplot(mydata, aes(kilometer, price)) +
   geom_point() +
   stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")




# 1. Add predictions 
pred.int <- predict(lm.cars.age, interval = "prediction")
mydata <- cbind(df.cars, pred.int)
# 2. Regression line + confidence intervals

p <- ggplot(mydata, aes(age, price)) +
   geom_point() +
   stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")



