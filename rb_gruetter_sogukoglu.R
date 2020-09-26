### Import packages

library(tidyverse)
library(corrplot)
library("ggplot2")
library(mgcv)
library(igraph)

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




# we assumed that "yearOfRegistration" and "kilometer" strongly influence the "price". 
# with the following correlation analyis we want to see which numeric variables actually influence the price.



# correlation analysis: see which variables influence price
predictors = c(colnames(df.cars))
corrplot(cor(data.matrix(df.cars[predictors]) ))

# In the correlation analysis we see that 
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
# So there are also variables that we did not think as having a strong effect previously.


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


lm.yearOfRegistration <- lm(price ~ as.factor(yearOfRegistration), data = df.cars)
# lm.cars is the complex model that contains every independent variable
lm.cars <- lm(price ~ 1, data = df.cars)
anova(lm.cars, lm.yearOfRegistration)

# the boxplot above clearly shows that the year of registration influences the price, because newer cars cost more. 
# the anova test shows that there is a strong evidence that at least one year-level influences the price (see p-value) .
# the anova test shows also that the model only having yearOfRegistration as independent variable 
# has smaller residuals than the complex model (see RSS).


## name
# because name has many levels it is not suitable for anova test or plotting



## brand

ggplot(data = df.cars, aes(group=brand, y = price, x = as.factor(brand))) +
   geom_boxplot() + xlab("brand") + ylab("price")


lm.brand <- lm(price ~ as.factor(brand), data = df.cars)
anova(lm.cars, lm.brand)
# the boxplot above clearly shows that the brand influences the price. 
# the anova test shows that there is a strong evidence that at least one brand influences the price (see p-value) .
# the anova test shows also that the model only having brand as independent variable has smaller residuals than the complex model (see RSS).


## fuel type
ggplot(data = df.cars, aes(group = fuelType, y = price, x = as.factor(fuelType))) +
   geom_boxplot() + xlab("fuel type") + ylab("price")

lm.fuelType <- lm(price ~ as.factor(fuelType), data = df.cars)
anova(lm.cars, lm.fuelType)
# the boxplot above certainly shows that the fuel type influences the price. 
# the anova test shows that there is a strong evidence that at least one fuel type-level influences the price (see p-value) .
# the anova test shows also that the model only having fuelType as independent variable has smaller residuals than the complex model (see RSS).


## gearbox
ggplot(data = df.cars, aes(group = gearbox, y = price, x = as.factor(gearbox))) +
   geom_boxplot() + xlab("gearbox") + ylab("price")

lm.gearbox <- lm(price ~ as.factor(gearbox), data = df.cars)
anova(lm.cars, lm.gearbox)
# the boxplot above clearly shows that gearbox influences the price. 
# the anova test shows that there is a strong evidence that at least one gearbox-type influences the price (see p-value) .
# the anova test shows also that the model only having gearbox as independent variable has smaller residuals than the complex model (see RSS).


## vehicleType
ggplot(data = df.cars, aes(y = price, x = as.factor(vehicleType))) +
   geom_boxplot() + xlab("vehicle type") + ylab("price")

lm.vehicleType <- lm(price ~ as.factor(vehicleType), data = df.cars)
anova(lm.cars, lm.vehicleType)
# the boxplot above clearly shows that the vehicle type influences the price. 
# the anova test shows that there is a strong evidence that at least one vehicle-type influences the price (see p-value) .
# the anova test shows also that the model only having vehicleType as independent variable has smaller residuals than the complex model (see RSS).


## notRepairedDamage

ggplot(data = df.cars, aes(y = price, x = as.factor(notRepairedDamage))) +
   geom_boxplot() + xlab("not repaired damage") + ylab("price")

lm.notRepairedDamage <- lm(price ~ as.factor(notRepairedDamage), data = df.cars)
anova(lm.cars, lm.notRepairedDamage)
# the boxplot above clearly shows that the the information whether car has a non-repaired demage or not influences the price. 
# the anova test shows that there is a strong evidence that this information influences the price (see p-value) .
# the anova test shows also that the model only having this information as independent variable has smaller residuals than the complex model (see RSS).

## postalCode

ggplot(data = df.cars, aes(y = price, x = as.factor(postalCode))) +
   geom_boxplot() + xlab("postal Code") + ylab("price")

lm.postalCode <- lm(price ~ as.factor(postalCode), data = df.cars)
anova(lm.cars, lm.postalCode)




## lets go on with the numeric variables which are the following:
# - powerPS
# - kilometer
# - age
# numeric variables are best explained with a classical plot

## powerPS

ggplot(data = df.cars, mapping = aes(y = price, x = powerPS)) + geom_point(alpha = 0.4) +
   geom_point()  + xlab("PS") +ylab("price") +geom_smooth(method = "gam")

# there is little to recognise. therefore the x axis is logaritised in the following plot:

ggplot(data = df.cars, mapping = aes(y = price, x = powerPS)) + geom_point(alpha = 0.4) +
   geom_point()  + xlab("PS") +ylab("price") + scale_x_log10() +geom_smooth(method = "gam")

# In the plot above, one can see that "powerPS" does not show a linear but a quadratic relationship to the "price"
# ....following model should confirm that: 
gam.powerPS <- gam(price ~ s(powerPS), data = df.cars)
summary(gam.powerPS)



## kilometer

ggplot(data = df.cars, mapping = aes(y = price, x = kilometer)) + geom_point(alpha = 0.4) +
   geom_point()  + xlab("kilometer") +ylab("price") + geom_smooth(method = "gam")

# in plot above, one can see that kilometres seems to have a linear effect on price.
# ...floowing model should confirm that:
lm.kilometer <- lm(price ~ kilometer, data = df.cars)
summary(lm.kilometer)
# and indeed the p-value shows that "kilometer" has a linear effect on the price


## age

ggplot(
   data = df.cars, 
   mapping = aes(x = age, 
                 y = price)
)  + geom_point(alpha = 0.4) +
   geom_smooth(method = "lm", se=FALSE) +
   scale_color_brewer(type = "qual", palette = "Dark2") +
   scale_x_log10() +
   ggtitle("Price based on age")

# the above plot time that age has a linear influence on price. 
# note: because there were not enough values in the x-axis, "lm" was used for the smooth-line instead of "gam"

# following model confirms that age has a linear effect on price.
lm.age <- lm(price ~ age, data = df.cars)
summary(lm.age)

## Modelling
##########Bodo
##Different models
full.model.1 <- price ~ seller + offerType + abtest + vehicleType + yearOfRegistration +
   gearbox + powerPS + model + kilometer + monthOfRegistration + fuelType + brand +
   notRepairedDamage + postalCode + age
starting.model.1 <- price ~ yearOfRegistration + brand + fuelType + gearbox + vehicleType +
   powerPS + kilometer + age


##linear modelling
lm.full.model.1 <- lm(full.model.1, data = df.cars)
lm.starting.model.1 <- lm(starting.model.1, data = df.cars)
lm.medium.model.1 <- lm(medium.model.1, data = df.cars)
lm.simple.model.1 <- lm(simple.model.1, data = df.cars)

##Proof
summary(lm.full.model.1)
summary(lm.starting.model.1)
summary(lm.medium.model.1)
summary(lm.simple.model.1)

##Updating
lm.starting.model.1 <- update(lm.starting.model.1, . ~ . - abtest)

final.model.1 <- price ~ seller + offerType + abtest + vehicleType + yearOfRegistration +
   gearbox + powerPS + model + kilometer + monthOfRegistration + fuelType + brand +
   notRepairedDamage + postalCode + age

##########Malik
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


## additional section
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


pred.int <- predict(lm.cars.kilometer, interval = "prediction")
mydata <- cbind(df.cars, pred.int)


p <- ggplot(mydata, aes(kilometer, price)) +
   geom_point() +
   stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")





pred.int <- predict(lm.cars.age, interval = "prediction")
mydata <- cbind(df.cars, pred.int)
# 2. Regression line + confidence intervals

p <- ggplot(mydata, aes(age, price)) +
   geom_point() +
   stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")



######ipgraph#######
##A simple example
users <- data.frame(name=c("Billy", "Harry", "Ruth"))
relations <- data.frame(from=c("Billy", "Harry", "Harry", "Ruth", "Ruth"), to=c("Harry", "Billy", "Ruth", "Harry", "Billy"))

g <- graph_from_data_frame(relations, directed=TRUE, vertices=users)
print(g)
plot(g)