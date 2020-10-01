# Title: Prediction of the price of second hand cars
# Goal of this project: The aim of this project was to build a model that can predict 
# the price of the cars based on certain parameters as precisely as possible.
# Authors: Bodo Gruetter, Malik Sogukoglu

# Source of dataset: https://www.kaggle.com/orgesleka/used-cars-database
# Description of variables in the dataset:
# - dateCrawled: Date when data record was crawled
# - name: name of the car
# - seller: information whether seller was "gewerblich"(commercial) or "privat"(private)
# - offerType: information if car was searched ("Gesuch") or offered ("Angebot") 
# - price: Price of car
# - abtest: information if auto has been tested ("test") or the test is still pending ("control").
# - vehicleType: information about the class of the car
# - yearOfRegistration: car's year of registration
# - gearbox: information whether the gearbox is automatic or manual.
# - powerPS: horsepower of the car
# - model: model of the car
# - kilometer: kilometers covered by the car
# - monthOfRegistration: month of registration of the car
# - fuelType: information whether fuel type is "diesel" or "benzin".
# - brand: brand of the car
# - notRepairedDamage: information whether car has not repaired damage or not
# - dateCreated; information when data was crawled
# - nrOfPictures: unknown
# - postalCode: information where car is at the moment
# - lastSeen: information when car was last seen.
   

### Import packages

library(tidyverse)
library(corrplot)
library("ggplot2")
library(mgcv)
library(igraph)
library(igraphdata)


### Prepare Data

# CSV is read whereby zero values are explicitly marked as NAs
df.cars <- read.csv2(file="autos.csv", sep = ",", header = T, na.strings = c("", " ", "NA"))
# remove NA


df.cars <- na.omit(df.cars)
df.cars

# To predict price of cars "kilometres" and "yearOfRegistration" could be good independent variables.

# With the following plots it is shown how "kilometres" resp. "yearOfRegistration" stand to the variable "price".


plot(df.cars$price ~df.cars$kilometer)

plot(df.cars$price ~df.cars$yearOfRegistration)


# The unrealistic "price" and "yearOfRegistration" data in the plots above clearly show 
# that there are outliers which should be filtered. These unrealistic values are probably caused by failures during crawling.


# filter outliers:
# to get rid of these unrealistic values, only cars with a 
# value up to 100000 Euros and only the models between 2010 and 2016 are considered 

df.cars <- df.cars %>% filter(yearOfRegistration > 2010, 
                              yearOfRegistration < 2016, 
                              price < 100000)

# the following example-boxplot shows that by filtering to the years 2010 - 2016 the graphic became much more visible
boxplot(df.cars$price ~df.cars$yearOfRegistration)

# columns lastSeen,nrOfPictures,dateCreated,dateCrawled are removed since they obviously do not provide useful information
df.cars = subset(df.cars, select = -c(lastSeen,nrOfPictures,dateCreated,dateCrawled) )

# the "age" of the car would be an important value to calculate the price 
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



# correlation analysis: shows which variables influence price
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
# have a certain influence on the price. 
# So, there are also variables that we did not think as having a strong effect previously.


# In the following analysis we will take a closer look at
# the effects of the variables that are influential according to the correlation plot.


# ...lets start with the categorical values
# categorical variables are best explaint with a boxplot and anova-test. 

# see which are the categorical
sapply(df.cars, class)

# the independent categorical variables to be considered are
# - yearOfRegistration
# - name
# - brand
# - fuel type
# - gearbox
# - vehicle Type
# - notRepairedDamage
# - postalCode
# - age

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
# has smaller residuals than the dummy model (see RSS).


## name
# because name has many levels it is not suitable for plotting and anova test



## brand

ggplot(data = df.cars, aes(group=brand, y = price, x = brand)) +
   geom_boxplot() + xlab("brand") + ylab("price")


lm.brand <- lm(price ~ brand, data = df.cars)
anova(lm.cars, lm.brand)
# the boxplot above clearly shows that the brand influences the price. 
# the anova test shows that there is a strong evidence that at least one brand influences the price (see p-value) .
# the anova test shows also that the model only having brand as independent variable has smaller residuals than the dummy model (see RSS).


## fuel type
ggplot(data = df.cars, aes(group = fuelType, y = price, x = fuelType)) +
   geom_boxplot() + xlab("fuel type") + ylab("price")

lm.fuelType <- lm(price ~ fuelType, data = df.cars)
anova(lm.cars, lm.fuelType)
# the boxplot above certainly shows that the fuel type influences the price. 
# the anova test shows that there is a strong evidence that at least one fuel type-level influences the price (see p-value) .
# the anova test shows also that the model only having fuelType as independent variable has smaller residuals than the dummy model (see RSS).


## gearbox
ggplot(data = df.cars, aes(group = gearbox, y = price, x = gearbox)) +
   geom_boxplot() + xlab("gearbox") + ylab("price")

lm.gearbox <- lm(price ~ gearbox, data = df.cars)
anova(lm.cars, lm.gearbox)
# the boxplot above clearly shows that gearbox influences the price. 
# the anova test shows that there is a strong evidence that at least one gearbox-type influences the price (see p-value) .
# the anova test shows also that the model only having gearbox as independent variable has smaller residuals than the dummy model (see RSS).


## vehicleType
ggplot(data = df.cars, aes(y = price, x = vehicleType)) +
   geom_boxplot() + xlab("vehicle type") + ylab("price")

lm.vehicleType <- lm(price ~ vehicleType, data = df.cars)
anova(lm.cars, lm.vehicleType)
# the boxplot above clearly shows that the vehicle type influences the price. 
# the anova test shows that there is a strong evidence that at least one vehicle-type influences the price (see p-value) .
# the anova test shows also that the model only having vehicleType as independent variable has smaller residuals than the dummy model (see RSS).


## notRepairedDamage

ggplot(data = df.cars, aes(y = price, x = notRepairedDamage)) +
   geom_boxplot() + xlab("not repaired damage") + ylab("price")

lm.notRepairedDamage <- lm(price ~ notRepairedDamage, data = df.cars)
anova(lm.cars, lm.notRepairedDamage)
# the boxplot above clearly shows that the the information whether car has a non-repaired demage or not influences the price. 
# the anova test shows that there is a strong evidence that this information influences the price (see p-value) .
# the anova test shows also that the model only having this information as independent variable has smaller residuals than the dummy model (see RSS).

## postalCode

# because postalCode has many levels it is not suitable for plotting and anova test

 ## age
ggplot(data = df.cars, aes(y = price, x = as.factor(age))) +
   geom_boxplot() + xlab("age") + ylab("price")

lm.age <- lm(price ~ as.factor(age), data = df.cars)
anova(lm.cars, lm.age)
# the above plot and model (see p-value) show that age has a linear influence on price. 


## lets go on with the numeric variables which are the following:
# - powerPS
# - kilometer
# numeric variables are best explained with a classical plot

## powerPS

ggplot(data = df.cars, mapping = aes(y = price, x = powerPS)) + geom_point(alpha = 0.4) +
   geom_point()  + xlab("PS") +ylab("price") +geom_smooth(method = "gam")

# there is little to recognise. therefore the x axis is logaritised in the following plot:

ggplot(data = df.cars, mapping = aes(y = price, x = powerPS)) + geom_point(alpha = 0.4) +
   geom_point()  + xlab("PS") +ylab("price") + scale_x_log10() +geom_smooth(method = "gam")

# In the plot above, one can see that "powerPS" does not show a linear but a quadratic relationship to the "price"
# ....following GAM-model confirms that where the p-value us significant: 
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




## Modelling
##########Bodo
##interaction effects: analyses regarding possible interactions between a independent categorical variables and a numerical variable

# based on information from the correlation plot above, there could be interactions between the following variables:
# - factor(yearOfRegistration) : kilometer
# - kilometer : fuelType
# - kilometer : gearbox

# example factor(yearOfRegistration) : kilometer
qplot(y = price, x = kilometer, data = df.cars, facets = ~ as.factor(yearOfRegistration)) + geom_smooth()
# example kilometer : fuelType
qplot(y = price, x = kilometer, data = df.cars, facets = ~ fuelType) + geom_smooth()
# In both graphs above, a clear interaction is to be recognized, 
# since between the categories clear differences are to be determined.

# kilometer : gearbox
qplot(y = price, x = kilometer, data = df.cars, facets = ~ gearbox) + geom_smooth() 
# In the above graph is a slight interaction effect between gearbox and kilometer.

## linear model to see interactions
### linear model for interaction factor(yearOfRegistration) : kilometer
lm.cars.interaction.1 <- lm(df.cars$price ~ df.cars$kilometer * as.factor(df.cars$yearOfRegistration))
summary(lm.cars.interaction.1)
# The linear model above confirms a strong interaction between kilometers and the year 2014

### linear model for interaction fuelType : kilometer
lm.cars.interaction.2 <- lm(df.cars$price ~ df.cars$kilometer * df.cars$fuelType)
summary(lm.cars.interaction.2)
# Between kilometer and fuelType there is actually no interaction according to the linear model above.

### linear model for interaction gearbox : kilometer
lm.cars.interaction.3 <- lm(df.cars$price ~ df.cars$kilometer * df.cars$gearbox)
summary(lm.cars.interaction.3)
# According to the linear model just above, between kilometer and gearbox there is actually a strong interaction.


# further interaction analyses:

# age : factor(yearOfRegistration)
qplot(y = price, x = age, data = df.cars, facets = ~ as.factor(yearOfRegistration)) + geom_smooth()

# age : gearbox
qplot(y = price, x = age, data = df.cars, facets = ~ gearbox) + geom_smooth()


##Different models
complex.model.1 <- price ~ as.factor(yearOfRegistration) + brand + fuelType + vehicleType +
   s(powerPS) + gearbox * kilometer + as.factor(age)

starting.model.1 <- price ~ as.factor(yearOfRegistration) + fuelType + gearbox + vehicleType +
   s(powerPS) + kilometer + as.factor(age)


##Modelling of the starting model
# yearOfRegistration is not considered anymore because it correlates 1:1 with age

starting.model.1 <- price ~ brand + fuelType + gearbox + vehicleType +
   s(powerPS) + kilometer + as.factor(age)

gam.starting.model.1 <- gam(starting.model.1, data = df.cars)

summary(gam.starting.model.1)

gam.starting.model.2 <- update(gam.starting.model.1, . ~ . - fuelType)

summary(gam.starting.model.2)$r.squared

gam.starting.model.3 <- update(gam.starting.model.2, . ~ . - vehicleType)

summary(gam.starting.model.3)

gam.starting.model.4 <- update(gam.starting.model.3, . ~ . - brand)

summary(gam.starting.model.4)


starting.model <- price ~ gearbox +
   s(powerPS) + kilometer + as.factor(age)

##Modelling of the more complex model
complex.model <- price ~ 
   s(powerPS) + gearbox * kilometer + as.factor(age)

gam.complex.model.1 <- gam(complex.model, data = df.cars)

summary(gam.complex.model.1)

hist()

##Model comparison
for(i in 1:10){
   df.cars.train.id <- sample(seq_len(nrow(df.cars)),size = floor(0.75*nrow(df.cars)))
   df.cars.train <- df.cars[df.cars.train.id,]
   df.cars.test <- df.cars[-df.cars.train.id,]
   
   #predict data with starting model 1
   gam.starting.model.train <- gam(starting.model, data = df.cars.train)
   predicted.starting.model.test <- predict(gam.starting.model.train,
                                              newdata = df.cars.test)
   r.squared.starting.model <- cor(predicted.starting.model.test, df.cars.test$price)^2
   
   #predict data with starting model 2
   gam.complex.model.train <- gam(complex.model, data = df.cars.train)
   predicted.complex.model.test <- predict(gam.complex.model.train,
                                           newdata = df.cars.test)
   r.squared.complex.model <- cor(predicted.complex.model.test, df.cars.test$price)^2
}

mean(r.squared.starting.model)
mean(r.squared.complex.model)

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
lm.cars.kilometer <- lm(df.cars$price ~df.cars$kilometer)

pred.int <- predict(lm.cars.kilometer, interval = "prediction")
mydata <- cbind(df.cars, pred.int)


p <- ggplot(mydata, aes(kilometer, price)) +
   geom_point() +
   stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")



lm.cars.age <- lm(df.cars$price ~df.cars$age)

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

##further example
data(package = "igraphdata")
data("enron")
plot(enron)
