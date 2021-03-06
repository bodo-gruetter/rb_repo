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

#####Functions#####
##returns the rsquared for gam
##create function
getR2 <- function(gam){
   ##calculate R2
   R2 <- 1-((sum(residuals(gam)^2))/
               (sum((gam$y - mean(gam$y))^2)))
   ##Return R2
   return(paste("R-Squared: ", R2))
}

#####Import packages#####

library(tidyverse)
library(corrplot)
library("ggplot2")
library(mgcv)
library(igraph)
library(igraphdata)

#####Prepare Environment#####
# Set seed for reproducability
set.seed(123)
# clear environment
rm(list = ls())
# set plotting window to default
par(mfrow = c(1, 1))

### Prepare Data

# CSV is read whereby zero values are explicitly marked as NAs
df.cars <- read.csv2(file="autos.csv", sep = ",", header = T, na.strings = c("", " ", "NA"))
# remove NA
df.cars <- na.omit(df.cars)


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





#####Graphical Analysis#####




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

# Now that all necessary independent variables have been determined, it is important to ensure that the dependent variable "price" follows a normal distribution: 

hist(df.cars$price)

#...and it does not, so the values in this column are to be squered squared:

df.cars$price <- sqrt(df.cars$price)
hist(df.cars$price)

#...after squaring, price is now normally distributed and thus suitable for analysis.

hist(df.cars$price)
df.cars$price <- sqrt(df.cars$price)
dim(df.cars)

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
# the boxplot above clearly shows that the information whether car has a non-repaired demage or not influences the price. 
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

# ...there is little to recognise. therefore the x axis is logaritised in the following plot:

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
# ...following model should confirm that:
lm.kilometer <- lm(price ~ kilometer, data = df.cars)
summary(lm.kilometer)
# and indeed the p-value shows that "kilometer" has a linear effect on the price




#####Modelling#####
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
# In the above graph is a slight interaction effect between gearbox and kilometer 
# since there are no recognizable differences between the categories of gearbox

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
# According to the linear model just above, between kilometer and gearbox there is no interaction.


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
# storing our starting model based on the findings of the graphical analysis.
# yearOfRegistration is not considered anymore because it correlates 1:1 with age
starting.model.1 <- price ~ as.factor(age) + brand + fuelType + gearbox + vehicleType +
   s(powerPS) + kilometer

# gam fitting
gam.starting.model.1 <- gam(starting.model.1, data = df.cars)
summary(gam.starting.model.1)

# updating the starting model by omitting the fuelType
gam.starting.model.2 <- update(gam.starting.model.1, . ~ . - fuelType)
summary(gam.starting.model.2)$r.squared

# updating the starting model by omitting the vehicleType
gam.starting.model.3 <- update(gam.starting.model.2, . ~ . - vehicleType)
summary(gam.starting.model.3)

# updating the starting model by omitting the brand
gam.starting.model.4 <- update(gam.starting.model.3, . ~ . - brand)
summary(gam.starting.model.4)

# storing our developed starting model
starting.model <- price ~ as.factor(age) + gearbox +
   s(powerPS) + kilometer

##Modelling of our simple Model
# storing our simple model with one predictor
simple.model <- price ~ brand

#lm fitting
lm.simple.model <- lm(simple.model, data = df.cars)
summary(lm.simple.model)

##Modelling of the more complex model
#Storing our complex model
complex.model <- price ~ as.factor(age) + brand + fuelType + vehicleType +
   s(powerPS) + gearbox + kilometer

#gam fitting
gam.complex.model <- gam(complex.model, data = df.cars)
summary(gam.complex.model)





#####Model comparison#####
#drop levels that might be unused in train data set to prevent an error
df.cars <- droplevels(df.cars[!df.cars$brand == 'trabant',])
df.cars <- droplevels(df.cars[!df.cars$brand == 'daihatsu',])
df.cars <- droplevels(df.cars[!df.cars$brand == 'saab',])
df.cars <- droplevels(df.cars[!df.cars$fuelType == 'andere',])

# 10-fold CROSS VALIDATION
for(i in 1:10){
   # build a 75%-train and a 25%-test data set
   df.cars.train.id <- sample(seq_len(nrow(df.cars)),size = floor(0.75*nrow(df.cars)))
   df.cars.train <- df.cars[df.cars.train.id,]
   df.cars.test <- df.cars[-df.cars.train.id,]
   
   #train starting model as a gam
   gam.starting.model.train <- gam(starting.model, data = df.cars.train)
   #predict data with starting model
   predicted.starting.model.test <- predict(gam.starting.model.train,
                                            newdata = df.cars.test)
   #calculate the r squared of the starting model
   r.squared.starting.model <- cor(predicted.starting.model.test, df.cars.test$price)^2
   
   #train simple model as a lm
   lm.simple.model.train <- gam(simple.model, data = df.cars.train)
   #predict data with simple model
   predicted.simple.model.test <- predict(lm.simple.model.train,
                                          newdata = df.cars.test)
   #calculate the r squared of the simple model
   r.squared.simple.model <- cor(predicted.simple.model.test, df.cars.test$price)^2
   
   #train complex model as a gam
   gam.complex.model.train <- gam(complex.model, data = df.cars.train)
   #predict data with complex model
   predicted.complex.model.test <- predict(gam.complex.model.train,
                                           newdata = df.cars.test)
   #calculate the r squared of the complex model
   r.squared.complex.model <- cor(predicted.complex.model.test, df.cars.test$price)^2
}
#calculate the arithmetic mean of the r-squared values of the tree
#models for comparison
mean(r.squared.starting.model)
mean(r.squared.simple.model)
mean(r.squared.complex.model)







#####some further analyses#####
## check normal distribution of residuals of kilometer
qqnorm(df.cars$kilometer)
qqline(df.cars$kilometer)

# check normal distribtuion of kilometer
hist(df.cars$kilometer)

## calculate average price for cars of different classes
dfPrizeMean <- df.cars %>%
   group_by(vehicleType) %>%
   summarize(meanPrice = mean(price))
dfPrizeMean

## calculate average price for cars of different brands
dfPrizeMean <- df.cars %>%
   group_by(brand) %>%
   summarize(meanPrice = mean(price))
dfPrizeMean


# Colored plot: "Price based on Kilometers (per vehicle type)"
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

plot_cars




# Colored plot: "Price based on year of registration (per vehicle type)"

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


# prediction: kilometer predicts price
lm.cars.kilometer <- lm(df.cars$price ~df.cars$kilometer)
pred.int <- predict(lm.cars.kilometer, interval = "prediction")
mydata <- cbind(df.cars, pred.int)
p <- ggplot(mydata, aes(kilometer, price)) +
   geom_point() +
   stat_smooth(method = lm)
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")


# prediction: age predicts price
lm.cars.age <- lm(df.cars$price ~df.cars$age)
pred.int <- predict(lm.cars.age, interval = "prediction")
mydata <- cbind(df.cars, pred.int)
p <- ggplot(mydata, aes(age, price)) +
   geom_point() +
   stat_smooth(method = lm)
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
   geom_line(aes(y = upr), color = "red", linetype = "dashed")







######ipgraph#######
##A simple example
#create dataframe with entities
users <- data.frame(name=c("Billy", "Harry", "Ruth"))
#create dataframe with relationships between the entities
relations <- data.frame(from=c("Billy", "Harry", "Harry", "Ruth", "Ruth"), to=c("Harry", "Billy", "Ruth", "Harry", "Billy"))

#build a graph from a dataframe
g <- graph_from_data_frame(relations, directed=TRUE, vertices=users)
#print result
print(g)
#plot result
plot(g)

#build a graph with the same shape of our simple example, but directly
g.triangle <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F )
plot(g.triangle)

#build a star shaped graph
g.star <- graph_from_literal(a:b:c--d--e:f:g)
plot(g.star)

#build a two vertices graph with indirected edge
g.indirected <- graph_from_literal(a--b)
plot(g.indirected)

#build a two vertices graph with directed edge in one direction
g.directed <- graph_from_literal(a-+b)
plot(g.directed)

#build a two vertices graph with directed edge in both direction
g.symmetrical <- graph_from_literal(a++b)
plot(g.symmetrical)

#build a ring shaped graph
g.ring <- make_ring(10)
plot(g.ring)

#use predefined graphs
data(package = "igraphdata")
data("karate")
plot(karate)
