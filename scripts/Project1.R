rm(list = ls(all.names = TRUE))

library(dplyr)
library(corrplot)
library(tidyverse)
library(tidyr)
library(tidyselect)
library(plotly)
library(reactable)
library(htmlwidgets)
library(IRdisplay)
library(car)

dir.create("/Users/pablosalarcarrera/Desktop/OLD Classes/MSD325/")
setwd("/Users/pablosalarcarrera/Desktop/OLD Classes/MSD325/")

CarPrice = read.csv("NewCarPricePrediction.csv", header=T)

names(CarPrice)

dim(CarPrice)

which(is.na(CarPrice))
sum(is.na(CarPrice))


head(CarPrice)

car <- drop_na(CarPrice)

dim(car)
summary(car)

str(car)

boxplot(CarPrice$Mileage,
        ylab = "Mileage"
)

boxplot(CarPrice$Price,
        ylab = "Price"
)


plot(CarPrice$Mileage , CarPrice$Price )

lm.fit = lm(CarPrice$Price ~ CarPrice$Mileage, data = CarPrice)
summary(lm.fit)


plot(CarPrice$Cylinders, CarPrice$Price)
lm.fit2 = lm(CarPrice$Price ~ CarPrice$Cylinders, data = CarPrice)
summary(lm.fit2)

plot(CarPrice$Engine.volume, CarPrice$Price)
lm.fit3 = lm(CarPrice$Price ~ CarPrice$Engine.volume, data = CarPrice)
summary(lm.fit3)

attach(car) 

mydata.cor = cor(CarPrice)
corrplot(mydata.cor)

lm.fit4 = lm(Price~Prod..year+Manufacturer+Category+Mileage+Leather.interior+Engine.volume+Cylinders+Gear.box.type+Wheel)
summary(lm.fit4)

lm.fit5 = lm(Price~Prod..year+Manufacturer+Category+Mileage+Engine.volume+Cylinders+Gear.box.type+Wheel)
summary(lm.fit5)


lm.fit6 = lm(Price~Prod..year+Manufacturer+Category+Mileage+Engine.volume+Gear.box.type+Wheel)
summary(lm.fit6)

vif(lm.fit6)

lm.fit7 = lm(Price~Prod..year*Manufacturer+Category+Mileage+Engine.volume+Gear.box.type+Wheel)
summary(lm.fit7)

lm.fit8 = lm(Price~Prod..year*Mileage+Category+Manufacturer+Engine.volume+Gear.box.type+Wheel)
summary(lm.fit8)

lm.fit9 = lm(Price~Prod..year*Mileage+Category+Manufacturer+Engine.volume*Gear.box.type+Wheel)
summary(lm.fit9)

lm.fit10 = lm(Price~Prod..year+I(Prod..year^2)+Mileage+Category+Engine.volume+Gear.box.type+Wheel)
summary(lm.fit10)

lm.fit11 = lm(Price~poly(Prod..year,5)+Mileage+Category+Engine.volume*Gear.box.type+Wheel)
summary(lm.fit11)

anova(lm.fit6, lm.fit9, lm.fit11)

par(mfrow=c(2,2))
plot(lm.fit11)

