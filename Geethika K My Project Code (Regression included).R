# Analysis of Airline Ticket Pricing
# NAME: GEETHIKA KALVAPALLI
# EMAIL: kalvapalli.geethika@gmail.com
# COLLEGE: BITS PILANI HYDERABAD CAMPUS

##READ THE DATA
hotels.df <- read.csv(paste("Cities42.csv",sep=""))
attach(hotels.df)

##SUMMARIZE THE DATA
library(psych)
describe(hotels.df)

##Y=F(x) where Y is dependent variable which is RoomRent in this dataset.
##3 most important independent variables i.e; x1,x2 and x3 are Weekend, Newyeareve and HotelCapacity

##Visualize Y,x1,x2 and x3
boxplot(RoomRent, main="Boxplot of RoomRent", xlab="RoomRent", horizontal = TRUE)
table(IsWeekend)
table(IsNewYearEve)
boxplot(HotelCapacity, main="Boxplot of HotelCapacity", xlab="HotelCapacity", horizontal = TRUE)

##Scatterplots of Y vs each independent variable
library(car)
scatterplot(IsWeekend, RoomRent, main="RoomRent vs Weekend", xlab="1 refers to weekend", ylab="RoomRent")
scatterplot(IsNewYearEve, RoomRent, main="RoomRent vs Newyeareve", xlab = "1 refers to Newyeareve", ylab = "RoomRent")
scatterplot(HotelCapacity, RoomRent, main="RoomRent vs HotelCapacity", xlab = "HotelCapacity", ylab = "RoomRent")

##Corrgram of Y,x1,x2 and x3
library(corrgram)
corrgram(hotels.df[,c(6,7,10,18)], order=TRUE,
         main="Hotel RoomRent Pricing",
         lower.panel=panel.pts, upper.panel=panel.pie,
         diag.panel=panel.minmax, text.panel=panel.txt)

##Variance-Covariance matrix for Y,x1,x2,x3
cov(hotels.df[,c(6,7,10,18)])

##Hypothesis
##H1:The prices of Indian Hotel rooms during newyeareve are higher than the prices at other times of the year.

##t test
aggregate(RoomRent ~ IsNewYearEve, data = hotels.df, FUN = mean)
log.roomrent <- log(RoomRent)
t.test(log.roomrent ~ IsNewYearEve, var.equal = TRUE)

##Models
##Model1:RoomRent=b0+b1*IsNewYearEve+e
##Model2:RoomRent=b0+b1*IsNewYearEve+b2*IsWeekend+b3*StarRating+e
##Model3:RoomRent=b0+b1*IsNewYearEve+b2*Cityname+b3*CityRank+b4*IsMetroCity+b5*IsTouristDestination+b6*IsWeekend+b7*Date+b8*StarRating+b9*Airport+b10*FreeWifi+b11*FreeBreakfast+b12*HotelCapacity+b13*HasSwimmingPool+b14*IsWeekend*IsNewyeareve+e
model1 <- lm(RoomRent ~ IsNewYearEve)
summary(model1)
model2 <- lm(RoomRent ~ IsNewYearEve + IsWeekend + StarRating)
summary(model2)
model3 <- lm(RoomRent ~ IsNewYearEve + CityName + CityRank + IsMetroCity + IsTouristDestination + IsWeekend + Date + StarRating + Airport + FreeWifi + FreeBreakfast + HotelCapacity + HasSwimmingPool + IsWeekend*IsNewYearEve)
summary(model3)

