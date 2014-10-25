
#Installing Packages
install.packages("lubridate")
install.packages("randomForest")

#Declaring Libraries
library(lubridate)
library(randomForest)
library(stringr)
library(dplyr)
#Importing testing and training datasets
train_bike = read.csv("train.csv")
test_bike =read.csv("test.csv")

str(train_bike)
str(test_bike)


train_bike$dow = wday(train_bike$datetime) 
train_bike$hod = hour(train_bike$datetime)
test_bike$dow = wday(test_bike$datetime) 
test_bike$hod = hour(test_bike$datetime)

#Linear regresion testing
attach(train_bike)

model_lr_1 = lm(count~season+holiday+
              weather+temp+atemp+humidity+windspeed+casual+registered)

SSE_lr_1 = sum(model_lr_1$residuals^2)
summary(model_lr_1)

model_rf = randomForest(as.factor(count)~season+holiday+
                          weather+temp+atemp+humidity+windspeed+dow+hod)
detach(train_bike)

summary(model_rf)
varImpPlot(model_rf)

Prediction = predict(model_rf,test_bike)
submit = data.frame(datetime = test_bike$datetime,count=Prediction)

write.csv(submit,'results.csv',row.names=FALSE)

#Version 2

#Factorizing the coded variables for training dataset
train_factor = train_bike
train_factor$weather = factor(train_bike$weather)
train_factor$holiday = factor(train_bike$holiday)
train_factor$season = factor(train_bike$season)
train_factor$workingday =factor(train_bike$workingday)

#Factorizing the coded variables for testing dataset
test_factor = test_bike
test_factor$weather = factor(test_bike$weather)
test_factor$holiday = factor(test_bike$holiday)
test_factor$season = factor(test_bike$season)
test_factor$workingday =factor(test_bike$workingday)

train_factor$time=factor(substring(train_bike$datetime,12,20))
test_factor$time=factor(substring(test_bike$datetime,12,20))

str(train_factor)
train_factor$day = factor(weekdays(as.Date(train_factor$datetime)))
test_factor$day = factor(weekdays(as.Date(test_factor$datetime)))

aggregate(x=train_factor$count,by = list(train_factor$day),FUN = mean)

#Making a variable Sunday as bikes are least rented on that day
train_factor$sunday[train_factor$day=="Sunday"] = "1"
train_factor$sunday[(train_factor$sunday!="1")]="0"
table(train_factor$sunday)
train_factor$sunday = factor(train_factor$sunday)

test_factor$sunday[test_factor$day == "Sunday"] = "1"
test_factor$sunday[test_factor$day != "Sunday"] = "0"
table(test_factor$sunday)
test_factor$sunday = factor(test_factor$sunday)



#Adding the hour variable
train_factor$hour = as.numeric(factor(substring(train_factor$time,1,2)))
test_factor$hour = as.numeric(factor(substring(test_factor$time,1,2)))

unique(train_factor$hour)

time_bucket = c(0,3,11,15,21,23)
train_factor$daypart = findInterval(train_factor$hour,time_bucket,rightmost.closed = TRUE)
test_factor$daypart = findInterval(test_factor$hour,time_bucket,rightmost.closed = TRUE)



#Using the party package
install.packages("party")
library(party)
?ctree

formula = count ~ season + holiday + workingday + weather + temp + atemp + humidity + hour + daypart + sunday
fit.ctree = ctree(formula,data = train_factor)
summary(fit.ctree)


#Run the above model against a test dataset
predict.ctree = predict(fit.ctree,test_factor)
submit.ctree = data.frame(datetime = test_bike$datetime,
                          count=predict.ctree)
write.csv(submit.ctree,'results_party.csv',row.names=FALSE)







