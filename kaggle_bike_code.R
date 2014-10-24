
#Installing Packages
install.packages("lubridate")
install.packages("randomForest")

#Declaring Libraries
library(lubridate)
library(randomForest)

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

