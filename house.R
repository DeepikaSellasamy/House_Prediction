#Reading the data
setwd("C:/Users/DEEPIKA/Downloads")
house=read.csv("kc_house_data.csv",na.strings = c(""))

#loading the packages
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caretEnsemble)
library(doParallel)
library(corrplot)
library(dplyr)
library(e1071)
library(mlr)
library(caTools)
library(tidyr)
library(MLmetrics)
library(plot3D)
library(scatterplot3d)
library(lubridate)

#viewing the data
summary(house)
str(house)

#checking missing values
table(is.na(house))
is.na(house)
table(is.na(house$view))
sum(is.na(house))

#checking null values
table(is.null(house))
is.null(house)
sum(is.null(house))

#filling missing values with mean
house$sqft_above[is.na(house$sqft_above)] <- mean(house$sqft_above,na.rm=T)

#checking outliers
summary(house$bedrooms)
bx=boxplot(house$bedrooms)

summary(house$price)
bx=boxplot(house$price)

#converting variable for year of renovated and sqft_basement
house$yr_renovated=ifelse(house$yr_renovated==0,0,1)
house$yr_renovated=as.factor(house$yr_renovated)

house$sqft_basement=ifelse(house$sqft_basement==0,0,1)
house$sqft_basement=as.factor(house$sqft_basement)

#visualizing some plots for understanding
hist(house$price,main='Distribution of price',
     xlab='price',
     ylab='frequency',
     col='pink',
     bins=5)

#distribution of bedrooms
#no.of bedrooms ranges from 3,4 is higher than other
ggplot(house,aes(bedrooms))+geom_histogram(stat = "count")

#variation of sqft_living
ggplot(house,aes(sqft_living))+geom_histogram(stat = "count")

#variation of sqft_above
ggplot(house,aes(sqft_above))+geom_histogram(stat = "count")

#price by bedrooms
#prices are higher in the range for bedroom 3,4 compare to other
ggplot(house,aes(bedrooms,price))+geom_bar(stat="identity")

hist(house$condition,main='Distribution of condition',
     xlab='House condition',
     ylab='frequency',
     col='pink')

hist(house$yr_built,main='Distribution of Houses by year',
     xlab='year',
     ylab='frequency',
     col='pink')

#Price by sqft
plot(y=house$price,x =house$sqft_lot15,xlab='sq feet',ylab='price',col='red')

plot(y=house$price,x =house$floors,xlab='floors',ylab='price',col='red')

plot(y=house$price,x =house$view,xlab='view',ylab='price',col='red')

#price by condition of the house
ggplot(house,aes(condition,price))+geom_bar(stat="identity")

#price by sqft
ggplot(house,aes(sqft_living,price))+geom_bar(stat="identity")
ggplot(house,aes(sqft_above,price))+geom_bar(stat="identity")
ggplot(house,aes(sqft_basement,price))+geom_bar(stat="identity")

#Grade with price range
ggplot(house,aes(grade,price))+geom_bar(stat="identity")

#price with renovated houses
ggplot(house,aes(factor(yr_renovated),price))+geom_bar(stat="identity")

#price with zipcode
ggplot(house,aes(zipcode,price))+geom_bar(stat="identity")

#House condition and Grade
#avg house condition has the highest grade
ggplot(house,aes(condition,grade))+geom_point()

#price of the house by bathrooms
ggplot(house,aes(bathrooms,price))+geom_bar(stat="identity")
ggplot(house,aes(bathrooms,price,fill=as.factor(bedrooms)))+geom_bar(stat="identity",position="dodge")

#plotting for bedrooms,bathrooms,price
x <- house$bedrooms
y <- house$bathrooms
z <-house$price

scatter3D(x, y, z, pch = 18,  theta = 20, phi = 20,
          main = "house", xlab = "bedrooms",
          ylab ="bathrooms", zlab = "price")

scatterplot3d(house$condition, house$grade, house$price,
              main = "3D Scatterplot")

ggplot(gather(house %>% select_if(is.numeric)), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#EDA
house %>% 
  select(bedrooms,bathrooms,price) %>% filter(bedrooms=='2',bathrooms=='2') %>% summarise(avgprice=mean(price))

#If the condition of the house greater than or equal to 3 then the price of the house also high
house %>% 
  select(bedrooms,bathrooms,price) %>% filter(bedrooms=='3',condition<2) %>% summarise(avgprice=mean(price))

house %>% 
  select(bedrooms,bathrooms,price) %>% filter(bedrooms=='3',condition>=3) %>% summarise(avgprice=mean(price))

#higher grade leads to the price of the house high
house %>% 
  select(bedrooms,bathrooms,price) %>% filter(grade<7) %>% summarise(avgprice=mean(price))

house %>% 
  select(bedrooms,bathrooms,price) %>% filter(grade>=7) %>% summarise(avgprice=mean(price))

house %>% 
  select(bedrooms,bathrooms,price) %>% filter(grade=='11') %>% summarise(avgprice=mean(price))

#price range with waterfront
house %>% 
  select(bedrooms,bathrooms,price) %>% filter(waterfront=='1') %>% summarise(avgprice=mean(price))

house %>% 
  select(bedrooms,bathrooms,price) %>% filter(waterfront=='0') %>% summarise(avgprice=mean(price))

#price range with sqft_living
house %>% 
  select(bedrooms,bathrooms,price,sqft_living) %>% filter(sqft_living<=1000) %>% summarise(avgprice=mean(price))

house %>% 
  select(bedrooms,bathrooms,price,sqft_living) %>% filter(sqft_living>=1800) %>% summarise(avgprice=mean(price))

#price variation with sqft_above
house %>% 
  group_by(condition) %>% filter(sqft_above<=1750) %>% summarise(avgprice=mean(price))

house %>% 
  group_by(condition,grade) %>% filter(sqft_above>=1750) %>% summarise(avgprice=mean(price))

#checking correlation between variables
#price is positively correlated with bedroom, bathroom, Sqft_living, view , grade, sqft_above, sqft_basement, lat, sqft_living 15
#sqft_lot, sqft_lot15 and yr_built seem to be poorly related to price..
cor_data=data.frame(house[,3:21])
correlation=cor(cor_data)
par(mfrow=c(1, 1))
corrplot(correlation,method="color")

#removing unwanted variables
house= house %>%
  select(-c("id","lat","long","date"))
summary(house)

#Splitting data
ran = createDataPartition(house$price, 
                          p = 0.7,                         
                          list = FALSE)

ran
house_train=house[ran,]
house_test=house[-ran,]

X = house[, -1]
y = house[, 1]

Xtrain = X[ran, ]
Xtest = X[-ran, ]
ytrain = y[ran]
ytest = y[-ran]

#linear model 
model=lm(data=house_train,price~bedrooms+sqft_living+view+sqft_above+sqft_lot+yr_built+yr_renovated)
summary(model)

#prediction on test data
predict_model=predict(model,house_test)
predict_model

#accuracy checking
model_test=mean(abs(house_test$price-predict_model)/house_test$price)
accuracy_test=1-model_test
accuracy_test

#decision tree model
tree <- rpart(price ~ bedrooms+bathrooms+sqft_living+floors+waterfront+view+condition+grade+sqft_above+sqft_basement+zipcode,house_train)
rpart.plot(tree)

#prediction on test data
predict_tree=predict(tree,house_test)
predict_tree

#accuracy checking
model_test=mean(abs(house_test$price-predict_tree)/house_test$price)
accuracy_test=1-model_test
accuracy_test

#cross validation
fitControl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5)

#KNN model
knnFit <- caret::train(price ~ ., house_train, method = "knn", trcontrol=fitControl,preProcess = c("center","scale"))

#prediction on test data
predict_knn=predict(knnFit,house_test)
predict_knn

#accuracy checking
model_test=mean(abs(house_test$price-predict_knn)/house_test$price)
accuracy_test=1-model_test
accuracy_test

#random forest model
forest <- randomForest(ytrain~.,data=Xtrain,mtry=6,ntree=20,keep.forest=TRUE)

#prediction on test data
predict_forest=predict(forest1,Xtest)
predict_forest

#accuracy checking
model_test=mean(abs(ytest-predict_forest)/ytest)
accuracy_test=1-model_test
accuracy_test

#error rate
err= sqrt(mean((house_test$price-predict_forest)^2))
err

