
#Load Packages
library(corrplot)
library(e1071)
library(randomForestSRC)
library(lattice)
library(ggplot2)
library(caret)
library(c50)
library(promises)
library(MASS)
library(klaR)
library(datasets)
library(mlbench)
library(dplyr)

rm(list = ls()) 
getwd()

#Import Data
complete <- read.csv("J:\\My Drive\\Data Analytics\\UT\\Info\Data\\CompleteResponse1.csv")
incomplete <- read.csv("J:\\My Drive\\Data Analytics\\UT\Info\Data\\SurveyIncomplete1.csv")

#view Data
dim(complete)
dim(incomplete)

str(complete)
str(incomplete)

attributes(complete)
attributes(incomplete)

summary(complete)
summary(incomplete)

names(complete)
names(incomplete)

#Examine Data
head(complete,5)
head(incomplete,5)

tail(complete,5)
tail(incomplete,5)

summary(complete)
summary(incomplete)


#Pre-process Data ( change numerical variables to factors)
complete$elevel <- as.factor(complete$elevel)
complete$car <- as.factor(complete$car)
complete$zipcode <- as.factor(complete$zipcode)
complete$brand <- as.factor(complete$brand)

incomplete$elevel <- as.factor(incomplete$elevel)
incomplete$car <- as.factor(incomplete$car)
incomplete$zipcode <- as.factor(incomplete$zipcode)
incomplete$brand <- as.factor(incomplete$brand)
summary(incomplete)

correlations  <- cor(complete[,c(1,6)])  
correlations1 <- cor(incomplete[,c(1,6)])  

print(correlations)
print(correlations1)

str(complete)
str(incomplete)

set.seed(107)

# Partition Data
inTrainb <- createDataPartition(
  y = complete$brand,
  p = .75,
  list = FALSE
)

#Create Test and Train Data
training <- complete[ inTrainb,]
testing  <- complete[-inTrainb,]

nrow(training)
nrow(testing)

#Train Control
ctrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 1)


#Train models (C50 RF)
c50fita <- train(
  brand ~ .,
  data = training,
  method = "C5.0",
  tuneLength =1,
  trControl = ctrl )

c50fita

summary(c50fita)

print(c50fita)
plot(c50fita)
varImp(object = c50fita)
plot(varImp(object=c50fita),main="C50fit_len1 - Variable Importance")

c50fitb <- train(
  brand ~ .,
  data = training,
  method = "C5.0",
  tuneLength =2,
  trControl =ctrl
)

summary(c50fitb)

print(c50fitb)
plot(c50fitb)
varImp(object = c50fitb)
plot(varImp(object=c50fitb),main="C50fit_len2 - Variable Importance")

rfGrid <- expand.grid(mtry=c(1,2,3,4,5))

system.time(rfFita <- train(brand ~.,
                            data = training, 
                            method = "rf", 
                            trControl = ctrl,
                            tuneGrid=rfGrid))
print(rfFita)
plot(rfFita)
varImp(object = rfFita)
plot(varImp(object=rfFita),main="rfFit - Variable Importance")

#train complete ds

predictors(c50fita)  
c50fitapred <- predict(c50fita, training)
plot(c50fitapred)
summary(c50fitapred)

comparison <- cbind(training$brand, c50fitapred)
colnames(comparison) <- c("actual","predicted")
head(comparison)
confusionMatrix(data=c50fitapred,training$brand)

predictors(c50fitb)
c50fitbpred <- predict(c50fitb, training)
plot(c50fitbpred)
summary(c50fitbpred)

comparison2 <- cbind(training$brand, c50fitbpred)
colnames(comparison2) <- c("actual","predicted")
head(comparison2)
confusionMatrix(data=c50fitbpred,training$brand)

predictors(rfFita)
rfFitapred <- predict(rfFita, training)
plot(rfFitapred)
summary(rfFitapred)

comparison3 <- cbind(training$brand, rfFitapred)
colnames(comparison3) <- c("actual","predicted")
head(comparison3)
confusionMatrix(data=rfFitapred,training$brand)

#test complete ds

predictors(c50fita)  
c50fitapred2test <- predict(c50fita, testing)
plot(c50fitapred2test)
summary(c50fitapred2test)

comparison <- cbind(testing$brand, c50fitapred2test)
colnames(comparison) <- c("actual","predicted")
head(comparison)
confusionMatrix(data=c50fitapred2test,testing$brand)

predictors(c50fitb)
c50fitbpred2test <- predict(c50fitb, testing)
plot(c50fitbpred2test)
summary(c50fitbpred2test)

comparison2 <- cbind(testing$brand, c50fitbpred2test)
colnames(comparison2) <- c("actual","predicted")
head(comparison2)
confusionMatrix(data=c50fitbpred2test,testing$brand)

predictors(rfFita)
rfFitapred2test <- predict(rfFita, testing)
plot(rfFitapred2test)
summary(rfFitapred2test)

comparison3 <- cbind(testing$brand, rfFitapred2test)
colnames(comparison3) <- c("actual","predicted")
head(comparison3)
confusionMatrix(data=rfFitapred2test,testing$brand)

#test incomplete ds

predictors(c50fita)  
c50fitapred3inc <- predict(c50fita, incomplete)
plot(c50fitapred3inc)


comparisonaa <- cbind(incomplete$brand, c50fitapred3inc)
colnames(comparisonaa) <- c("actual","predicted")
head(comparisonaa)
print(comparisonaa)

write.csv(comparisonaa, file = "c50fita.csv", row.names=FALSE)

confusionMatrix(data=c50fitapred3inc,incomplete$brand)

predictors(c50fitb)
c50fitbpred3inc <- predict(c50fitb, incomplete)
plot(c50fitbpred3inc)
summary(c50fitbpred3inc)

comparison2aa <- cbind(incomplete$brand, c50fitbpred3inc)
colnames(comparison2aa) <- c("actual","predicted")
head(comparison2aa)
confusionMatrix(data=c50fitbpred3inc,incomplete$brand)
print(comparison2aa)
print(c50fitbpred3inc)
summary(comparison2aa)


predictors(rfFita)
rfFitapred3inc <- predict(rfFita, incomplete)
plot(rfFitapred3inc)
summary(rfFitapred3inc)

comparison3aa <- cbind(incomplete$brand, rfFitapred3inc)
colnames(comparison3aa) <- c("actual","predicted")
head(comparison3aa)
confusionMatrix(data=rfFitapred3inc,incomplete$brand)


resamps <- resamples(list(rf = rfFita, c50 = c50fitb))
summary(resamps)
xyplot(resamps, what = "BlandAltman")

diffs <- diff(resamps)
summary(diffs)


  