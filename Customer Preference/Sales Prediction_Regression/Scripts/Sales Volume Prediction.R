

library(ggplot2)
library(lattice)
library(caret)
library(randomForest)
library(e1071)
library(mnormt)
library(DEoptimR)
library(doSNOW)
library(corrplot)
library(gbm)
library(xgboost)
library(bst)
library(kernlab)
library(promises)
library(MASS)
library(klaR)
library(datasets)
library(mlbench)
library(dplyr)
library(ggthemes)
library(rpart)
library(plyr, lib.loc="J:\\My Drive\\Data Analytics\\R\\Hardware\\R-3.5.3\\library")

rm(list = ls())
getwd()
setwd("J:\\My Drive\\Data Analytics\\UT\\Info\\R\\Course2\\Task3\\R_File\\Muli_Regre_Trial1_05_21\\previous")

newprods <- read.csv("J:\\My Drive\\Data Analytics\\R\\R Tutorial Data Sets\\Task2\\newproductattributes1.csv")
exstprods <- read.csv("J:\\My Drive\\Data Analytics\\R\\R Tutorial Data Sets\\Task2\\existingproductattributes1.csv")

#Rename  Variables
colnames(exstprods) <- c("ProductType","ProductNum", "Price","FiveStar","FourStar",
                         "ThreeStar","TwoStar","OneStar","PosRvw","NegRvw","WouldRecmnd",
                         "BestSell","ShipWeight","ProdDepth","ProdWidth","ProdHeight",
                         "ProfMarg","Volume")

colnames(newprods) <- c("ProductType","ProductNum", "Price","FiveStar","FourStar",
                        "ThreeStar","TwoStar","OneStar","PosRvw","NegRvw","WouldRecmnd",
                        "BestSell","ShipWeight","ProdDepth","ProdWidth","ProdHeight",
                        "ProfMarg","Volume")

dim(exstprods)
str(exstprods)
summary(exstprods)

dim(newprods)
str(newprods)
summary(newprods)

*Outlier check
boxplot(exstprods$Volume, main="Volume_Outliers")
boxplot(exstprods$Price, main="Price_Outliers")
boxplot(exstprods$FiveStar, main="FiveStar_Outliers")
boxplot(exstprods$FourStar, main="FourStar_Outliers")
boxplot(exstprods$ThreeStar, main="ThreeStar_Outliers")
boxplot(exstprods$TwoStar, main="TwoStar_Outliers")
boxplot(exstprods$OneStar, main="OneStar_Outliers")
boxplot(exstprods$PosRVw, main="PosRVw_Outliers")
boxplot(exstprods$NegVw, main="NegVw_Outliers")
boxplot(exstprods$WouldRecmnd, main="WouldRecmnd_Outliers")
boxplot(exstprods$ShipWeight , main="ShipWeight_Outliers")
boxplot(exstprods$ProdDepth , main="ProdDepth _Outliers")
boxplot(exstprods$ProdWidth , main="ShipWeight_Outliers")
boxplot(exstprods$ProdHeight , main="ProdDepth _Outliers")

outliers <-boxplot(exstprods$Volume, plot=FALSE)$out
exstprods[which(exstprods$Volume %in% outliers),]#
exstprods1 <- exstprods[-which(exstprods$Volume %in% outliers),]

dim(exstprods1)
dim(exstprods)

anyNA(exstprods1)	
is.na(exstprods1)

exstprods1$ProductNum <- NULL
exstprods1$BestSell <- NULL

newprods$ProductNum <- NULL
newprods$BestSell <- NULL

str(exstprods1)
summary(exstprods1)

str(newprods)
summary(newprods)

exstprods16v <- exstprods1
newprods16v <- newprods

correlations16v <- cor(exstprods16v[,c(2,3,4,5,6,7,8,9,10,12,13,14,15,16)])  

print(correlations16v)
write.csv(correlations16v, file = "Correlation1.csv", row.names=FALSE)

corrplot(correlations16v)

pdf(file="corrplot1_existds.pdf")
corrplot(correlations16v)
dev.off()

exstprods1$FiveStar <- NULL
exstprods1$OneStar <- NULL
exstprods1$ThreeStar <- NULL
exstprods1$ProdWidth <- NULL
exstprods1$ProdHeight <- NULL
exstprods1$ShipWeight <- NULL
exstprods1$Price <- NULL

newprods$FiveStar <- NULL
newprods$OneStar <- NULL
newprods$ThreeStar <- NULL
newprods$ProdWidth <- NULL
newprods$ProdHeight <- NULL
newprods$ShipWeight <- NULL
newprods$Price <- NULL

str(exstprods1)
str(newprods)

correlationsv7 <- cor(exstprods1[,c(2,3,4,5,6,7,8,9)])
print(correlationsv7) # evaluate correlations
write.csv(correlationsv7, file = "Correlation2.csv", row.names=FALSE)

corrplot(correlationsv7)

pdf(file="corrplot2_existds.pdf")
corrplot(correlationsv7)
dev.off()

str(exstprods1)

exstprodssamp <- exstprods1

existdummytest1 <- dummyVars(" ~ .", data = exstprodssamp)
existdummy <- data.frame(predict(existdummytest1, newdata = exstprodssamp))
str(existdummy)

existdummy$ProductType.Accessories <- NULL
existdummy$ProductType.Display  <- NULL
existdummy$ProductType.GameConsole  <- NULL
existdummy$ProductType.PrinterSupplies <- NULL
existdummy$ProductType.Printer  <- NULL
existdummy$ProductType.Software <- NULL
existdummy$ProductType.Tablet  <- NULL
existdummy$ProductType.ExtendedWarranty  <- NULL

str(existdummy)

newprodssamp <- newprods

newdummytest1 <- dummyVars(" ~ .", data = newprodssamp)
newdummy <- data.frame(predict(newdummytest1, newdata = newprodssamp))
str(newdummy)

newdummy$ProductType.Accessories <- NULL
newdummy$ProductType.Display  <- NULL
newdummy$ProductType.GameConsole  <- NULL
newdummy$ProductType.PrinterSupplies <- NULL
newdummy$ProductType.Printer  <- NULL
newdummy$ProductType.Software <- NULL
newdummy$ProductType.Tablet  <- NULL
newdummy$ProductType.ExtendedWarranty  <- NULL

str(newdummy)

exstprods12v <- exstprods1
newprods12v <- newprods

str(newprods12v)
str(exstprods12v)
str(existdummy)
str(newdummy)

data.frame(table(exstprods12v$ProductType))


# Train/test sets 


set.seed(185) 


inTrainingdum <- createDataPartition(existdummy$Volume, p=0.75, list=FALSE)
trainSetdum <- existdummy[inTrainingdum,]  
testSetdum <- existdummy[-inTrainingdum,]  

dim(trainSetdum)
dim(testSetdum)


# Train control 


ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
ctrl2 <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
ctrl3 <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
ctrl4<- trainControl(method = "repeatedcv", number = 3, repeats = 1)


# tunegrid parameters


rfGrid <- expand.grid(mtry=c(1,2,3,4,5,6,7,10))
svmGrid <- expand.grid(cost=c(0.01, 0.02, 0.10, 0.25, 0.5, 1, 1.5, 2))
gbmGrid <- expand.grid(interaction.depth=c(1, 7, 9, 11), n.trees = c( 25, 50, 100), shrinkage = c( 0.1), n.minobsinnode = c(1,5,7,10))


# Train and test model on train and test/validate ds


library(randomForest)
#rf

set.seed(185)

system.time(rffitdum <- train(
  Volume ~.,          
  data = trainSetdum,           
  method = "rf",        
  trControl=ctrl3,
  tuneGrid=rfGrid,
  preprocess = c("center","scale"), 
  importance = TRUE
))

rffitdum 
saveRDS(rffitdum, "rffitdum_Sales.rds")  
predictors(rffitdum)

plot(rffitdum)
ggplot(rffitdum)

pdf(file="plot_rffitdum_Sales.pdf")
plot(rffitdum)
dev.off()

summary(rffitdum)

varImp(rffitdum) 
rffitvarimpdum <- varImp(rffitdum) 
print(rffitvarimpdum)

pdf(file="plotvarimpdum_rffit_Sales.pdf")
plot(rffitvarimpdum)
dev.off()

#gbm

system.time(gbmfit <- train(
  Volume ~.,          
  data = trainSet,           
  method = "gbm",  
  tuneGrid=gbmGrid,
  preprocess = c("center","scale"), 
  trControl=ctrl3
  
))

library(gbm)

library(dplyr)

set.seed(185)
#gbm

system.time(gbmfitdum <- train(
  Volume ~.,          
  data = trainSetdum,           
  method = "gbm",     
  tuneGrid=gbmGrid,
  trControl=ctrl3
  
))

gbmvar<- varImp(gbmfitdum)
gbmvar

summary(gbmfitdum)

gbmfitdum

saveRDS(gbmfitdum , "printgbmfitdum_Sales.rds")    

plot(gbmfitdum)

pdf(file="plot_gbmfitdum_Sales.pdf")
plot(gbmfitdum)
dev.off()

pdf(file="plotvarimpdum_gbm_Sales.pdf")
plot(gbmvar)
dev.off()

summary(gbmfitdum1)

gbmdplot <- gbm::relative.influence(gbmfitdum1$finalModel, n.trees = 50, scale = TRUE) %>% sort(decreasing = TRUE) %>% data.frame

pdf(file="plotvarimpdum_gbmfit_Sales.pdf")
plot()
dev.off()

library(e1071)
#svm

set.seed(185)
#svm
system.time(svmfitdum <- train(
  Volume ~.,          
  data = trainSetdum,           
  method = "svmLinear2",  
  tuneGrid=svmGrid,
  preprocess = c("center","scale"), 
  trControl=ctrl3,
  
))

svmfitdum 
saveRDS(svmfitdum, "printsvmfitdum_Sales.rds")    

pdf(file="plot_svmfitdum_Sales.pdf")
plot(svmfitdum)
dev.off()

summary(svmfitdum)

varImp(svmfitdum)
svmfitvarimpdum1<- varImp(svmfitdum) 
print(svmfitvarimpdum1)

pdf(file="plotvarimpdum_svmfit_Sales.pdf")
plot(svmfitvarimpdum1)
dev.off()

rffitdumpred2test <- predict(rffitdum, testSetdum)
rffitdumpred2test
rffitdumPerformance <- postResample(rffitdumpred2test, testSetdum$Volume)
rffitdumPerformance
plot(rffitdumpred2test, testSetdum$Volume)

pdf(file="rffitdumPerformance.pdf")
plot(rffitdumpred2test, testSetdum$Volume)
dev.off()

gbmfitdumpred2test <- predict(gbmfitdum, testSetdum)
gbmfitdumpred2test
gbmfitdumPerformance <- postResample(gbmfitdumpred2test, testSetdum$Volume)
gbmfitdumPerformance
plot(gbmfitdumpred2test, testSetdum$Volume)

pdf(file="gbmfitdumPerformance.pdf")
plot(gbmfitdumpred2test, testSetdum$Volume)
dev.off()


###---
svmfitdum
svmfitpred2test <- predict(svmfit, testSet)
svmfitpred2test
svmfitPerformance <- postResample(svmfitpred2test, testSet$Volume)
svmfitPerformance
plot(svmfitpred2test, testSet$Volume)

svmfitdumpred2test <- predict(svmfitdum, testSetdum)
svmfitdumpred2test
svmfitdumPerformance <- postResample(svmfitdumpred2test, testSetdum$Volume)
svmfitdumPerformance
plot(svmfitdumpred2test, testSetdum$Volume)

pdf(file="svmfitdumPerformance.pdf")
plot(svmfitdumpred2test, testSetdum$Volume)
dev.off()

#train-newprods

output1 <- newdummy
output2 <- newdummy
output3 <- newdummy
output4 <- newdummy

str(output1)

rffitdumpred3dum <- predict(rffitdum, output1)
gbmfitdumpred3dum <- predict(gbmfitdum, output1)
svmfitdumpred3dum <- predict(svmfitdum, output1)

summary(rffitdumpred3dum)
summary(gbmfitdumpred3dum)
summary(svmfitdumpred3dum)

output1$rfdumVol <-rffitdumpred3dum
output1$gbmdumVol <-gbmfitdumpred3dum 
output1$svmdumVol <-svmfitdumpred3dum 

output1

write.csv(output1, file="output1.csv" , row.names =TRUE)

output2$Volumerf <- predict(rffitdum, output2)
volumeprerf <- predict(rffitdum, output2)
ggplot()+geom_bar(output2, aes(x=P))

output2$Volumegbm <- predict(gbmfitdum, output2)
volumepregbm <- predict(gbmfitdum, output2)
ggplot()+geom_bar(output2, aes(x=P))

output2$Volumesvm <- predict(svmfitdum, output2)
volumepresvm <- predict(svmfitdum, output2)
ggplot()+geom_bar(output2, aes(x=P))


PC = filter(output3, ProductTypePC == 1)
Laptop = filter(output3, ProductTypeLaptop == 1)
Netbook = filter(output3, ProductTypeNetbook == 1)
Smartphone = filter(output3, ProductTypeSmartphone == 1)

predict(rffitdum, PC)
predict(rffitdum, Laptop)
predict(rffitdum, Netbook)
predict(rffitdum, Smartphone)

predict(gbmfitdum, PC)
predict(gbmfitdum, Laptop)
predict(gbmfitdum, Netbook)
predict(gbmfitdum, Smartphone)

predict(svmfitdum, PC)
predict(svmfitdum, Laptop)
predict(svmfitdum, Netbook)
predict(svmfitdum, Smartphone)


