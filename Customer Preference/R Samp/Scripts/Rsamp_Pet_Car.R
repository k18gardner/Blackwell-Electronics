
install.packages("caret", dependencies = T)
install.packages("generics")
install.packages("ggplot2")
install.packages("klaR")
install.packages("readr")
install.packages("shiny")
install.packages("arules")
install.packages("arulesViz")
install.packages("knitr")
install.packages("mlbench")
install.packages("devtools")
install.packages("randomForest")
install.packages('C50', dependencies = T)
install.packages("RMySQL")
install.packages("corrplot")
install.packages("inum")
install.packages("doParallel")
install.packages("bigrquery")
install.packages("ggthemes")
install.packages("xgboost")
install.packages("RWeka")
install.packages("mda")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("rtf")
install.packages("Formula", dependencies = c("Depends", "Suggests"))
install.packages("plotrix", dependencies = c("Depends", "Suggests"))
install.packages("plotmo", dependencies = c("Depends", "Suggests"))
install.packages("TeachingDemos", dependencies = c("Depends", "Suggests"))

library(lattice)
library(ggplot2)
library(c50)

rm(list = ls())
getwd()
setwd('J:\\My Drive\\Data Analytics\\UT\\Info\\Rfile')

#Loading packages needed
library(readr)

#Uploading a data and creating dataframes

cars1<-read.csv("J:\\My Drive\\Data Analytics\\UT\\Info\\Data\\cars.csv")

#List attributes
attributes(cars1)

#Print general stats of attributes
summary(cars1) 

#Display structure of data.
str(cars1) 

#View rows & columns
dim(cars1) 

#Names of attributes of data
names(cars1) 

#Rename variables
colnames(cars1) <- c("Name","Speed", "Distance")

#Checking for missing variables
is.na(cars1)

#Basic Visualizations
hist(cars1$Speed)
hist(cars1$Distance)
plot(cars1$Speed,cars1$Distance)
qqnorm(cars1$Speed)
qqnorm(cars1$Distance)

#set seed for same set of random numbers
set.seed(123)

#set size of datasets for split
ctrainSize<-round(nrow(cars1)*0.8)
ctestSize<-nrow(cars1)-ctrainSize

ctrainSize
ctestSize

#create train and test datasets
training_indicesc<-sample(seq_len(nrow(cars1)),size =ctrainSize)
ctrainSet<-cars1[training_indicesc,]
ctestSet<-cars1[-training_indicesc,]

#create linear regression model
Linnmod<-lm(Distance~ Speed, ctrainSet)
summary(Linnmod)

#prediction
Predc <- predict(Linnmod, ctestSet)
Predc


-------------------------------------------------------------------------------------------------------
iris1<-read.csv("J:\\My Drive\\Data Analytics\\UT\\Info\\Data\\iris.csv")

colnames(iris1) <- c("SepLen","SepWid", "PetLen","Petwid", "Spec")

attributes(iris)
summary(iris)
str(iris)
names(iris)

hist(iris$SepLen)
hist(iris$SepWid)
hist(iris$PetLen)
hist(iris$PetWid)

H <- c(50,50,50)
M <- c("setosa","versicolor","virginica")
barplot(H,names.arg=M,xlab="Species Type",ylab="Frequency",col="white",
        main="Flowers")
plot(iris$SepLen)
qqnorm(iris$SepLen)
qqnorm(iris$SepWid)
qqnorm(iris$PetWid)
qqnorm(iris$PetLen)

iris$Specs<- as.numeric(iris$Spec)

str(iris)
set.seed(123)


itrainSize<-round(nrow(iris)*0.8)
itestSize<-nrow(iris)-itrainSize

itrainSize
itestSize

training_indicesi<-sample(seq_len(nrow(iris)),size =itrainSize)
itrainSet<-iris[training_indicesi,]
itestSet<-iris[-training_indicesi,]

Linnmod<-lm(PetLen~ PetWid, itrainSet)
summary(Linnmod)
Predi <- predict(Linnmod, itestSet)
Predi


write.csv(correlations16v, file = "Correlation1.csv", row.names=FALSE)
pdf(file="corrplot2_existds.pdf")
corrplot(correlationsv7)
dev.off()

pdf(file="plot_gbmfitdum_Sales.pdf")
plot(gbmfitdum)
dev.off()
