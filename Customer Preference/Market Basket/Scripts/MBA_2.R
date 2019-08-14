library(plyr, lib.loc="J:\\My Drive\\Data Analytics\\R\\Hardware\\R-3.5.3\\library")
library(TSP)
library(prabclus)
library(fpc)
library(registry)
library(ggplot2)
library(arulesViz)
library(arules)
library(tidyverse)
library(knitr)
library(lubridate)
library(dplyr)
library(grid)
library(MASS)
library(mclust)
library(Matrix)
library(shiny)


etd0<- read.transactions("J:\\My Drive\\Data Analytics\\UT\\Info\\R\\Course2\\Task4\\Data\\ElectronidexTransactions2017.csv",
format = "basket", header = FALSE, sep=",", cols = NULL, rm.duplicates = TRUE, skip = 0, encoding = "unknown")

#Exlporation

#View the transactions
inspect(etd0)

#View the first to three transactions
inspect(etd0[1:3]) 

#Number of transactions
length(etd0) 

#Number of items per transaction
size(etd0) 

summary(etd0)

itemFrequency(etd0[ ,1])
itemFrequency(etd0[,1:10])

sort(itemFrequency(etd0, type = "absolute"), decreasing = T)

plot(density(size (etd0)), col = "turquoise", lwd = "3", main = "Number of items per transaction")

image(sample(etd0, 600)) 


etd1<-etd0[which(size(etd0)==1),]
itemFrequencyPlot(etd1 ,type = "absolute", topN = 10)
sort(itemFrequency(etd1, type = "absolute"), decreasing = T)

rule <- apriori(etd0, parameter = list(support = .05, confidence = .4)
inspect(rule)

rule <- apriori(etd0, parameter = list(support = .009, confidence = .6)
                inspect(rule)
                
rule <- apriori(etd0, parameter = list(support = .009, confidence = .5)
                                inspect(rule)
inspect(head(rule, 10))

head(quality(rule))
plot(rule, measure=c("support","lift"), shading="confidence");
plot(rule, shading="order", control=list(main ="Two-key plot"));

subrules = rule[quality(rule)$confidence > 0.5];
inspect(subrules)
plot(subrules, method="matrix", measure="lift");
plot(rule, method="grouped");

is.redundant(rule)

ItemRules <- subset(rule, items %in% "Hp Laptop")
ItemRules <- subset(rule, items %in% "Acer Aspire")
itemRules

plot(rule, "graph", engine = "interactive")
plot(rule, "graph", engine = "interactive", 10)
plot(rules_by_lift,10)
plot((rules_by_lift),10)
rule <- apriori(etd0, parameter = list(support = .009, confidence = .6))
plot(rule, "graph", engine = "interactive")
