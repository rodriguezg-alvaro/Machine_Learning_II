#################################################################################
############## TASK1-ASSESSMENT 1 ENSEMBLE METHODS   ############################
##############        MACHINE LEARNING II            ############################
##############           ESU   Feb-2022              ############################
#################################################################################

## Set working directory --------------------------------------------------------
getwd()

## Load libraries ---------------------------------------------------------------
library(boot)
library(caret)
library(ggplot2)
library(GGally)
library(MLTools)
library(rpart)
library(randomForest) 

## remove all epxisting variables--------------------------------------------------
rm(list=ls())


#################################################################################
# LOAD DATASETS
#################################################################################
#
frectALL = read.table("./Assignment_1/Data/SimDataRECTANGLEwithoutNoise.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
frectALL$Y = as.factor(frectALL$Y)
str(frectALL)
summary(frectALL)
ggplot(frectALL)+geom_point(aes(x=X1,y=X2,color=Y))


frectNoiseALL = read.table("./Assignment_1/Data/SimDataRECTANGLEwithNoise.dat", sep = "", header = TRUE, stringsAsFactors = FALSE)
frectNoiseALL$Y = as.factor(frectNoiseALL$Y)
str(frectNoiseALL)
summary(frectNoiseALL)
ggplot(frectNoiseALL)+geom_point(aes(x=X1,y=X2,color=Y))
## Exploratory analysis -------------------------------------------------------------------------------------
#Exploratory analysis for rectangle without noise
ggpairs(frectALL, aes(colour = Y, alpha = 0.4))

#Exploratory analysis for rectangle with noise
ggpairs(frectNoiseALL, aes(colour = Y, alpha = 0.4))


#################################################################################
# SELECT SUBSET OF DATA
#################################################################################
# set number of observations
N=1000

#sampling
indexN=sample(c(1:5000),N,replace = FALSE)
frectTR = frectALL[indexN,]
ggplot(frectTR)+geom_point(aes(x=X1,y=X2,color=Y))

indexN=sample(c(1:5000),N,replace = FALSE)
frectNoiseTR = frectNoiseALL[indexN,]
ggpairs(frectNoiseTR, aes(colour = Y, alpha = 0.4))
ggplot(frectNoiseTR)+geom_point(aes(x=X1,y=X2,color=Y))

#################################################################################
# TASK 1: BOOTSTRAP
#################################################################################
# One of the great advantages of the bootstrap approach is that it can be
# applied in almost all situations. No complicated mathematical calculations
# are required. Performing a bootstrap analysis in R entails only two steps.
# First, we must create a function that computes the statistic of interest.
# Second, we use the boot() function, which is part of the boot library, to
# boot() perform the bootstrap by repeatedly sampling observations from the data
# set with replacement.


bootComplexityAccuracyTree.fn=function (data, index){
  
  formula = Y ~ X4+X5
  cost = 0.001
  # fit with subset of data given by index !!!
  tree.fit <- rpart(formula, data=data[index,], method="class", control=rpart.control(minsplit=1, cp=cost) ) 
  #plot(tree.fit)
  
  # Predict with the data (all)
  yest = predict(tree.fit, type="class", newdata = data) # predict Y with all dataset
  
  cm <- confusionMatrix(data = yest,               # predict, all dataset
                        reference = data$Y,        # Real observations, all dataset
                        positive = "I")            # Class labeled as Positive
  
  accuracy <-  cm$overall[1] # get the accuracy from the confusion matrix ( ALL TR SET !!!!! )
  
  nleaves <- sum(tree.fit$frame$var=="<leaf>")  # number of leaves (an idea of the complexity)
  return (c(nleaves,accuracy))
}

#Check the function
bootComplexityAccuracyTree.fn(frectNoiseTR,1:100)

# Produce R bootstrap estimates for the mean and the variance of y
bb = boot(frectNoiseTR, bootComplexityAccuracyTree.fn,R=1000)
bb

#Histogramas, media de hojas y desviación del dataset con ruido
hist(bb$t[,1])
hist(bb$t[,2])
bb$t[,1]
mean(bb$t[,1])
sd(bb$t[,1])
mean(bb$t[,2])
sd(bb$t[,1])
#Histogramas, media de hojas y desviación del dataset sin ruido
bb2 = boot(frectTR, bootComplexityAccuracyTree.fn,R=1000)
bb2
hist(bb2$t[,1])
hist(bb2$t[,2])
mean(bb2$t[,1])
mean(bb2$t[,2])
var(bb$t[,1])
var(bb2$t[,1])


