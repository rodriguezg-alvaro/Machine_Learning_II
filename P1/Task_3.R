#################################################################################
##############  LabPractice 2.2 GENETIC ALGORITHMS     ##########################
##############        MACHINE LEARNING II              ##########################
##############           ESU   Jan-2022                ##########################
#################################################################################

## Set working directory --------------------------------------------------------

## Load libraries ---------------------------------------------------------------
library(caret)
library(ggplot2)

library(GA)  # install.packages("GA")
# help(package="GA")

## remove all epxisting variables--------------------------------------------------
rm(list=ls())

#-------------------------------------------------------------------------------------------------
#---- SET THE SYNTHETIC LINEAR REGRESSION PROBLEM    ---------------------------------------------
#-------------------------------------------------------------------------------------------------

fcreate <- function(coef1, coef2){function(x) coef1*x+coef2} 
f <- fcreate(1,5)
minVal = -20; maxVal = 20;
curve(f, minVal, maxVal)

x <- seq(minVal, maxVal, by = 0.1)
fval <- f(x)
noise <- runif(length(x),-3,3)
fnoise<-noise

freal<-fcreate(1,5)
yreal<- freal(x)+fnoise
data=data.frame(x,yreal)
# show the dataset and the theoretical line
p<- ggplot() + geom_point(aes(x=x, y=yreal)) + labs(title = "Problem: y = b1 * x + b0 ") + geom_abline(aes(intercept=5, slope=1, color="theoretical" ))
plot(p)

y_est=lm(yreal~x,data=data)
summary(y_est)
coef=y_est$coefficients
y_estim=coef[1]+coef[2]*x
rmse_lm=mean((yreal-y_estim)^2)
rmse_lm


p<- ggplot() + geom_point(aes(x=x, y=yreal)) + labs(title = "Problem: y = b1 * x + b0 ") + 
  geom_abline(aes(intercept=5, slope=1, color="theoretical" ))+
  geom_smooth(data=data,aes(x,yreal,color='regresión lineal'),se=FALSE)
plot(p)

#-------------------------------------------------------------------------------------------------
#---- fit LINEAR REGRESSION BY GA  ---------------------------------------------
#-------------------------------------------------------------------------------------------------

myfitness <- function(coef) {
  festimated<-fcreate(coef[1], coef[2])
  yestimated = festimated(x)
  -mean((yreal-yestimated)^2) # Negative (GA maximizes fitness)
}

myfitness(c(1,1))
myfitness(c(0,1))
myfitness(c(1,5))

ESUMonitor <- function(obj) 
{ 
  lines <- as.data.frame(obj@population)
  lines$ind = rownames(lines)
  p<- ggplot() + geom_point(aes(x=x, y=yreal)) + labs(title = paste("Iteration =", obj@iter)) + geom_abline(aes(intercept=lines[,2], slope=lines[,1], color="red" ))
  print(p)
  Sys.sleep(0.1)
}


# Run Basic GA using the monitor (see the Plots)
GA <- ga(type = "real-valued", fitness = myfitness, 
         lower = c(-3, -3), upper = c(8,8), 
         popSize = 100,   #  <-------  Number of individuals
         pcrossover = 0.8,
         pmutation = 0.1,
         maxiter = 100,         #  <----------------------  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         monitor = ESUMonitor,   #  <----------------------  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
         optim = TRUE
)
abline(v = GA@solution, lty = 3, col = 2)
plot(GA)
summary(GA)
print(GA@solution)


lines <- as.data.frame(GA@population)
lines$ind = rownames(lines)
p<-ggplot() + geom_point(aes(x=x, y=yreal))+ geom_abline(aes(intercept=lines[,2], slope=lines[,1], color="red" ))
print(p)

