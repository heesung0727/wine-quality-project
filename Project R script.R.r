winequality.white <-
  read.csv("C:/MATH3330/Project/R/winequality-white.csv", sep = ";")
View(winequality.white)

library(leaps)
library(MASS)
library(car)
library(qpcR)
library(Hmisc)
library(olsrr)
library(asbio)
library(QuantPsyc)


#model selection

x <- model.matrix(quality ~ . - 1, data = winequality.white)
y <- winequality.white$quality

bestmods <- leaps(x, y, nbest = 1)
print(bestmods)


modela <- lm(quality ~ alcohol, data=winequality.white)
modelb <- lm(quality ~ alcohol + volatile.acidity, data=winequality.white)
modelc <- lm(quality ~ alcohol + volatile.acidity + residual.sugar, data=winequality.white)
modeld <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + free.sulfur.dioxide, data=winequality.white)
modele <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH, data=winequality.white)
modelf <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates, data=winequality.white)
modelg <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide, data=winequality.white)
modelh <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity, data=winequality.white)
modeli <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide, data=winequality.white)
modelj <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide + chlorides, data=winequality.white)
modelk <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide + chlorides + citric.acid, data=winequality.white)

press(modela, as.R2 = FALSE)
press(modelb, as.R2 = FALSE)
press(modelc, as.R2 = FALSE)
press(modeld, as.R2 = FALSE)
press(modele, as.R2 = FALSE)
press(modelf, as.R2 = FALSE)
press(modelg, as.R2 = FALSE)
press(modelh, as.R2 = FALSE)
press(modeli, as.R2 = FALSE)
press(modelj, as.R2 = FALSE)
press(modelk, as.R2 = FALSE)

summary(modela)$adj.r.squared
summary(modelb)$adj.r.squared
summary(modelc)$adj.r.squared
summary(modeld)$adj.r.squared
summary(modele)$adj.r.squared
summary(modelf)$adj.r.squared
summary(modelg)$adj.r.squared
summary(modelh)$adj.r.squared
summary(modeli)$adj.r.squared
summary(modelj)$adj.r.squared
summary(modelk)$adj.r.squared


model8 <- modelh
model9 <- modeli

stres8 <- rstudent(model8)
qqnorm(stres8,
       xlab = "Externaly studentized residuals")
qqline(stres8)

stres9 <- rstudent(model9)
qqnorm(stres9,
       xlab = "Externaly studentized residuals")
qqline(stres9)

fv8 <- fitted.values(model8)

fv9 <- fitted.values(model9)

scatterplot(fv8, stres8, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

scatterplot(fv9, stres9, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

outlierTest(model8)
outlierTest(model9)

h8 <- hatvalues(model8)
h9 <- hatvalues(model9)


press8<- PRESS(model8)
press9<- PRESS(model9)

plot(h8)
plot(h9)


boxplot(h8, plot=FALSE)

plot(press8$residuals)
plot(press9$residuals)

boxplot(press8$residuals, plot = FALSE)


winequality.white <- winequality.white[-c(2782, 3308, 4746), ]

model8 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity, data=winequality.white)
model9 <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + density + pH + sulphates + free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide, data=winequality.white)


stres8 <- rstudent(model8)
qqnorm(stres8,
       xlab = "Externaly studentized residuals")
qqline(stres8)

stres9 <- rstudent(model9)
qqnorm(stres9,
       xlab = "Externaly studentized residuals")
qqline(stres9)

fv8 <- fitted.values(model8)

fv9 <- fitted.values(model9)

scatterplot(fv8, stres8, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

scatterplot(fv9, stres9, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

outlierTest(model8)
outlierTest(model9)

h8 <- hatvalues(model8)
h9 <- hatvalues(model9)


press8<- PRESS(model8)
press9<- PRESS(model9)

plot(h8)
plot(h9)


boxplot(h8, plot=FALSE)

plot(press8$residuals)
plot(press9$residuals)

boxplot(press8$residuals, plot = FALSE)

#multicollinearity


predictors8 <- winequality.white[c(1, 2, 4, 6, 8, 9, 10, 11)]
predictors8 <- matrix(unlist(predictors8), ncol = 8, nrow = 4893)

predictors9 <- winequality.white[c(1, 2, 4, 6, 7, 8, 9, 10, 11)]
predictors9 <- matrix(unlist(predictors9), ncol = 9, nrow = 4893)


rcorr(predictors8)
rcorr(predictors9)

vif(model8)
vif(model9)


ols_eigen_cindex(model8)
ols_eigen_cindex(model9)

model8b <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + pH + sulphates + free.sulfur.dioxide + fixed.acidity, data=winequality.white)
model9b <- lm(quality ~ alcohol + volatile.acidity + residual.sugar + pH + sulphates + free.sulfur.dioxide + fixed.acidity + total.sulfur.dioxide, data=winequality.white)

vif(model8b)
vif(model9b)


ols_eigen_cindex(model8b)
ols_eigen_cindex(model9b)

#checking of residuals after removal of density

stres8b <- rstudent(model8b)
fv8b <- fitted.values(model8b)

stres9b <- rstudent(model9b)
fv9b <- fitted.values(model9b)


qqnorm(stres8b,
       xlab = "Externaly studentized residuals")
qqline(stres8b)

scatterplot(fv8b, stres8b, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

qqnorm(stres9b,
       xlab = "Externaly studentized residuals")
qqline(stres9b)

scatterplot(fv9b, stres9b, 
            xlab = "Fitted values",
            ylab = "Externally studentized residuals")

mean(model8b$residuals)
mean(model9b$residuals)

#comparison between the two models

press(model8b, as.R2 = FALSE)
press(model9b, as.R2 = FALSE)

summary(model8b)$adj.r.squared
summary(model9b)$adj.r.squared

comparison <- anova(model8b, model9b)
comparison

#final model inspection

summary(model9b)
betas <- lm.beta(model9b)
betas

table(winequality.white$quality)
