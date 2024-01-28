## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Loading in BranchGLM
library(BranchGLM)

# Fitting gaussian regression models for mtcars dataset
cars <- mtcars

## Identity link
BranchGLM(mpg ~ ., data = cars, family = "gaussian", link = "identity")


## -----------------------------------------------------------------------------
# Fitting gamma regression models for mtcars dataset
## Inverse link
GammaFit <- BranchGLM(mpg ~ ., data = cars, family = "gamma", link = "inverse")
GammaFit

## Log link
GammaFit <- BranchGLM(mpg ~ ., data = cars, family = "gamma", link = "log")
GammaFit


## -----------------------------------------------------------------------------
# Fitting poisson regression models for warpbreaks dataset
warp <- warpbreaks

## Log link
BranchGLM(breaks ~ ., data = warp, family = "poisson", link = "log")


## -----------------------------------------------------------------------------
# Fitting binomial regression models for toothgrowth dataset
Data <- ToothGrowth

## Logit link
BranchGLM(supp ~ ., data = Data, family = "binomial", link = "logit")

## Probit link
BranchGLM(supp ~ ., data = Data, family = "binomial", link = "probit")


## -----------------------------------------------------------------------------
# Fitting logistic regression model for toothgrowth dataset
catFit <- BranchGLM(supp ~ ., data = Data, family = "binomial", link = "logit")

Table(catFit)


## -----------------------------------------------------------------------------
# Creating ROC curve
catROC <- ROC(catFit)

plot(catROC, main = "ROC Curve", col = "indianred")


## -----------------------------------------------------------------------------
# Getting Cindex/AUC
Cindex(catFit)

AUC(catFit)


## ----fig.width = 4, fig.height = 4--------------------------------------------
# Showing ROC plots for logit, probit, and cloglog
probitFit <- BranchGLM(supp ~ . ,data = Data, family = "binomial", 
                       link = "probit")

cloglogFit <- BranchGLM(supp ~ . ,data = Data, family = "binomial", 
                       link = "cloglog")

MultipleROCCurves(catROC, ROC(probitFit), ROC(cloglogFit), 
                  names = c("Logistic ROC", "Probit ROC", "Cloglog ROC"))


## -----------------------------------------------------------------------------

preds <- predict(catFit)

Table(preds, Data$supp)

AUC(preds, Data$supp)

ROC(preds, Data$supp) |> plot(main = "ROC Curve", col = "deepskyblue")


## -----------------------------------------------------------------------------
# Predict method
predict(GammaFit)

# Accessing coefficients matrix
GammaFit$coefficients


