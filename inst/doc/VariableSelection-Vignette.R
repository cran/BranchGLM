## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----fig.height = 4, fig.width = 6--------------------------------------------
# Loading BranchGLM package
library(BranchGLM)

# Fitting gamma regression model
cars <- mtcars

# Fitting gamma regression with inverse link
GammaFit <- BranchGLM(mpg ~ ., data = cars, family = "gamma", link = "inverse")

# Forward selection with mtcars
forwardVS <- VariableSelection(GammaFit, type = "forward")
forwardVS

## Getting final coefficients
coef(forwardVS, which = 1)

## Plotting path
plot(forwardVS)


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Backward elimination with mtcars
backwardVS <- VariableSelection(GammaFit, type = "backward")
backwardVS

## Getting final coefficients
coef(backwardVS, which = 1)

## Plotting path
plot(backwardVS)


## -----------------------------------------------------------------------------
# Branch and bound with mtcars
VS <- VariableSelection(GammaFit, type = "branch and bound", showprogress = FALSE)
VS

## Getting final coefficients
coef(VS, which = 1)


## -----------------------------------------------------------------------------
# Can also use a formula and data
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC")
formulaVS

## Getting final coefficients
coef(formulaVS, which = 1)


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Finding top 10 models
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC", 
                               bestmodels = 10)
formulaVS

## Plotting results
plot(formulaVS, type = "b")

## Getting all coefficients
coef(formulaVS, which = "all")


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Finding all models with an AIC within 2 of the best model
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC", 
                               cutoff = 2)
formulaVS

## Plotting results
plot(formulaVS, type = "b")


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Example of using keep
keepVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               keep = c("hp", "cyl"), metric = "AIC",
                               showprogress = FALSE, bestmodels = 10)
keepVS

## Getting summary and plotting results
plot(keepVS, type = "b")

## Getting coefficients for top 10 models
coef(keepVS, which = "all")


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Variable selection with grouped beta parameters for species
Data <- iris
VS <- VariableSelection(Sepal.Length ~ ., data = Data, family = "gaussian", 
                           link = "identity", metric = "AIC", bestmodels = 10, 
                           showprogress = FALSE)
VS

## Plotting results
plot(VS, cex.names = 0.75, type = "b")


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Treating categorical variable beta parameters separately
## This function automatically groups together parameters from a categorical variable
## to avoid this, you need to create the indicator variables yourself
x <- model.matrix(Sepal.Length ~ ., data = iris)
Sepal.Length <- iris$Sepal.Length
Data <- cbind.data.frame(Sepal.Length, x[, -1])
VSCat <- VariableSelection(Sepal.Length ~ ., data = Data, family = "gaussian", 
                           link = "identity", metric = "AIC", bestmodels = 10, 
                           showprogress = FALSE)
VSCat

## Plotting results
plot(VSCat, cex.names = 0.75, type = "b")


