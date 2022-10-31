## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Loading BranchGLM package
library(BranchGLM)

# Fitting gamma regression model
cars <- mtcars

# Fitting gamma regression with inverse link
GammaFit <- BranchGLM(mpg ~ ., data = cars, family = "gamma", link = "inverse")

# Forward selection with mtcars
forwardVS <- VariableSelection(GammaFit, type = "forward")
forwardVS

## Getting final model
fit(summary(forwardVS), which = 1)



## -----------------------------------------------------------------------------
# Backward elimination with mtcars
backwardVS <- VariableSelection(GammaFit, type = "backward")
backwardVS

## Getting final model
fit(summary(backwardVS), which = 1)


## -----------------------------------------------------------------------------
# Branch and bound with mtcars
VS <- VariableSelection(GammaFit, type = "branch and bound", showprogress = FALSE)
VS

## Getting final model
fit(summary(VS), which = 1)


## -----------------------------------------------------------------------------
# Can also use a formula and data
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC")
formulaVS

## Getting final model
fit(summary(formulaVS), which = 1)


## ---- fig.height = 4, fig.width = 6-------------------------------------------
# Finding top 10 models
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC", 
                               bestmodels = 10)
formulaVS

## Getting summary and plotting results
formulasumm <- summary(formulaVS) 
plot(formulasumm, type = "b")
plot(formulasumm, ptype = "variables")

## Getting best model
fit(formulasumm, which = 1)


## ---- fig.height = 4, fig.width = 6-------------------------------------------
# Finding all models with a AIC within 2 of the best model
formulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               showprogress = FALSE, metric = "AIC", 
                               cutoff = 2)
formulaVS

## Getting summary and plotting results
formulasumm <- summary(formulaVS) 
plot(formulasumm, type = "b")
plot(formulasumm, ptype = "variables")


## ---- fig.height = 4, fig.width = 6-------------------------------------------
# Example of using keep
keepVS <- VariableSelection(mpg ~ . ,data = cars, family = "gamma", 
                               link = "inverse", type = "branch and bound",
                               keep = c("hp", "cyl"), metric = "AIC",
                               showprogress = FALSE, bestmodels = 10)
keepVS

## Getting summary and plotting results
keepsumm <- summary(keepVS) 
plot(keepsumm, type = "b")
plot(keepsumm, ptype = "variables")

## Getting final model
fit(keepsumm, which = 1)


