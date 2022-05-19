## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval = FALSE------------------------------------------------------------
#  
#  devtools::install_github("JacobSeedorff21/BranchGLM")
#  

## -----------------------------------------------------------------------------
### Using mtcars

library(BranchGLM)

cars <- mtcars

### Fitting linear regression model with Fisher scoring

carsFit <- BranchGLM(mpg ~ ., data = cars, family = "gaussian", link = "identity")

carsFit

### Fitting linear regression with LBFGS

LBFGSFit <- BranchGLM(mpg ~ ., data = cars, family = "gaussian", link = "identity",
                      method = "LBFGS", grads = 5)

LBFGSFit


## -----------------------------------------------------------------------------
### Predict method

predict(carsFit)

### Accessing coefficients matrix

carsFit$coefficients


## -----------------------------------------------------------------------------
### Forward selection with mtcars

VariableSelection(carsFit, type = "forward")


## -----------------------------------------------------------------------------
### Backward elimination with mtcars

VariableSelection(carsFit, type = "backward")


## -----------------------------------------------------------------------------
### Branch and bound with mtcars

VariableSelection(carsFit, type = "branch and bound", showprogress = FALSE)

### Can also use a formula and data

FormulaVS <- VariableSelection(mpg ~ . ,data = cars, family = "gaussian", 
                               link = "identity", type = "branch and bound",
                               showprogress = FALSE)

### Number of models fit divided by the number of possible models

FormulaVS$numchecked / 2^(length(FormulaVS$variables))

### Extracting final model

FormulaVS$finalmodel


## -----------------------------------------------------------------------------
### Example of using keep

VariableSelection(mpg ~ . ,data = cars, family = "gaussian", 
                               link = "identity", type = "branch and bound",
                               keep = c("hp", "cyl"), metric = "BIC",
                               showprogress = FALSE)


## -----------------------------------------------------------------------------
### Predicting if a car gets at least 18 mpg

catData <- ToothGrowth

catFit <- BranchGLM(supp ~ ., data = catData, family = "binomial", link = "logit")

Table(catFit)


## -----------------------------------------------------------------------------

catROC <- ROC(catFit)

plot(catROC, main = "ROC Curve", col = "indianred")


## -----------------------------------------------------------------------------

Cindex(catFit)

AUC(catFit)


## ---- fig.width = 4, fig.height = 4-------------------------------------------
### Showing ROC plots for logit, probit, and cloglog

probitFit <- BranchGLM(supp ~ . ,data = catData, family = "binomial", 
                       link = "probit")

cloglogFit <- BranchGLM(supp ~ . ,data = catData, family = "binomial", 
                       link = "cloglog")

MultipleROCCurves(catROC, ROC(probitFit), ROC(cloglogFit), 
                  names = c("Logistic ROC", "Probit ROC", "Cloglog ROC"))


## -----------------------------------------------------------------------------

preds <- predict(catFit)

Table(preds, catData$supp)

AUC(preds, catData$supp)

ROC(preds, catData$supp) |> plot(main = "ROC Curve", col = "deepskyblue")


