## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# Loading BranchGLM package
library(BranchGLM)

# Using iris dataset to demonstrate usage of VI
Data <- iris
Fit <- BranchGLM(Sepal.Length ~ ., data = Data, family = "gaussian", link = "identity")

# Doing branch and bound selection 
VS <- VariableSelection(Fit, type = "branch and bound", metric = "BIC", 
showprogress = FALSE)

# Getting variable importance
VI <- VariableImportance(VS, showprogress = FALSE)
VI


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Plotting variable importance
oldmar <- par("mar")
par(mar = c(4, 6, 3, 1) + 0.1)
barplot(VI)
par(mar = oldmar)


## -----------------------------------------------------------------------------
# Getting approximate null distributions
set.seed(59903)
myBoot <- VariableImportance.boot(VI, nboot = 1000, showprogress = FALSE)
myBoot


## ----fig.height = 4, fig.width = 6--------------------------------------------
# Plotting histogram of results for second set of variables
hist(myBoot)

# Plotting boxplots of results
oldmar <- par("mar")
par(mar = c(4, 6, 3, 1) + 0.1)
boxplot(myBoot, las = 1)
par(mar = oldmar)



