% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/VariableSelection.R, R/summaryBranchGLMVS.R
\name{predict.BranchGLMVS}
\alias{predict.BranchGLMVS}
\alias{predict.summary.BranchGLMVS}
\title{Predict Method for BranchGLMVS or summary.BranchGLMVS Objects}
\usage{
\method{predict}{BranchGLMVS}(object, which = 1, ...)

\method{predict}{summary.BranchGLMVS}(object, which = 1, ...)
}
\arguments{
\item{object}{a \code{BranchGLMVS} or \code{summary.BranchGLMVS} object.}

\item{which}{a positive integer to indicate which model to get predictions from,
the default is 1 which is used for the best model. For the branch and bound
algorithms the number k is used for the kth best model and for the stepwise algorithms
the number k is used for the model that is k - 1 steps away from the final model.}

\item{...}{further arguments passed to \link{predict.BranchGLM}.}
}
\value{
A numeric vector of predictions.
}
\description{
Obtains predictions from BranchGLMVS or summary.BranchGLMVS objects.
}
\examples{
Data <- iris
Fit <- BranchGLM(Sepal.Length ~ ., data = Data, 
family = "gamma", link = "log")

# Doing branch and bound selection 
VS <- VariableSelection(Fit, type = "branch and bound", metric = "BIC", 
bestmodels = 10, showprogress = FALSE)

## Getting predictions from best model
predict(VS, which = 1)

## Getting linear predictors from 5th best model
predict(VS, which = 5, type = "linpreds")

}
\seealso{
\link{predict.BranchGLM}
}
