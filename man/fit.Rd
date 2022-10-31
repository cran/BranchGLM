% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summaryBranchGLMVS.R
\name{fit}
\alias{fit}
\alias{fit.summary.BranchGLMVS}
\title{Fits GLMs for summary.BranchGLMVS objects}
\usage{
fit(object, ...)

\method{fit}{summary.BranchGLMVS}(object, which = 1, keepData = TRUE, keepY = TRUE, ...)
}
\arguments{
\item{object}{a \code{summary.BranchGLMVS} object.}

\item{...}{further arguments passed to other methods.}

\item{which}{a positive integer indicating which model to fit, 
the default is to fit the first model .}

\item{keepData}{Whether or not to store a copy of data and design matrix, the default 
is TRUE. If this is FALSE, then the results from this cannot be used inside of \code{VariableSelection}.}

\item{keepY}{Whether or not to store a copy of y, the default is TRUE. If 
this is FALSE, then the binomial GLM helper functions may not work and this 
cannot be used inside of \code{VariableSelection}.}
}
\value{
An object of class \code{\link{BranchGLM}}.
}
\description{
Fits GLMs for summary.BranchGLMVS objects
}
\details{
The information needed to fit the GLM is taken from the original information 
supplied to the \code{VariableSelection} function.
}
\examples{
Data <- iris
Fit <- BranchGLM(Sepal.Length ~ ., data = Data, family = "gaussian", link = "identity")

# Doing branch and bound selection 
VS <- VariableSelection(Fit, type = "branch and bound", metric = "BIC", 
bestmodels = 10, showprogress = FALSE)

## Getting summary of the process
Summ <- summary(VS)

## Getting the best model according to BIC
FinalModel <- fit(Summ, which = 1)
FinalModel

## Getting the 8th best model according to BIC
EighthModel <- fit(Summ, which = 8)
EighthModel

}