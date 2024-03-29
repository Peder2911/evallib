% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bootstrap.R
\name{bootstrappedROC}
\alias{bootstrappedROC}
\title{bootstrappedROC}
\usage{
bootstrappedROC(pred, actual, res, draws = 100, parallel = FALSE)
}
\arguments{
\item{pred}{Probability predictions}

\item{actual}{Outcomes}

\item{res}{Resolution of ROC thresholds. A float between 0 and 1.}

\item{draws}{Number of times to randomly sample the data for bootstrapping.
Larger numbers yield better results, but also increase runtime.}
}
\value{
A list containing ROC data and AUC with confidence intervals.
}
\description{
Helper function that makes ROC graph data from predictions and outcomes with
confidence intervals and standard deviation.
}
\details{
Note that the function requires specification of the resolution, since 
it must be possible to collapse the results into an array.
}
\examples{
pred <- sample(seq(0,1,0.001), size = 150, replace = TRUE)
actual <- sample(c(0,1), size = 150, replace = TRUE)
bootstrappedROC(pred,actual,roc)

}
