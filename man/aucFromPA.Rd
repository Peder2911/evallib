% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics.R
\name{aucFromPA}
\alias{aucFromPA}
\title{aucFromPA}
\usage{
aucFromPA(p, actual, ...)
}
\arguments{
\item{p}{Vector of predicted probabilities}

\item{actual}{Vector of actual scores}
}
\value{
An AUC score
}
\description{
Just a helper function that yields an AUC score from predicted and actual values.
}
\examples{
predicted <- sample(seq(0,1,0.01),replace = TRUE, size = 150)
actual <- sample(c(1,0),replace = TRUE, size = 150)
aucFromPA(predicted,actual)
}
