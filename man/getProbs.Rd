% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProbs.R
\name{getProbs}
\alias{getProbs}
\title{The Get Probs Function (Emulates Binomial Experiment)}
\usage{
getProbs(min = 0, max = 10, size = 1, p = 0.5)
}
\arguments{
\item{min}{-> minimum number of successes}

\item{max}{-> maximum number of successes}

\item{size}{-> number of trials in experiment}

\item{p}{-> probability of a success}
}
\value{
A Table of each possible number of successes between min and max and their distribution using dbinom
}
\description{
The Get Probs Function (Emulates Binomial Experiment)
}
\examples{
getProbs(0, 10, 10, 0.5)
}
