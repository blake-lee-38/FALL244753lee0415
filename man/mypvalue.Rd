% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mypvalue.R
\name{mypvalue}
\alias{mypvalue}
\title{The my P value function plots and returns the p value for a specified tcalc value and sample size}
\usage{
mypvalue(t0, xmax = 4, n = 20, alpha = 0.05)
}
\arguments{
\item{t0}{-> Tcalc value}

\item{xmax}{-> Used to control histogram}

\item{n}{-> Sample size}

\item{alpha}{-> For confidence level 1 - alpha}
}
\value{
A histogram of the p-value for t-calc and a list with the p value and q value
}
\description{
The my P value function plots and returns the p value for a specified tcalc value and sample size
}
\examples{
mypvalue(t0 = 2, n = 30)
}
