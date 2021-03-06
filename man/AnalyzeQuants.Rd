% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnalyzeQuants.R
\name{AnalyzeQuants}
\alias{AnalyzeQuants}
\title{Helper function, analyzes quantile results matrix}
\usage{
AnalyzeQuants(QuantMatrix, Quantiles, shape, scale)
}
\arguments{
\item{QuantMatrix}{Input matrix which should come from QuantAll}

\item{Quantiles}{Quantiles being estimated}

\item{shape}{Shape of Weibull distribution, used to find true values}

\item{scale}{Scale of Weibull distribution, used to find true values}
}
\description{
Helper function which extracts coverage and quantile estimates
}
