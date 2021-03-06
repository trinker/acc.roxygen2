% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/pars4rox.R
\name{roxpars}
\alias{pars4rox}
\alias{roxpars}
\title{roxygen2 Parameters}
\usage{
roxpars(fun, environment = .GlobalEnv, copy2clip = TRUE)

pars4rox(pars = NULL, copy2clip = TRUE)
}
\arguments{
\item{fun}{A function.}

\item{environment}{The environment to evaluate the function in.}

\item{copy2clip}{logical. If \code{TRUE} attempts to copy the output to the
clipboard.}
}
\description{
\code{roxpars} - Converts the arguments from a function to roxygen2
parameters.

\code{pars4rox} - Converts a string of comma separated parameters to roxygen2
style parameters.
}
\examples{
roxpars(mean)
}

