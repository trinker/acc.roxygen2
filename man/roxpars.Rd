\name{roxpars}
\alias{pars4rox}
\alias{roxpars}
\title{roxygen2 Parameters}
\usage{
roxpars(fun)

pars4rox(pars = NULL)
}
\arguments{
  \item{fun}{}

  \item{pars}{A string of comma separated parameters.}
}
\description{
\code{roxpars} - Converts the arguments from a function to
roxygen2 parameters.

\code{pars4rox} - Converts a string of comma separated
parameters to roxygen2 style parameters.
}
\examples{
roxpars(mean)
}

