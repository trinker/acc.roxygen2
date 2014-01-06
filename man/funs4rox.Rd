\name{funs4rox}
\alias{funs4rox}
\title{roxygen2 Format Functions}
\usage{
funs4rox(..., rdir = NULL, funs = NULL, environment = .GlobalEnv)
}
\arguments{
  \item{\ldots}{Functions (expressions) to create .R files
  for.}

  \item{rdir}{The location of the R directory to create the
  .R files.  If \code{NULL} prints to the console.}

  \item{funs}{Alternate character string (in lieu of
  \ldots).}

  \item{environment}{The environment to evaluate the
  function in.}
}
\value{
Returns a basic .R files for a function with roxygen2
documentation.
}
\description{
This function generates basic roxygen framework for
functions that can be set to print to your package's R
directory.
}
\examples{
## Create practice folders
library(reports)
folder("pack1/R"); folder("pack2/R")

## Expression functions
funs4rox(mean, pack_fun, lm, rdir="pack1/R")

## Character functions
funs <- c('mean', 'pack_fun', "lm")
funs4rox(funs = funs, rdir="pack2/R")

## Clean up
lapply(c("pack1", "pack2"), delete)
}

