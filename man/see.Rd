\name{see}
\alias{see}
\alias{seealso}
\title{Function Links}
\usage{
  see(fun, character.only = FALSE)

  seealso(..., ann = TRUE)
}
\arguments{
  \item{fun}{A function.}

  \item{character.only}{logical.  If \code{TRUE} indicates
  whether function can be assumed to be character strings.}

  \item{\ldots}{functions}

  \item{ann}{logical.  Should the roxygen2 annotation be
  used.}
}
\description{
  \code{see} - Generate function links for R documentation.

  \code{seealso} - Generate multiple function links for
  roxygen2 documentation.
}
\examples{
see(lm)
library(qdap); library(tools)
seealso(lm, mean, file_ext, qcv)
}

