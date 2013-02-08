\name{expand_statdoc}
\alias{expand_statdoc}
\title{Expand staticdocs index.html File}
\usage{
  expand_statdoc(path, file = NULL, drop = NULL,
    rm.other = TRUE)
}
\arguments{
  \item{path}{Path to the index.html file.}

  \item{file}{The path/file name to output.  If NULL the
  original index is overwritten.}

  \item{drop}{A character vector of functions in the
  parenthesis that should be excluded from the index file.}
}
\description{
  staticdocs writes multiple files contained in an .Rd file
  to one icon followed by a parenthesis with the other
  functions.  This function exapands the parenthesis to be
  separate icons of their own.
}
\examples{
\dontrun{
(examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
file.copy(examp, to=getwd(), recursive = TRUE)
path <- paste0(getwd(), "/qdap/index.html")
file <- paste0(getwd(), "/qdap/index2.html")

library(qdap)
expand_statdoc(path, file, qcv(syn, mgsub, adjmat, wc))
expand_statdoc(path, , qcv(syn, mgsub, adjmat, wc))
}

