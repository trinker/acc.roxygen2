\name{exbutton_statdoc}
\alias{exbutton_statdoc}
\title{Add Extra staticdocs Icons}
\usage{
exbutton_statdoc(path, extra, file = NULL)
}
\arguments{
  \item{path}{Path to the index.html file.}

  \item{extra}{The extra functions found in the parenthesis
  for who new icons should be added.}

  \item{file}{The path/file name to output.}
}
\description{
staticdocs writes multiple files contained in an .Rd file
to one icon followed by a parenthesis with the other
functions.  This function adds extra icons for supplied
parenthesis functions.
}
\examples{
\dontrun{
(examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
file.copy(examp, to=getwd(), recursive = TRUE)
path <- paste0(getwd(), "/qdap/index.html")
file <- paste0(getwd(), "/qdap/index2.html")
library(qdap)
extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, linsear_write, SMOG)
exbutton_statdoc(path, extras)
exbutton_statdoc(path, extras, file)
}

