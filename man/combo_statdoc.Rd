\name{combo_statdoc}
\alias{combo_statdoc}
\title{Combine Function Icons}
\usage{
  combo_statdoc(path, combine, file = NULL)
}
\arguments{
  \item{path}{Path to the index.html file.}

  \item{combine}{A list of vectors to combine together.
  The functions will be combined int he order of each
  vector.}

  \item{file}{The path/file name to output.}
}
\description{
  This function allows for specifically combining functions
  under a common line.
}
\examples{
\dontrun{
library(qdap)
(examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
file.copy(examp, to=getwd(), recursive = TRUE)
path <- paste0(getwd(), "/qdap/index.html")
file <- paste0(getwd(), "/qdap/index2.html")
library(qdap)
comb <- list(
    qcv(distTab, multiscale),
    qcv(outlier.detect, cm_time.temp, word_list)
)
combo_statdoc(path, comb)
combo_statdoc(path, comb, file)
}

