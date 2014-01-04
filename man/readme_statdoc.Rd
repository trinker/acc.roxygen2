\name{readme_statdoc}
\alias{readme_statdoc}
\title{Change read.me File}
\usage{
readme_statdoc(path, readme, file = NULL)
}
\arguments{
  \item{path}{Path to the index.html file.}

  \item{readme}{Path to the readme file.}

  \item{file}{The path/file name to output.}
}
\description{
Alter the read.me file with a preset.
}
\examples{
\dontrun{
(examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
file.copy(examp, to=getwd(), recursive = TRUE)
path <- paste0(getwd(), "/qdap/index.html")
rdme <- system.file("extdata/readme.R", package = "acc.roxygen2")
file <- paste0(getwd(), "/qdap/index3.html")
readme_statdoc(path, rdme, file)
}

