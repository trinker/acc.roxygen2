\name{removeDR}
\alias{removeDR}
\title{Remove the dontrun Pieces of Documentation}
\usage{
  removeDR(in_path = "C:/Users/trinker/GitHub/qdap/R",
    out_path = "C:/Users/trinker/GitHub/qdap2/R")
}
\arguments{
  \item{in_path}{The repo path to .R files with
  \code{dontrun}s.}

  \item{out_path}{The repo path to where to put the .R
  files without \code{dontrun}s.}
}
\description{
  Allows the creation of documentation with \code{dontrun}
  for upload but removal for staticdocs to a second file
  without fear of overwritting repo.
}
\examples{
\dontrun{
removeDR()
rmd("qdap2")
library(staticdocs)
build_package(package="C:/Users/trinker/GitHub/qdap2",
    base_path="C:/Users/trinker/GitHub/qdap/inst/staticdocs2", examples = TRUE)
}
}

