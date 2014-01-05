\name{roxfun}
\alias{roxfun}
\title{Generate roxygen2 Documentation}
\usage{
roxfun(fun, copy2clip = TRUE)
}
\arguments{
  \item{fun}{A function.}

  \item{copy2clip}{logical. If \code{TRUE} attempts to copy
  the output to the clipboard.}
}
\description{
Generate the roxygen2 basic documentation.
}
\examples{
roxfun(lm)
}

