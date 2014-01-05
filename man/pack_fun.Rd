\name{pack_fun}
\alias{pack_fun}
\title{Generate a Basic roxygen2 .R File}
\usage{
pack_fun(..., file = "", copy2clip = TRUE)
}
\arguments{
  \item{\ldots}{A function.}

  \item{file}{A connection, or a character string naming
  the file to print to. If \code{""} (the default),
  \code{cat} prints to the standard output connection, the
  console unless redirected by sink.}

  \item{copy2clip}{logical. If \code{TRUE} attempts to copy
  the output to the}
}
\value{
Returns a basic .R file for a function with roxygen2
documentation.
}
\description{
Generate a basic roxygen2 .R file
}
\examples{
pack_fun(pack_fun)
}

