\name{funs4rox}
\alias{funs4rox}
\title{roxygen2 Format Functions}
\usage{
funs4rox(..., destination = "", copy2clip = TRUE, append = FALSE)
}
\arguments{
  \item{\ldots}{A function.}

  \item{file}{A connection, or a character string naming
  the file to print to. If \code{""} (the default),
  \code{cat} prints to the standard output connection, the
  console unless redirected by sink.}

  \item{append}{append logical. Only used if the argument
  file is the name of file (and not a connection or
  "|cmd"). If TRUE output will be appended to file;
  otherwise, it will overwrite the contents of file. This
  allows for appnding to the main package .R file.}

  \item{copy2clip}{logical. If \code{TRUE} attempts to copy
  the output to the clipboard.}
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
pack_fun(pack_fun, lm)
}

