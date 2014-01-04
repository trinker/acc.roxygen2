\name{funbody}
\alias{funbody}
\title{Grab Function and Body}
\usage{
funbody(fun, copy2clip = TRUE)
}
\arguments{
  \item{fun}{A function.}

  \item{copy2clip}{logical. If \code{TRUE} attempts to copy
  the output to the clipboard.}
}
\value{
Returns a character vector of a function and its body.
}
\description{
Grabs the supplied function and
}
\examples{
funbody(lm)
}

