\name{ann}
\alias{ann}
\title{Annotate}
\usage{
  ann(object = "clipboard", space = 1, symbol = "##")
}
\arguments{
  \item{object}{Object to annotate.  Defualt reads from the
  clipboard.}

  \item{space}{Number of spaces between annotation
  symbol(s) and annotated code.}

  \item{symbol}{The Symbol(s) to place at the begining of
  each line of the code for annotation.}
}
\description{
  Annotate R Code
}
\examples{
ann(mtcars)
ann(lm)
}

