\name{search_repo}
\alias{search_repo}
\title{Search a Repo for Terms}
\usage{
  search_repo(..., repo = "qdap",
    base.git = "C:/Users/trinker/GitHub/", terms = NULL,
    split = "\\|")
}
\arguments{
  \item{\ldots}{Terms to search for in the .R files.  No
  need for quotes.}

  \item{repo}{Repo name.}

  \item{base.git}{Base path to where repos are store (the
  repo is stored).}

  \item{terms}{An optional argument to present the terms as
  one long character string.  This is useful if the split
  (separator) is not a comma (e.g. spaces are the term
  separators).}

  \item{split}{Character vector of length one to use for
  splitting (i.e. the separator used in the vector).  For
  use with the argument \code{terms}.}
}
\description{
  Generates counts of terms found in the .R documentation.
}

