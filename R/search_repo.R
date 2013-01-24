#' Search a Repo for Terms
#'
#' Generates counts of terms found in the .R documentation.
#' 
#' @param \ldots Terms to search for in the .R files.  No need for quotes.
#' @param repo Repo name.
#' @param base.git Base path to where repos are store (the repo is stored).
#' @param terms An optional argument to present the terms as one long character 
#' string.  This is useful if the split (separator) is not a comma (e.g. spaces 
#' are the term separators).
#' @param split Character vector of length one to use for splitting (i.e. the 
#' separator used in the vector).  For use with the argument \code{terms}.
#' @import qdap
#' @export
search_repo <- function(..., repo = "qdap", 
    base.git = "C:/Users/trinker/GitHub/", terms = NULL, split = "\\|") {
    require(qdap)
    path <- paste0(base.git, "/", repo, "/R/")
    if (!is.null(terms)) {
        x <- strsplit(terms, split = split)
    } else {
        x <- substitute(...())
    }
    unblanker <- function(x)subset(x, nchar(x)>0)
    z <- unblanker(scrubber(unlist(lapply(x, function(y) as.character(y)))))
    z <- unblanker(z)
    WD <- getwd()
    setwd(path)
    m <- suppressWarnings(sapply(dir(), readLines))
    n <- do.call(rbind, lapply(seq_along(m), function(i){
        data.frame(fun=rep(names(m)[i], length(m[[i]])), text=m[[i]])
    }))
    x <- with(n, termco(text, fun, z))
    y <- x$raw[rowSums(x$raw[, -c(1:2), drop=FALSE]) !=0, ]
    setwd(WD)
    left.just(replacer(y), 1)[, -2]
}