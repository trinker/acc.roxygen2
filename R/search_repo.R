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
#' @param ignore.case logical.  If \code{FALSE}, the pattern matching is case 
#' sensitive and if \code{TRUE}, case is ignored during matching.
#' @param perl logical. Should Perl-compatible regexps be used?
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as 
#' is. Overrides all conflicting arguments.
#' @importFrom qdap left_just
#' @export
search_repo <- function (..., repo = getOption("primary_repo"), base.git = getOption("base_git"), 
    terms = NULL, split = "\\|", ignore.case = TRUE, perl = FALSE, fixed = FALSE) {

    path <- file.path(base.git, repo, "R")
    if (!is.null(terms)) {
        terms <- strsplit(terms, split = split)
    } else {
        terms <- substitute(...())
    }
    m <- setNames(suppressWarnings(sapply(file.path(path, dir(path)), readLines)), dir(path))

    out2 <- setNames(lapply(terms, function(x) {
        out <- lapply(m, function(y = m, z = x) {
            grep(x, y, ignore.case = ignore.case, perl = perl, fixed = fixed)
        })
        out <- sapply(out[!sapply(out, identical, integer(0))], paste, collapse=", ")
        data.frame(file=names(out), row.number=out, row.names=NULL)
    }), unlist(terms))

    lapply(out2, qdap::left_just)
}
