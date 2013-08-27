#' roxygen2 Parameters
#'
#' \code{roxpars} - Converts the arguments from a function to roxygen2 
#' parameters.
#' 
#' @param fun
#' @rdname roxpars
#' @export
#' @importFrom qdap genX
#' @examples
#' roxpars(mean)
roxpars <- 
function(fun) {
    ## Get parameters
    text.var <- paste(head(capture.output(args(fun)), -1), collapse = " ")
    text.var <- Trim(gsub("function (", "", text.var, fixed = TRUE))
    text.var <- substring(text.var, 1, nchar(text.var) - 1)
    pars4rox(text.var)
}

#' roxygen2 Parameters
#'
#' \code{pars4rox} - Converts a string of comma separated parameters to roxygen2 style parameters.
#' 
#' @param pars A string of comma separated parameters. 
#' @rdname roxpars
#' @export
#' @importFrom qdap Trim qcv spaste
pars4rox <-
function(pars = NULL) {
    if (is.null(pars)) {
        pars <- unlist(as.list(read_clip()))
    }
    x <- paste0("#' @param", spaste(qcv(terms=pars, split = ",")))
    x <- Trim(sapply(strsplit(x, "="), function(x) x[1]))
    x <- gsub("\\.\\.\\.", "\\\\ldots", x)
    out <- paste(x, collapse = "\n")
    write_clip(out)
    message(out)
    invisible(out)
}


