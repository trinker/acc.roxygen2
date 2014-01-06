#' roxygen2 Parameters
#'
#' \code{roxpars} - Converts the arguments from a function to roxygen2 
#' parameters.
#' 
#' @param fun A function.
#' @param environment The environment to evaluate the function in.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @export
#' @rdname roxpars
#' @importFrom qdap genX
#' @examples
#' roxpars(mean)
roxpars <- function (fun, environment = .GlobalEnv, copy2clip = TRUE) {

    char <- try(is.character(fun), silent = TRUE)
    if (class(char) != "try-error" && char) {
        fun <- as.name(fun)
    }

    text.var <- paste(head(capture.output(eval(substitute(args(fun)), 
    	environment)), -1), collapse = " ")
    text.var <- Trim(gsub("function (", "", text.var, fixed = TRUE))
    text.var <- substring(text.var, 1, nchar(text.var) - 1)
    pars4rox(text.var, copy2clip = copy2clip)
}

#' roxygen2 Parameters
#'
#' \code{pars4rox} - Converts a string of comma separated parameters to roxygen2 
#' style parameters.
#' 
#' @rdname roxpars
#' @export
#' @importFrom qdap Trim qcv spaste
pars4rox <-
function(pars = NULL, copy2clip = TRUE) {
    if (is.null(pars)) {
        pars <- unlist(as.list(read_clip()))
    }
    x <- paste0("#' @param", spaste(qcv(terms=pars, split = ",")))
    x <- Trim(sapply(strsplit(x, "="), function(x) x[1]))
    x <- gsub("\\.\\.\\.", "\\\\ldots", x)
    out <- paste(x, collapse = "\n")
	if (copy2clip) {
        write_clip(out)
	}
    message(out)
    invisible(out)
}


