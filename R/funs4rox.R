#' roxygen2 Format Functions
#'
#' This function generates basic roxygen framework for functions that 
#' can be set to print to your package's R directory.
#' 
#' @param \ldots Functions (expressions) to create .R files for.
#' @param rdir The location of the R directory to create the .R files.  If 
#' \code{NULL} prints to the console.
#' @param funs Alternate character string (in lieu of \ldots).
#' @param environment The environment to evaluate the function in.
#' @return Returns a basic .R files for a function with roxygen2 documentation.
#' @export
#' @examples
#' ## Create practice folders
#' library(reports)
#' folder("pack1/R"); folder("pack2/R")
#' 
#' ## Expression functions
#' funs4rox(mean, pack_fun, lm, rdir="pack1/R")
#' 
#' ## Character functions
#' funs <- c('mean', 'pack_fun', "lm")
#' funs4rox(funs = funs, rdir="pack2/R")
#' 
#' ## Clean up
#' lapply(c("pack1", "pack2"), delete)
funs4rox <- function(..., rdir = NULL, funs = NULL, environment = .GlobalEnv) {

    if (is.null(funs)) {
        funs <- as.character(match.call(expand.dots = FALSE)[[2]])
    }

    invisible(lapply(funs, function(x) {
        file <- ifelse(is.null(rdir), "", file.path(rdir, paste0(x, ".R")))
        cat(paste(c(roxfun2(x, environment = environment), 
        	funbody2(x, envir = environment), "\n"), 
        	collapse = "\n"), file = file)
    }))

}

roxfun2 <- function (fun, environment = .GlobalEnv) {
    pars <- suppressMessages(roxpars(fun, environment = environment))
    name.desc <- c("#' Title", "#' ", "#' Description", "#' ")
    ending <- c("#' @return", "#' @references", "#' @keywords", 
        "#' @export", "#' @seealso", "#' @examples")
    paste0(c(name.desc, pars, ending), collapse = "\n")
}

funbody2 <- function (fun, pos = 1, envir = as.environment(pos)) {
    x <- capture.output(get(fun, envir = envir))
    x[1] <- paste(as.character(substitute(fun)), "<-", x[1])
    x <- x[!grepl("<bytecode:|<environment:", x)]
    loneobrack <- grepl("^\\s*\\{\\s*$", x)
    x[which(loneobrack) - 1] <- paste0(x[which(loneobrack) - 1], x[loneobrack])
    x <- x[!loneobrack]
    paste0(x, collapse = "\n")
}

