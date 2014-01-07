#' roxygen2 Format Functions
#'
#' \code{funs4rox} - This function generates basic roxygen framework for 
#' functions that can be set to print to your package's R directory.
#' 
#' @param \ldots Functions (expressions) to create .R files for.
#' @param rdir The location of the R directory to create the .R files.  If 
#' \code{NULL} prints to the console.
#' @param funs Alternate character string (in lieu of \ldots).
#' @param environment The environment to evaluate the function in.
#' @return Returns a basic .R files for a function with roxygen2 documentation.
#' @rdname funs4rox
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

#' roxygen2 Format Functions
#'
#' \code{meths4rox} - This function generates basic roxygen framework for 
#' methods functions that can be set to print to your package's R directory.
#' 
#' @rdname funs4rox
#' @export
meths4rox <- function(..., rdir = NULL, funs = NULL, environment = .GlobalEnv) {

    if (is.null(funs)) {
        funs <- as.character(match.call(expand.dots = FALSE)[[2]])
    }

    invisible(lapply(funs, function(x) {

        file <- ifelse(is.null(rdir), "", file.path(rdir, paste0(x, ".R")))
        cat(paste(c(roxmeth2(x, environment = environment), 
        	funbody2(x, envir = environment), "\n"), 
        	collapse = "\n"), file = file)
    }))

}

roxmeth2 <- function(fun, environment = .GlobalEnv, copy2clip = TRUE) {

    ## Get parameters
    pars <- suppressMessages(roxpars(fun, environment = environment))
    meth1 <- meth2 <- beg2char(fun, ".") 
    if (meth1 == "summary") meth1 <- "summarize"
    repl <- gsub(paste0(meth2, "\\."), "", fun)
    vow <- c("A", "E", "I", "O", "U")
    art <- ifelse(substring(repl, 1, 1) %in% c(vow, tolower(vow)), "an", "a")

    ## Generate name and description
    name.desc <- sprintf(c("#' %ss %s %s Object", "#'",
        "#' %ss %s %s object.","#'"), CA(meth1), art, repl)
    ending <- sprintf(c("#' @method %s %s", 
        "#' @S3method %s %s"), meth2, repl)
    paste0(c(name.desc, pars, ending), collapse = "\n")
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
    x[1] <- paste(fun, "<-", x[1])
    x <- x[!grepl("<bytecode:|<environment:", x)]
    loneobrack <- grepl("^\\s*\\{\\s*$", x)
    x[which(loneobrack) - 1] <- paste0(x[which(loneobrack) - 1], x[loneobrack])
    x <- x[!loneobrack]
    paste0(x, collapse = "\n")
}


