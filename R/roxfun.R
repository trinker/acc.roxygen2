#' Generate roxygen2 Documentation
#' 
#' \code{roxfun} - Generate the roxygen2 basic documentation for general 
#' functions.
#' 
#' @param fun A function.
#' @param environment The environment to evaluate the function in.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @rdname roxfun
#' @export
#' @examples
#' roxfun(lm)
roxfun <- function(fun, environment = .GlobalEnv, copy2clip = TRUE) {
    fun <- as.character(substitute(fun))

    ## Get parameters
    pars <- suppressMessages(roxpars(fun, environment = environment))
    
    ## Generate name and description
    name.desc <- c("#' Title", "#' ", "#' Description", "#' ")
    ending <- c("#' @return", "#' @references", "#' @keywords", "#' @export", 
        "#' @seealso", "#' @examples")
    out <- paste0(c(name.desc, pars, ending), collapse = "\n")
        if (copy2clip) {
            write_clip(out)
        }
    message(out)
    invisible(out)
}

#' Generate roxygen2 Documentation
#' 
#' \code{roxfun} - Generate the roxygen2 basic documentation for methods 
#' functions print, plot and summary.
#' 
#' @rdname roxfun
#' @export
roxmeth <- function(fun, environment = .GlobalEnv, copy2clip = TRUE) {
    fun <- as.character(substitute(fun))
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
    out <- paste0(c(name.desc, pars, ending), collapse = "\n")
    if (copy2clip) {
        write_clip(out)
    }
    message(out)
    invisible(out)
}


