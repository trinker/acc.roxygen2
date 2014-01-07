#' Generate roxygen2 Documentation
#' 
#' Generate the roxygen2 basic documentation.
#' 
#' @param fun A function.
#' @param environment The environment to evaluate the function in.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
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

