#' Generate roxygen2 Documentation
#' 
#' Generate the roxygen2 basic documentation.
#' 
#' @param fun A function.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @export
#' @examples
#' roxfun(lm)
roxfun <- function(fun, copy2clip = TRUE) {
    fname <- as.character(substitute(fun))

    ## Get parameters
    pars <- suppressMessages(roxpars(fun))
    
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
