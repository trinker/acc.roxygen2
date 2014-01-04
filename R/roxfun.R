#' Title
#' 
#' Description
#' 
#' @param fun A function.
#' @export
#' @examples
#' roxfun(lm)
roxfun <- function(fun) {
    fname <- as.character(substitute(fun))

    ## Get parameters
    pars <- suppressMessages(roxpars(fun))
    
    ## Generate name and description
    name.desc <- c("#' Title", "#' ", "#' Description", "#' ")
    ending <- c("#' @return", "#' @references", "#' @keywords", "#' @export", 
        "#' @seealso", "#' @examples")
    out <- paste0(c(name.desc, pars, ending), collapse = "\n")
    write_clip(out)
    message(out)
}
