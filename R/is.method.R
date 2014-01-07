#' Test for Generic Methods
#' 
#' Test for objects of type generic method.
#' 
#' @param x An object.
#' @param meths The methods to match x against.
#' @param character.only logical.  If \code{TRUE} x is a character vector rather 
#' than an expression.
#' @return Returns logical assessment of an object being a method.
#' @keywords method
#' @export
#' @examples
#' is.method('print.table')
#' is.method(print.table)
is.method <- function(x, meths = c("plot", "print", "summary"), 
	character.only = FALSE) {
    if (!character.only) {
        x <- substitute(x)
    }
    beg2char(as.character(x), ".") %in% meths
}
