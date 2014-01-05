#' roxygen2 Format Functions
#'
#' This function generates basic roxygen framework for functions that 
#' can be set to print to your package's R directory.
#' 
#' @param \ldots A function.
#' @param file A connection, or a character string naming the file to print to. 
#' If \code{""} (the default), \code{cat} prints to the standard output 
#' connection, the console unless redirected by sink.
#' @param append append	logical. Only used if the argument file is the 
#' name of file (and not a connection or "|cmd"). If TRUE output will 
#' be appended to file; otherwise, it will overwrite the contents of file.  
#' This allows for appnding to the main package .R file.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a basic .R files for a function with roxygen2 documentation.
#' @export
#' @examples
#' pack_fun(pack_fun, lm)
funs4rox <- function(..., destination = "", copy2clip = TRUE, append = FALSE) {

## feed to pack_fun
## see dat4rox for structure
## destination will print to that folder, "" will print to console

}