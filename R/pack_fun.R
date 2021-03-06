#' Generate a Basic roxygen2 .R File
#' 
#' Generate a basic roxygen2 .R file
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
#' @return Returns a basic .R file for a function with roxygen2 documentation.
#' @export
#' @examples
#' pack_fun(pack_fun)
pack_fun <- function(..., file = "", append = FALSE, copy2clip = TRUE) {
    x1 <- suppressMessages(roxfun(...))
    x2 <- suppressMessages(funbody(...))
    out <- paste(c(x1, "\n", x2, "\n"), collapse="")
    if (copy2clip) {
        write_clip(out)
    }    
    cat(out, file = file, append = append)
    invisible(out)
}
