#' Grab Function and Body
#' 
#' Grabs the supplied function and 
#' 
#' @param fun A function.
#' @param copy2clip logical. If \code{TRUE} attempts to copy the output to the 
#' clipboard.
#' @return Returns a character vector of a function and its body.
#' @export
#' @examples
#' funbody(lm)
funbody <- function(fun, copy2clip = TRUE) {
    x <- capture.output(fun)
    x[1] <- paste(as.character(substitute(fun)), "<-", x[1])
    x <- x[!grepl("<bytecode:|<environment:", x)]

    ## Determine if a open bracket is on line by self
    loneobrack <- grepl("^\\s*\\{\\s*$", x)
    x[which(loneobrack) - 1] <- paste0(x[which(loneobrack) - 1], 
        x[loneobrack])
    x <- x[!loneobrack] 

    out <- paste0(x, collapse = "\n")
	if (copy2clip) {
        write_clip(out)
	}
    message(out)
	invisible(out)
}

