#' Annotate
#'
#' Annotate R Code
#' 
#' @param object Object to annotate.  Defualt reads from the clipboard.
#' @param space Number of spaces between annotation symbol(s) and annotated 
#' code.
#' @param symbol The Symbol(s) to place at the begining of each line of the code 
#' for annotation.
#' @export
#' @examples
#' ann(mtcars)
#' ann(lm)
ann <- 
function(object = "clipboard", space = 1, symbol="##") {
    if (length(object) == 1 && is.character(object) && object == "clipboard") {
        y <- as.list(read_clip())
    } else {
        y <- as.list(capture.output(object))
    }
    spacer <- function(x) paste(symbol, paste(rep(" ", space), 
        collapse = ""), x, sep="")
    z <- sapply(y, spacer)
    zz <- as.matrix(as.data.frame(z))
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    write_clip(noquote(zz))
    return(noquote(zz))
}
