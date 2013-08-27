#' R
#'
#' R
#' @param object
#' @param space
#' @param symbol
#' @export
ann <- 
function(object = "clipboard", space = 1, symbol="##") {
    if (length(object) == 1 && is.character(object) && object == "clipboard") {
        y <- as.list(readClipboard())
    } else {
        y <- as.list(capture.output(object))
    }
    spacer <- function(x) paste(symbol, paste(rep(" ", space), 
        collapse = ""), x, sep="")
    z <- sapply(y, spacer)
    zz <- as.matrix(as.data.frame(z))
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    writeClipboard(noquote(zz), format = 1)
    return(noquote(zz))
}
