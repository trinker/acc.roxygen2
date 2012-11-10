ann <-
function(object = NULL, space = 1, symbol="#") {
    y <- if (is.null(object)) {
        as.list(readClipboard())
    } else {
        strsplit(as.vector(object), "[\n]")
    }
    spacer <- function(x) paste(symbol, paste(rep(" ", space), 
        collapse = ""), x, sep="")
    z <- if (object == "clipboard") {
        sapply(y, spacer)
    } else {
        lapply(y, spacer)
    }
    zz <- as.matrix(as.data.frame(z))
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    writeClipboard(noquote(zz), format = 1)
    return(noquote(zz))
}
