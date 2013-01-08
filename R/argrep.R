#' R
#'
#' R
#' @param args
#' @export
argrep <-
function(pars = NULL) {
    if (is.null(pars)) {
        pars <- unlist(as.list(readClipboard()))
    }
    x <- qdap::qcv(terms=pars, split = ",")
    x <- qdap::Trim(sapply(strsplit(x, "="), function(x) x[1]))
    x <- paste(paste(x, "=", x), collapse = ", ")
    zz <- matrix(x, nrow=1)
    dimnames(zz) <- list(c(rep("", nrow(zz))), c(""))
    writeClipboard(noquote(zz), format = 1)
    return(noquote(zz))
}
