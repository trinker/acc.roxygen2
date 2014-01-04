#' Add Extra staticdocs Icons 
#'
#' staticdocs writes multiple files contained in an .Rd file to one icon 
#' followed by a parenthesis with the other functions.  This function adds 
#' extra icons for supplied parenthesis functions.
#' 
#' @param path Path to the index.html file.
#' @param extra The extra functions found in the parenthesis for who new icons 
#' should be added.
#' @param file The path/file name to output.
#' @export
#' @examples
#' \dontrun{
#' (examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
#' file.copy(examp, to=getwd(), recursive = TRUE)
#' path <- paste0(getwd(), "/qdap/index.html")
#' file <- paste0(getwd(), "/qdap/index2.html")
#' library(qdap)
#' extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, linsear_write, SMOG)
#' exbutton_statdoc(path, extras)
#' exbutton_statdoc(path, extras, file)
#' }
exbutton_statdoc <- function(path, extra, file = NULL) {
    x <- suppressWarnings(readLines(path))
    pars <- which(grepl("a></code>(.+?)<br", x))
    partxt <- x[pars]
    extras <- lapply(partxt,  function(x) {
        gsub(".*a></code>\\((.+?)\\)<br.*", "\\1", x)
    })
    s.extras <- lapply(extras, function(x) {
        qcv(terms = x, split=",")
    })
    lens <- sapply(s.extras, length)
    locs <- sapply(extra, function(x) which(unlist(s.extras) %in% x))
    mlocs <- rep(1:length(partxt), lens)
    nlocs <- mlocs[locs]
    names(nlocs) <- names(locs)
    npartxt <- paste0("        ", mgsub(extra, "", partxt))
    ns.extras <- lapply(s.extras, function(x) {
        x[!x %in% extra]
    })
    parrepl <- sapply(ns.extras, function(x) {
        if (identical(x, character(0))) {
            NA
        } else {
            paste(x, collapse = ", ")
        }
    })
    n.partxt <- sapply(seq_along(parrepl), function(i) {
        if (is.na(parrepl[i])) {
            bet <- ""
        } else {
            bet <- paste0("\\(", parrepl[i], "\\)")
        }
        insert <- paste0("a></code>", bet, "<br />")
        gsub("a></code>\\((.+?)\\)<br />", insert, partxt[i])
    })
    pieces1 <- do.call(rbind, strsplit(partxt, "</a></code>"))
    pieces2 <- do.call(rbind, strsplit(pieces1[, 1], "\">"))
    paste0(pieces2[, 1], "\">", "</a></code>")
    pieces3 <- do.call(rbind, strsplit(n.partxt, "</code>"))
    out <- sapply(seq_along(partxt), function(i) {
        if (sum(nlocs %in% i) == 0) {
            ""
        } else {
            x <- names(nlocs)[nlocs %in% i]
            m <- sapply(x, function(z) {
                paste0(gsub("^\\s+|\\s+$", "", pieces2[i]), "\">", z, "</a></code>")
            })
            paste0(" ", paste(m, collapse=" "))
        }
    })
    changed <- paste0(pieces3[, 1], "</code>", out, pieces3[, 2])
    x[pars] <- changed
    if (!is.null(file)) {
        cat(paste(x, collapse="\n"), file = file)
    }
    return(x)
}

