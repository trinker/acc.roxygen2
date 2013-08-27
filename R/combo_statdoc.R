#' Combine Function Icons 
#'
#' This function allows for specifically combining functions under a common line.
#' 
#' @param path Path to the index.html file.
#' @param combine A list of vectors to combine together.  The functions will be 
#' combined int he order of each vector.
#' @param file The path/file name to output.
#' @export
#' @examples
#' \dontrun{
#' library(qdap)
#' (examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
#' file.copy(examp, to=getwd(), recursive = TRUE)
#' path <- paste0(getwd(), "/qdap/index.html")
#' file <- paste0(getwd(), "/qdap/index2.html")
#' library(qdap)
#' comb <- list(
#'     qcv(distTab, multiscale), 
#'     qcv(outlier.detect, cm_time.temp, word_list)
#' )
#' combo_statdoc(path, comb)
#' combo_statdoc(path, comb, file)
#}
combo_statdoc <- function(path, combine, file = NULL) {
    if (!is.list(combine)) {
        combine <- list(combine)
    }
    if (length(path) > 1) {
        x <- unlist(path)
    } else {
        x <- suppressWarnings(readLines(path))
    }
    terms <- lapply(combine, function(x) paste0("\">",  x, "</a>"))
    pars <- lapply(terms, function(v) {
        sapply(v, function(z) which(grepl(z, x)))
    })
    L1 <- sapply(pars, function(v) {
        p1 <- unlist(strsplit(x[v][1], "</code>"))
        p2 <- do.call(rbind, strsplit(x[v][-1], "</code>"))
        p2 <- unlist(paste0(" ", gsub("^\\s+", "", p2[, 1]), "</code>"))
        if (length(p2) > 1) {
            p2 <- paste(p2, collapse = "")
        }
        paste0(p1[1], "</code>", p2, p1[2])
    }) 
    reps <- sapply(pars, "[", 1)
    x[reps] <- L1
    rms <- unique(unlist(lapply(pars, function(v) {
        z <- v[-1]
        c(z-1, z, z+1)
    })))
    x <- x[-rms]
    if (!is.null(file)) {
        cat(paste(x, collapse="\n"), file = file)
    }
    return(x)
}

