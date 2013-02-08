#' Expand staticdocs index.html File
#'
#' staticdocs writes multiple files contained in an .Rd file to one icon 
#' followed by a parenthesis with the other functions.  This function exapands 
#' the parenthesis to be separate icons of their own.
#' 
#' @param path Path to the index.html file.
#' @param file The path/file name to output.  If NULL the original index is 
#' overwritten.
#' @param drop A character vector of functions in the parenthesis that should be 
#' excluded from the index file.
#' @export
#' @examples
#' \dontrun{
#' (examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
#' file.copy(examp, to=getwd(), recursive = TRUE)
#' path <- paste0(getwd(), "/qdap/index.html")
#' file <- paste0(getwd(), "/qdap/index2.html")
#' 
#' library(qdap)
#' expand_statdoc(path, file, qcv(syn, mgsub, adjmat, wc))
#' expand_statdoc(path, , qcv(syn, mgsub, adjmat, wc))
#}
expand_statdoc <- function(path, file = NULL, drop = NULL, rm.other = TRUE) {
    x <- suppressWarnings(readLines(path))
    pars <- which(grepl("a></code>(.+?)<br", x))
    partxt <- x[pars]
    new <- gsub("a></code>(.+?)<br", "a></code><br", x)
    extras <- lapply(partxt,  function(x) {
        gsub(".*a></code>\\((.+?)\\)<br.*", "\\1", x)
    })
    s.extras <- lapply(extras, function(x) {
        qcv(terms = x, split=",")
    })
    lens <- sapply(s.extras, length)
    lens[-1] <- lens[-1]*3
    if (!is.null(drop)) {
        s.extras <- lapply(s.extras, function(x) {
            x[!x %in% drop]
        })
    }
    partxt2 <- new[pars]   
    expands <- lapply(seq_along(partxt2), function(i) {
        sapply(seq_along(s.extras[[i]]), function(j) {
            c("      <li>", 
                gsub("\">(.+?)</a></code><br", 
                paste0("\">", 
                s.extras[[i]][j], "</a></code><br"), partxt2[i]),
                "            ")
        })
    })
    cum.lens <- c(0, head(cumsum(sapply(expands, length)), -1))
    invisible(lapply(seq_along(partxt2), function(i) {
        new <<- append(new, expands[[i]], after=c(pars + cum.lens)[i])
    }))
    if (is.null(file)) file <- path
    if (rm.other) {
        begin <- grep("<h3>Other</h3>", new)
        ul <- grep("</ul>", new) 
        end <- ul[ul - begin > 0 ][1] + 1
        new <- new[c(1:(begin - 1), end:length(new))]
    }
    cat(paste(new, collapse="\n"), file = file)
}
