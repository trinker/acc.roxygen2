#' Function Links
#' 
#' \code{see} - Generate function links for R documentation.
#' 
#' @param fun A function.
#' @param character.only logical.  If \code{TRUE} indicates whether function can 
#' be assumed to be character strings.
#' @export
#' @rdname see
#' @examples
#' see(lm)
#' library(qdap); library(tools)
#' seealso(lm, mean, file_ext, qcv) 
see <- function(fun, character.only = FALSE) {
    if (!character.only) {
       fun <- as.character(substitute(fun))
    }
    pack <- unlist(strsplit(find(fun), "\\:"))[2]
    out <- paste0("\\code{\\link[", pack, "]{", fun, "}}")
    write_clip(out)
    message(out)
    invisible(out)
}

#' Function Links
#' 
#' \code{seealso} - Generate multiple function links for roxygen2 documentation.
#' 
#' @param \ldots functions
#' @param ann logical.  Should the roxygen2 annotation be used.
#' @export
#' @rdname see
seealso <- function(..., ann = TRUE) {
    x <- substitute(...())
    funs <- unlist(lapply(x, function(y) as.character(y)))
    out <- unlist(suppressMessages(invisible(lapply(funs, see, 
        character.only = TRUE))))
    if(ann) {
        out <- paste("#'", out)
    }
    out <- paste(out, collapse = "\n")
    write_clip(out)
    message(out)
    invisible(out)
}

