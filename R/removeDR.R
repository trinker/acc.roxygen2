#' Remove the dontrun Pieces of Documentation
#'
#' Allows the creation of documentation with \code{dontrun} for upload but 
#' removal for staticdocs to a second file without fear of overwritting repo.
#' 
#' @param in_path The repo path to .R files with \code{dontrun}s.
#' @param out_path The repo path to where to put the .R files without 
#' \code{dontrun}s.
#' @export
#' @examples
#' \dontrun{
#' removeDR()
#' rmd("qdap2")
#' library(staticdocs)
#' build_package(package="C:/Users/trinker/GitHub/qdap2", 
#'     base_path="C:/Users/trinker/GitHub/qdap/inst/staticdocs2", examples = TRUE)
#' }
removeDR <- function(in_path="C:/Users/trinker/GitHub/qdap/R", 
    out_path="C:/Users/trinker/GitHub/qdap2/R") {
    require(qdap); require(devtools)
    delete(paste0(out_path, "/", dir(out_path)))
    files <- paste0(in_path, "/", dir(in_path))
    ofiles <- paste0(out_path, "/", dir(in_path))
    rac <- function(x) {
        lines <- readLines(x)
        starts <- grep("@examples", lines) + 1
        if (identical(starts, numeric(0))) {
            return(lines)
        }
        nonrox <- grep("#'", lines, invert = TRUE) 
        ends <- sapply(seq_along(starts), function(i) {
            nonrox[starts[i] < nonrox][1]
            }, USE.NAMES = FALSE) - 1
        selects <- ends - starts != 0 
        ends <- ends[selects]
        starts <- starts[selects]
        dnr <- grepl("#' \\dontrun{", lines, fixed = TRUE)
        if(any(dnr)) {
            lines <- lines[!dnr]
            rms <- ends - which(grepl("#' }", rev(lines[1:(ends-1)]), fixed = TRUE))[1] 
            lines <- lines[-rms]
        }
        lines
    }
    invisible(lapply(seq_along(files), function(i) {
        cat(paste(rac(files[i]), collapse="\n"), file = ofiles[i])
    }))
    cat("R files written!\n")
}
