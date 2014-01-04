#' Change read.me File
#'
#' Alter the read.me file with a preset.
#' 
#' @param path Path to the index.html file.
#' @param readme Path to the readme file.
#' @param file The path/file name to output.
#' @export
#' @examples
#' \dontrun{
#' (examp <- system.file("extdata/qdap", package = "acc.roxygen2"))
#' file.copy(examp, to=getwd(), recursive = TRUE)
#' path <- paste0(getwd(), "/qdap/index.html")
#' rdme <- system.file("extdata/readme.R", package = "acc.roxygen2")
#' file <- paste0(getwd(), "/qdap/index3.html")
#' readme_statdoc(path, rdme, file)
#' }
readme_statdoc <- function(path, readme, file = NULL) {
    if (length(path) > 1) {
        x <- path
    } else {
        x <- suppressWarnings(readLines(path))
    }
    y <- suppressWarnings(readLines(readme))
    start <- which(grepl("<h1>.+?</h1>", x))
    end <- which(grepl("<h2>Help topics</h2>", x))
    x <- c(x[1:start], "", y, "", x[end:length(x)])
    if (!is.null(file)) {
        cat(paste(x, collapse="\n"), file = file)
    }
    return(x)
}

