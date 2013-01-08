#' Roxygen Format Data Sets
#'
#' This function generates basic roxygen framework for data sets that 
#' can be set to print to your package's main .R file.
#'
#' @param \ldots Package datasets.
#' @param file A connection, or a character string naming the file to 
#' print to. If "" (the default), dat4rox prints to the standard output 
#' connection, the console.
#' @param append append	logical. Only used if the argument file is the 
#' name of file (and not a connection or "|cmd"). If TRUE output will 
#' be appended to file; otherwise, it will overwrite the contents of file.  
#' This allows for appnding to the main package .R file.
#' @keywords roxygen, data
#' @export
#' @examples
#' \dontrun{
#' dat4rox(mtcars, CO2)                   #print only to console
#' dat4rox(mtcars, CO2, file = "new.txt") #print to new file
#' #print to package.R file
#' #dat4rox(mtcars, CO2, file = "qdap-package.R", append = TRUE) 
#' }
dat4rox <- function(..., file = NULL, append = FALSE) {
    dat.sets <- as.character(match.call(expand.dots = FALSE)[[2]]) 
    dat.list <- invisible(lapply(dat.sets, get))
    names(dat.list) <- dat.sets
    dat.file <- function(dat, name, file = "", append = FALSE) {
        x <- "#'"
        out <- c("#'", x, "#' A dataset containing", x, "#' \\itemize{", 
            paste("#'   \\item ", colnames(dat), ".", sep = ""),
            "#' }", x, "#' @docType data", "#' @keywords datasets",
            paste("#' @name", name), paste0("#' @usage data(", name, ")"),
            paste("#' @format A data frame with", nrow(dat), "rows and", 
                ncol(dat), "variables"), "#' @references", "NULL\n")
        cat(paste(out, "\n", collapse=""), file = file, append = append)
    }
    invisible(lapply(seq_along(dat.list), function(i) {
        dat.file(dat.list[[i]], names(dat.list)[i])
    }))
    if (!is.null(file)) {
        apen <- rep(TRUE, length(dat.list))
        if (!append) {
            apen[1] <- FALSE
        }
        invisible(lapply(seq_along(dat.list), function(i) {
            dat.file(dat.list[[i]], names(dat.list)[i], file = file, append = apen[i])
        }))
    }
}
