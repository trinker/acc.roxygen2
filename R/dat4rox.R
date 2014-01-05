#' roxygen2 Format Data Sets
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
        is.enviroment <- function(x) class(x) == "environment"
        x <- "#'"
        what <- function(x) {
            if (is.data.frame(x)) {
                return("data frame")
            }
            if (is.list(x) & !is.data.frame(x)) {
                return("list")
            }
            if (is.vector(x)) {
                return("vector")
            }
            if (class(x) == "character") {
                return("character vector")
            }            
            if (is.environment(x)) {
                return("environment")
            }
        }
        type <- what(dat)
        if (type == "environment") {
            desc <- "#' A dataset containing an environment"
        } else {
            if (type == "data frame") {
                desc <- "#' A dataset containing"
            } else {
                if (type %in% c("character vector", "vector", "list")) {
                    desc <- paste("#' A dataset containing a", type)
                }
            }
        }
        if (is.data.frame(dat)) {
        	dets <- c("#' \\itemize{", paste("#'   \\item ", colnames(dat), ".", 
        	    sep = ""), "#' }")
        } else {
            if (is.vector(dat) | is.enviroment(dat) | class(dat) == "character") {
            	dets <- x
            } else {
                if (!is.data.frame(dat) && is.list(dat)) {
        	        dets <- c("#' \\describe{", paste("#'   \\item{", 
                        names(dat), "}{}", sep = ""), "#' }")
                }
            }
        }
        if (type == "data frame") {
            elems <- c(nrow(dat), "rows and", ncol(dat), "variables")
        } else {
            if (type %in% c("character vector", "vector")) {
                elems <- c(length(dat), "elements")
            } else {
                if (type == "list") {
                    elems <- c(length(dat), "elements")
                } else {
                    if (type == "environment") {
                        elems <- NULL
                    }
                }
            }    
        }
        out <- c("#'", x, desc, x, "#' @details",
            dets, x, "#' @docType data", "#' @keywords datasets",
            paste("#' @name", name), paste0("#' @usage data(", name, ")"),
            paste("#' @format A", type, "with", paste(elems, collapse = " ")), 
            "#' @references", "NULL\n")
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

