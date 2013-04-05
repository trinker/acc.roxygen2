#' Expand staticdocs index.html File
#'
#' staticdocs writes multiple files contained in an .Rd file to one icon 
#' followed by a parenthesis with the other functions.  This function exapands 
#' the parenthesis to be separate icons of their own.
#' 
#' @param path Path to the index.html file.
#' @param file The path/file name to output.  If NULL the original index is 
#' overwritten.
#' @param to.icon The extra functions found in the parenthesis for who new icons 
#' should be added.
#' @param readme Path to the readme file.
#' @param combine A list of vectors to combine together.  The functions will be 
#' combined int he order of each vector.
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
#' expand_statdoc(path, file, drop = qcv(syn, mgsub, adjmat, wc))
#' 
#' file2 <- paste0(getwd(), "/qdap/index3.html")
#' extras <- qcv(right.just, coleman_liau, flesch_kincaid, fry, linsear_write, SMOG)
#' expand_statdoc(path, file2, to.icon = extras, drop = qcv(syn, mgsub, adjmat, wc))
#' 
#' file3 <- paste0(getwd(), "/qdap/index4.html")
#' rdme <- system.file("extdata/readme.R", package = "acc.roxygen2")
#' expand_statdoc(path, file3, to.icon = extras, 
#'     readme = rdme, combine = qcv(character.table, char.table),  
#'     drop = qcv(syn, mgsub, adjmat, wc))
#}
expand_statdoc <-
function(path, file = NULL, to.icon = NULL, readme = NULL,
    combine = NULL, drop = NULL, rm.other = TRUE) {
      if (is.null(to.icon) & is.null(readme)) {
        x <- suppressWarnings(readLines(path))
    } else { 
    	if (!is.null(to.icon)) {
            x <- exbutton_statdoc(path, to.icon)
            if (!is.null(readme)) {
                x <- readme_statdoc(x, readme)
            }
    	} else {
    	    if (!is.null(readme)) {
    	    	x <- suppressWarnings(readLines(path))
                x <- readme_statdoc(x, readme)
            }	
    	}
    }
    pars <- which(grepl("a></code>\\((.+?)\\)<br", x))
    partxt <- x[pars]
    new <- gsub("a></code>\\((.+?)\\)<br", "a></code><br", x)
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
    if (!is.null(combine)) {
        new <- combo_statdoc(new, combine = combine)
    }    
    cat(paste(new, collapse="\n"), file = file)
}