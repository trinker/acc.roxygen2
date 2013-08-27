#' Generate File of Package Examples
#'
#' Generates a file of examples to your working directory.
#' 
#' @param file A file name, however, not a path.
#' @param path Path to repo.
#' @export
examples <- 
function(file = "examples.txt", path = "C:/Users/trinker/GitHub/qdap/R/") {
    WD <- getwd()
    setwd(path)
    file <- paste0(WD, "/", file)
    m <- suppressWarnings(sapply(dir(), readLines))
    x <- do.call(rbind, lapply(seq_along(m), function(i){
        data.frame(fun=rep(names(m)[i], length(m[[i]])), text=m[[i]],
        stringsAsFactors = FALSE)
    }))
    starts <- grep("@examples", x$text) + 1
    nonrox <- grep("#'", x$text, invert = TRUE) 
    ends <- sapply(seq_along(starts), function(i) {
        nonrox[starts[i] < nonrox][1]
        }, USE.NAMES = FALSE) - 1
    selects <- ends - starts != 0 
    ends <- ends[selects]
    starts <- starts[selects]
    L1 <- lapply(seq_along(ends), function(i) {
        x[starts[i]:ends[i], ]
    })
    L1 <- lapply(L1, function(x){
        if(grepl("#' \\dontrun{", x[1, 2], fixed = TRUE)) {
            x <- tail(head(x, -1), -1)
        }
        x
    })
    names(L1) <- sapply(L1, function(x) gsub(".R", "", x[1,1], fixed = TRUE))
    
    invisible(lapply(seq_along(L1), function(i){
        examp <- paste(substring(L1[[i]][, 2], 4), collapse="\n")
        cat("#======================\n", file = file, append = (i != 1))
        cat(paste0("#  ", names(L1)[i], "\n"), file = file, append = TRUE)
        cat("#======================\n", file = file, append = TRUE)
        cat(examp, file = file, append = TRUE)
        cat("\n\n", file = file, append = TRUE)
    }))
    setwd(WD)
    cat("Examples written to:\n");cat(paste0(file, "\n"))
}