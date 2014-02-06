#' Generate Text of Roxygen Documentation
#'
#' Generates text of roxygen documentation for quick spell check.
#' 
#' @param repo Character string indicating repo/package name.
#' @param out_path The desired file out path.
#' @param base.git Path the location of where git repos are stored.
#' @export
doc_check <- function(repo, out_path="spell_check.doc",
	base.git = getOption("base.git")) {
    in_path <- file.path (base.git, repo, "R")
    files <- file.path(in_path, dir(in_path))
    m <- suppressWarnings(lapply(files, readLines))
    names(m) <- dir(in_path)
    x <- do.call(rbind, lapply(seq_along(m), function(i){
        data.frame(fun=rep(names(m)[i], length(m[[i]])), text=m[[i]],
        stringsAsFactors = FALSE)
    }))
    outs <- x[sapply(gregexpr("#'", x$text, fixed = TRUE), function(x) x[[1]]) == 1, ]
    o1 <- split(outs, outs$fun)
    nms <- sapply(o1, function(x) x[1, 1])
    o2 <- sapply(o1, function(x) x[, 2])
    cat("SPELL CHECK\n==============\n\n", file = out_path, append=FALSE)
    invisible(lapply(seq_along(o2), function(i){
        cat(paste(nms[i], "\n#==================\n"), file = out_path, append=TRUE)
        cat(paste(o2[[i]], collapse="\n"), file = out_path, append=TRUE)
        cat("\n\n", file = out_path, append=TRUE)
    }))
    cat(paste0("spell_check files written to:\n", out_path, "\n"))
}
