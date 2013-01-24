#' Make .Rd files
#'
#' Makes and optionally opens the help manual for a roxygen2 repo.
#' 
#' @param repo Repo name.
#' @param base.git Base path to where repos are store (the repo is stored).
#' @param rox logical.  If TRUE makes the .Rd documentation from the .R 
#' documentation.
#' @param open logical.  If TRUE makes a pdf of the manual and opens it.
#' @export
rmd <- 
function(repo = "qdap", base.git = "C:/Users/trinker/GitHub/", 
    rox = TRUE, open = TRUE){
    pack <- as.character(substitute(repo))
    library(devtools)
    unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)
    x <- paste0(base.git, pack)
    if (rox) {
        require(devtools)
        document(x)
    }
    install(x)
    if (rox) {
        path <- find.package(pack)
        system(paste(shQuote(file.path(R.home("bin"), 
            "R")), "CMD", "Rd2pdf", shQuote(path)))
        if (open) {
            shell.exec(paste0(pack, ".pdf"))
        }
    }
}