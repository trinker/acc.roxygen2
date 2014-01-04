#' Make .Rd files
#'
#' Makes and optionally opens the help manual for a roxygen2 repo.
#' 
#' @param repo Repo name.
#' @param base.git Base path to where repos are store (the repo is stored).
#' @param rox logical.  If \code{TRUE} makes the .Rd documentation from the .R 
#' documentation.
#' @param open logical.  If \code{TRUE}  makes a pdf of the manual and opens it.
#' @export
#' @import devtools
rmd <- 
function(repo = getOption("primary_repo"), base.git = getOption("base_git"), 
    rox = TRUE, open = TRUE){
    pack <- as.character(substitute(repo))
    unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)
    x <- paste0(base.git, pack)
    if (rox) {
        document(x)
    }
    install(x)
    if (rox) {
        path <- find.package(pack)
        system(paste(shQuote(file.path(R.home("bin"), 
            "R")), "CMD", "Rd2pdf", shQuote(path)))
        if (open && Sys.info()["sysname"] == "Windows") {
            shell.exec(paste0(pack, ".pdf"))
        }
    }
}
