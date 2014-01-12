#' Make .Rd files
#'
#' Makes and optionally opens the help manual for a roxygen2 repo.
#' 
#' @param repo Repo name.
#' @param base.git Base path to where repos are store (the repo is stored).
#' @param rox logical.  If \code{TRUE} makes the .Rd documentation from the .R 
#' @param open logical.  If ]\code{TRUE} the documentation is opened.
#' @param quick logical. If \code{TRUE} skips docs, multiple-architectures, demos, 
#' and vignettes, to make installation as fast as possible.
#' @param open logical.  If \code{TRUE}  makes a pdf of the manual and opens it.
#' @export
#' @import devtools
rmd <- 
function(repo = getOption("primary_repo"), base.git = getOption("base_git"), 
    rox = TRUE, open = TRUE, quick = TRUE){
    pack <- as.character(substitute(repo))
    unlink(paste0(pack, ".pdf"), recursive = TRUE, force = TRUE)
    x <- file.path(base.git, pack)
    if (rox) {
        document(x)
    }
    install(x, quick = quick, build_vignettes = FALSE, dependencies = TRUE)
    if (rox) {
        path <- find.package(pack)
        system(paste(shQuote(file.path(R.home("bin"), 
            "R")), "CMD", "Rd2pdf", shQuote(path)))
        if (open && Sys.info()["sysname"] == "Windows") {
            shell.exec(paste0(pack, ".pdf"))
        }
    }
}
