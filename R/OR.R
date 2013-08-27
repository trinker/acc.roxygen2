#' Open a Repo
#'
#' Open a repo.
#' 
#' @param repo Repo name.
#' @param base.git Base path to where repos are store (the repo is stored).
#' @importFrom tools file_path_sans_ext
#' @export
OR <- function(repo = getOption("primary_repo"), 
    base.git = getOption("base_git")) {
    open_project(file.path(base.git, repo, paste0(file_path_sans_ext(repo), 
        ".Rproj")))
}
