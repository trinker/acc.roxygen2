#' @importFrom qdap Trim
#' @importFrom reports folder delete

#' \dontrun{
#' package_template(mean, mtcars, CO2, lm)
#' }
package_template <- function(..., name = "anRpackage",  
    environment = .GlobalEnv, path = ".", force = FALSE, 
    code_files = character(), list = NULL, open = TRUE, 
    rproj = open) {

    if (is.null(list)) {
        x <- substitute(...())
        list <- Trim(unlist(lapply(x, function(y) as.character(y))))
    }

    pfuns <- sapply(list, function(x) is.function(get(x, envir = environment)))

    ## Use package skeleton to do the initial work
    suppressMessages(package.skeleton(name = name, list = list, 
        environment = environment, path = path, force = force, 
        code_files = code_files))

    ## Enhance DESCRIPTION
    descloc <- file.path(path, name, "DESCRIPTION")
    desc1 <- paste(readLines(descloc), 
        collapse="\n")
    desc2 <- readLines(system.file("extdata/lib/desc.txt", package = "acc.roxygen2"))
    cat(paste0(desc1, "\n", gsub("\\\\n", "\\\n", desc2)), file = descloc)

    ## Create .Rprofile
    source(system.file("extdata/lib/rprof.txt", package = "acc.roxygen2"))
    cat(gsub("REPLACE_ME", name, rprof), 
        file=file.path(path, name, ".Rprofile"))

    ## Create the .Rproj
    if (rproj) {
        invisible(file.copy(system.file("extdata/lib/TEMP.txt", 
            package = "acc.roxygen2"), file.path(path, name)))
        invisible(file.rename(file.path(name, "TEMP.txt"), 
            file.path(name, paste0(name, ".Rproj"))))
    }

    ## Enhance directories
    delete(file.path(path, name, "Read-and-delete-me"))
    delete(file.path(path, name, "R"))  
    delete(file.path(path, name, "man"))
    dirs <- invisible(folder(folder.name = file.path(path, name, 
        c("R", "man", "inst"))))

    ## Create .R files
    funs4rox(rdir = dirs[[1]], funs = names(pfuns)[pfuns])

    ## Create package .R file (data entries)
    dat4rox2(names(pfuns)[!pfuns], environment = environment,
        file = file.path(path, name, "R", paste0(name, "-package.R")))

    ## Create NEWS file
    news <- readLines(system.file("extdata/lib/news.txt", 
        package = "acc.roxygen2"))
    news <- paste(news, collapse="\n")
    cat(gsub("REPLACE_ME", name, news), file=file.path(path, name, "NEWS"))

    ## Create README.md file
    source(system.file("extdata/lib/readme.txt", package = "acc.roxygen2"))
    cat(gsub("REPLACE_ME", name, readme), file=file.path(path, name, "README.md"))

    ## Create CITATION
    source(system.file("extdata/lib/cite.txt", package = "acc.roxygen2"))
    cat(gsub("REPLACE_ME", name, cite), file=file.path(dirs[[3]], "CITATION"))

    o <- paste0("\"", name, "\" created:\n", file.path(path, name), "\n")
    class(o) <- "reports"

    ## Open file
    if (open) {
        open_project(file.path(path, name, paste0(name, ".Rproj")))
    }
    return(o)

}


dat4rox2 <- function (funs, file = NULL, append = FALSE, 
    environment = environment) {

    dat.sets <- funs
    dat.list <- invisible(lapply(dat.sets, get, envir = environment))
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
            }
            else {
                if (type %in% c("character vector", "vector", 
                  "list")) {
                  desc <- paste("#' A dataset containing a", 
                    type)
                }
            }
        }
        if (is.data.frame(dat)) {
            dets <- c("#' \\itemize{", paste("#'   \\item ", 
                colnames(dat), ".", sep = ""), "#' }")
        } else {
            if (is.vector(dat) | is.enviroment(dat) | class(dat) == 
                "character") {
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
        out <- c("#'", x, desc, x, "#' @details", dets, x, "#' @docType data", 
            "#' @keywords datasets", paste("#' @name", name), 
            paste0("#' @usage data(", name, ")"), paste("#' @format A", 
                type, "with", paste(elems, collapse = " ")), 
            "#' @references", "NULL\n")
        cat(paste(out, "\n", collapse = ""), file = file, append = append)
    }
    if (!is.null(file)) {
        apen <- rep(TRUE, length(dat.list))
        if (!append) {
            apen[1] <- FALSE
        }
        invisible(lapply(seq_along(dat.list), function(i) {
            dat.file(dat.list[[i]], names(dat.list)[i], file = file, 
                append = apen[i])
        }))
    }
}


wheresRstudio <- function() {
    myPaths <- c("rstudio",  "~/.cabal/bin/rstudio", 
        "~/Library/Haskell/bin/rstudio", "C:\\PROGRA~1\\RStudio\\bin\\rstudio.exe",
        "C:\\RStudio\\bin\\rstudio.exe", "/Applications/RStudio.app/Contents/MacOS/RStudio")
    panloc <- Sys.which(myPaths)
    temp <- panloc[panloc != ""]
    if (identical(names(temp), character(0))) {
        ans <- readline("RStudio not installed in one of the typical locations.\n 
            Do you know where RStudio is installed? (y/n) ")
        if (ans == "y") {
                temp <- readline("Enter the (unquoted) path to RStudio: ")
        } else {
            if (ans == "n") {
                stop("RStudio not installed or not found.")
            }
        }
    } 
    short.path <- which.min(unlist(lapply(gregexpr("RStudio", temp), "[[", 1)))
    temp[short.path] 
}

open_project <- function(Rproj.loc) {
    action <- paste(wheresRstudio(), Rproj.loc)
    message("Preparing to open project!")
    try(system(action, wait = FALSE, ignore.stderr = TRUE))
}

