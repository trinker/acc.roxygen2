#========================
# Delete manuals Rd files
#========================
mans <- file.path(getwd(), "man")
delete(file.path(mans, dir(mans)))
#========================
#staticdocs dev version
#========================
#packages
# library(devtools); install_github("qdap", "trinker"); install_github("staticdocs", "hadley")
# install_github("acc.roxygen2", "trinker")
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_package(package="C:/Users/trinker/GitHub/acc.roxygen2", 
    base_path="C:/Users/trinker/Desktop/acc.roxygen2_dev/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/acc.roxygen2_dev"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/acc.roxygen2/inst/extra_statdoc/readme.R"
extras <- qcv(seealso)
#expand_statdoc(path2, to.icon = extras, readme = rdme, 
#    combine = qcv(character_table, char_table))

#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "acc.roxygen2_dev"))
file.copy(path, file, TRUE, TRUE)
delete(path)
#==========================
#staticdocs current version
#==========================
#packages
library(highlight); library(qdap); library(staticdocs); library(acc.roxygen2)

#STEP 1: create static doc  
#right now examples are FALSE in the future this will be true
#in the future qdap2 will be the go to source
build_package(package="C:/Users/trinker/GitHub/acc.roxygen2", 
    base_path="C:/Users/trinker/Desktop/acc.roxygen2/", examples = FALSE)

#STEP 2: reshape index
path <- "C:/Users/trinker/Desktop/acc.roxygen2"
path2 <- paste0(path, "/index.html")
rdme <- "C:/Users/trinker/GitHub/acc.roxygen2/inst/extra_statdoc/readme.R"
extras <- qcv(seealso)
#expand_statdoc(path2, to.icon = extras, readme = rdme, 
#    combine = qcv(character_table, char_table))


#STEP 3: move to trinker.guthub
library(reports)
file <- "C:/Users/trinker/GitHub/trinker.github.com/"
delete(paste0(file, "acc.roxygen2"))
file.copy(path, file, TRUE, TRUE)
delete(path)

#==========================
#move project directions
#==========================

#==========================
#Check spelling
#==========================
path <- file.path(getwd(), "R")
txt <- suppressWarnings(lapply(file.path(path, dir(path)), readLines))
txt <- lapply(txt, function(x) x[substring(x, 1, 2) == "#'"])
new <- lapply(1:length(txt), function(i){
    c("\n", dir(path)[i], "=========", txt[[i]])
})
out <- paste(unlist(new), collapse="\n")
cat(out, file=file.path(path.expand("C:/Users/trinker/Desktop"), "spelling.doc"))

#==========================
#Get Examples to run
#==========================
library(acc.roxygen2)
examples(path = "C:/Users/trinker/GitHub/qdap/R/")

#==========================
# NEWS.md
#==========================
update_news()


#==========================
# NEWS new version
#==========================
x <- c("BUG FIXES", "NEW FEATURES", "MINOR FEATURES", "IMPROVEMENTS", "CHANGES")
cat(paste(x, collapse = "\n\n"), file="clipboard")

#==============================
# Copy from Current R to R_dev
#==============================
r2dev()



