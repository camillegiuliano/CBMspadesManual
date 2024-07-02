#get packages

if (!"rmarkdown" %in% installed.packages() ||
    packageVersion("rmarkdown") < "2.17") {
  install.packages("rmarkdown", dependencies = TRUE)
}

if (!"knitr" %in% installed.packages() ||
    packageVersion("knitr") < "1.40.4") {
  remotes::install_github("yihui/knitr", dependencies = TRUE)
}

if (!"bookdown" %in% installed.packages() ||
    packageVersion("bookdown") < "0.29") {
  install.packages("bookdown", dependencies = TRUE)
}

if (!"htmlwidgets" %in% installed.packages() ||
    packageVersion("htmlwidgets") < "1.5.4") {
  install.packages("htmlwidgets", dependencies = TRUE)
}

if (!"tinytex" %in% installed.packages() ||
    packageVersion("tinytex") < "0.41") {
  ## version "0.41" is probably not mandatory, but it's the version used as of Oct 27 2022
  tinytex::install_tinytex()
}

pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) ||
    packageVersion("Require", lib.loc = pkgPath) < "0.1.6.9015") {
  remotes::install_github("PredictiveEcology/Require@bfb3ed19231d38362e7324f10435a387e29b6ce1",
                          upgrade = FALSE, force = TRUE)
}

install.packages('bookdown')
install.packages('fansi')
install.packages('vctrs')
install.packages('downlit')
install.packages('xml2')
library(bookdown)
library(fansi)
library(vctrs)
library(downlit)
library(xml2)

########
# CERES SCRIPT MAY NOT WORK
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
install.packages(c("Require", "SpaDES.project"), repos = repos)
##remove the yml header
.copyModuleRmds <- SpaDES.docs::prepManualRmds("modules", rebuildCache = FALSE)
##giving me an error: 
## Error in file(con, "r") : cannot open the connection
## In addition: Warning message:
##  In file(con, "r") : cannot open file '2.Rmd': No such file or directory

##NEW ERROR HERE:
## Error in 1:setupChunkStart : argument of length 0

## set manual version
Sys.setenv(LANDR_MAN_VERSION = "0.1") ## update this for each new release
########

#RENDERING
render_book(output_format = "all", envir = new.env())
<<<<<<< Updated upstream
=======



>>>>>>> Stashed changes
