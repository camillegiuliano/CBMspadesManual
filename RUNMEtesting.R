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
    packageVersion("Require", lib.loc = pkgPath) == "1.0.1") {
  install.packages("Require")
}

Require::Require(c("bookdown", "ROpenSci/bibtex", "data.table", "downlit",
                   "formatR", "git2r", "kableExtra", "yihui/knitr", 
                   "fansi", "xml2", "vctrs"))

library(devtools)
install_github("PredictiveEcology/SpaDES.docs@development")


########
# CERES SCRIPT MAY NOT WORK
repos <- c("predictiveecology.r-universe.dev", getOption("repos"))
install.packages(c("Require", "SpaDES.project"), repos = repos)
##remove the yml header
.copyModuleRmds <- SpaDES.docs::prepManualRmds("modules", rebuildCache = FALSE)

if (!file.exists("docs/.nojekyll")) {
  file.create("docs/.nojekyll")
}

# set manual version
Sys.setenv(SpadesCBM_MAN_VERSION = "0.1") ## update this for each new release
  # this added as a subtitle in the yml header

#RENDERING
render_book(output_format = "all", envir = new.env())
