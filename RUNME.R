
## this manual must be knitted by running this script

prjDir <- SpaDES.project::findProjectPath()

Require::Require(c("yaml", "purrr", "fs"))

docsDir <- file.path(prjDir, "_bookdown.yml") |>
  yaml::read_yaml() |>
  purrr::pluck("output_dir") |>
  fs::path_abs()

pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

Require::Require(c("bookdown", "ROpenSci/bibtex", "data.table", "downlit",
                   "formatR", "git2r", "kableExtra", "yihui/knitr", 
                   "fansi", "xml2", "vctrs", "RefManageR", "remotes"))
remotes::install_github("PredictiveEcology/SpaDES.docs@development")


# bibDir <- Require::checkPath(file.path(prjDir, "citations"), create = TRUE)
# figDir <- Require::checkPath(file.path(docsDir, "figures"), create = TRUE)

options(
  Ncpus = min(parallel::detectCores() / 2, 8)
)

# load packages -------------------------------------

library(bibtex)
library(bookdown)
library(data.table)
library(knitr)
library(RefManageR)
library(SpaDES.docs)
library(formatR)

## references ---------------------------------------

## automatically create a bib database for R packages
# allPkgs <- .packages(all.available = TRUE, lib.loc = .libPaths()[1])
# keyPkgs <- c(
#   "bookdown", "knitr", "LandR", "LandWebUtils",
#   "reproducible", "rmarkdown", "shiny", "SpaDES.core", "SpaDES.tools"
# )
# write_bib(allPkgs, file.path(bibDir, "packages.bib")) ## TODO: using allPkgs, not all pkgs have dates/years
# 
# ## collapse all chapter .bib files into one ------
# bibFiles <- c(
#   list.files(file.path(prjDir, "m"), "references_", recursive = TRUE, full.names = TRUE),
#   file.path(bibDir, "packages.bib"),
#   file.path(bibDir, "references.bib")
# )
# bibdata <- lapply(bibFiles, function(f) {
#   if (file.exists(f)) RefManageR::ReadBib(f)
# })
# bibdata <- Reduce(merge, bibdata)
# 
# WriteBib(bibdata, file = file.path(bibDir, "references.bib"))
# 
# csl <- file.path(bibDir, "ecology-letters.csl")
# if (!file.exists(csl)) {
#   download.file("https://www.zotero.org/styles/ecology-letters?source=1", destfile = csl)
# }

## RENDER BOOK ------------------------------------------

setwd(normalizePath(prjDir))

## prevents GitHub from rendering book using Jekyll
if (!file.exists(file.path(prjDir, ".nojekyll"))) {
  file.create(file.path(prjDir, ".nojekyll"))
}

## set manual version
Sys.setenv(SpadesCBM_MAN_VERSION = "0.1") ## version
Sys.getenv("SpadesCBM_MAN_VERSION")

## don't use Require for package installation etc.
Sys.setenv(R_USE_REQUIRE = "false")
Sys.getenv("R_USE_REQUIRE")

## NOTE: need dot because knitting is doing `rm(list = ls())`
.copyModuleRmds <- prepManualRmds("modules", rebuildCache = FALSE) ## use rel path!

## render the book using new env -- see <https://stackoverflow.com/a/46083308>
bookdown::render_book(output_format = "all", envir = new.env())


## remove temporary .Rmds
file.remove(.copyModuleRmds)
setwd(prjDir)
