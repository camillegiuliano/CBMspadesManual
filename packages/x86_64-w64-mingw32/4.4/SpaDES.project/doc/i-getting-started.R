## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

# Most are eval=FALSE within each chunk; but a few are not. Those must be eval=FALSE on GA and CRAN
SuggestedPkgsNeeded <- c("SpaDES.core", "igraph", "visNetwork", "ellipsis", "terra")
hasSuggests <- all(sapply(SuggestedPkgsNeeded, requireNamespace, quietly = TRUE))
useSuggests <- !(tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_")) == "true")

# Keep eval = TRUE on for emcintir because these are also tests
knitr::opts_chunk$set(eval = hasSuggests && useSuggests &&
                       Sys.info()["user"] == "emcintir")# && interactive())
origGetWd <- getwd()
origWorkingDir <- fs::path_real(ifelse(basename(getwd()) == "vignettes", "..", "."))
knitr::opts_knit$set(root.dir = origWorkingDir)
knitr::opts_chunk$set(message=FALSE)

options(Ncpus = 1,
        SpaDES.project.updateRprofile = FALSE)
#        SpaDES.project.projectPath = Require::tempdir2(paste0("SpaDES.project_" , paste0(sample(LETTERS, 8), collapse = ""))))

# options(Require.clonePkgs = TRUE)
origLibPath = .libPaths()[1]
# knitr::opts_knit$set(root.dir = Require::tempdir2(paste0("getting_started_", .rndstr(1))))


## ----setCRAN,echo=FALSE-------------------------------------------------------
#  # cleanup / restore state
#  if (is.null(getOption("repos")) || !"CRAN" %in% names(getOption("repos"))) {
#   options(repos = c(CRAN = "https://cloud.r-project.org"))
#  }

## ----install-spades-project---------------------------------------------------
#  if (!require("SpaDES.project")) {
#    {install.packages("SpaDES.project", repos = c("predictiveecology.r-universe.dev", getOption("repos")))
#     require("SpaDES.project")}
#  }

## ----versatile,message=FALSE--------------------------------------------------
#  out <- setupProject(
#    options = list(reproducible.useTerra = TRUE,
#                   "inst/options.R",
#                   "PredictiveEcology/SpaDES.project@transition/inst/options.R"
#    )
#  )
#  attr(out, "projectOptions")

## ----resetLibPaths1,echo=FALSE------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----echo=FALSE---------------------------------------------------------------
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "here"), create = TRUE ))

## ----sequentialPaths----------------------------------------------------------
#  lalahere <- getwd()
#  # 'dataPath' is defined based on the previous argument 'outputPath' in the same function call.
#  setupPaths(paths = list(projectPath = "here",
#                          outputPath = "outs",
#                          dataPath = file.path(paths[["outputPath"]], "data")))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----echo=FALSE---------------------------------------------------------------
#  knitr::opts_knit$set(root.dir = Require::checkPath(origWorkingDir, create = TRUE ))

## ----sequentialOptions--------------------------------------------------------
#  save(origWorkingDir, lalahere, origLibPath, origGetWd, file = "/home/emcintir/getwd.rda")
#  a <- setupOptions(options = list(
#      "inst/options.R",                     # many items set because this is a file; has "future.globals.maxSize" = 5.24288e8
#      future.globals.maxSize = 4e8,         # this value overrides first, because it is 2nd
#      if(user("emcintir"))                  # an "if" statement is OK --> but must use list after
#        list(future.globals.maxSize = 6e8)  # conditional on user, value is overridden again
#  ))

## ----echo=FALSE---------------------------------------------------------------
#  .libPaths(origLibPath)

## ----missingVals,eval=TRUE----------------------------------------------------
.nodes <- 2
out <- SpaDES.project::setupProject(.mode = .mode,
                                    .nodes = .nodes)
out[c(".mode", ".nodes")]

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example2-----------------------------------------------------------------
#  out <- setupProject(
#    modules = "PredictiveEcology/Biomass_borealDataPrep@development",
#    packages = NULL # for this example, skip installation of packages; normally don't do this
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----simplest2,eval=TRUE------------------------------------------------------
library(SpaDES.project)
setupProject(paths = list(modulePath = "myModules"))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----simplest,eval=TRUE-------------------------------------------------------
library(SpaDES.project)
setupProject()

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----sequential,eval=TRUE-----------------------------------------------------
# Use the newValue in `secondValue`
out <- setupProject(
             newValue = 1,
             secondValue = newValue + 1)
out[c("newValue", "secondValue")]


## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----Global-------------------------------------------------------------------
#  # Gets the wrong version of the module
#  githubRepo = "PredictiveEcology/Biomass_core@development" # incorrect one -- used in error
#  out <- setupProject(
#               gitHubRepo = "PredictiveEcology/Biomass_core@main", # The correct one --> not used
#               modules = githubRepo,
#               packages = NULL # no packages for this demonstration
#  )
#  out$modules # shows @development, which is unexpected
#  
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----cleanGlobal,echo=FALSE---------------------------------------------------
#  # cleanup / restore state
#  unlink("modules", recursive = TRUE)
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_3"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example3,error=FALSE-----------------------------------------------------
#  out <- setupProject(
#    options = list(reproducible.useTerra = TRUE),
#    params = list(Biomass_borealDataPrep = list(.plots = "screen")),
#    paths = list(modulePath = "m",
#                 projectPath = "."), # within vignette, project dir cannot be changed; user likely will
#    modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----clean3,echo=FALSE--------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_4"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example_4,error=FALSE----------------------------------------------------
#  out <- setupProject(
#    options = c(
#      "PredictiveEcology/SpaDES.project@transition/inst/options.R"
#    ),
#    params = list(
#      Biomass_borealDataPrep = list(.plots = "screen")
#    ),
#    paths = list(modulePath = "m",
#                 projectPath = "."), # within vignette, project dir cannot be changed; user likely will
#    modules = "PredictiveEcology/Biomass_borealDataPrep@development"
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----clean4,echo=FALSE--------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_5"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example_5,error=FALSE----------------------------------------------------
#  out <- setupProject(
#    name = "example_5", # puts in a folder with this name
#    modules = "PredictiveEcology/Biomass_borealDataPrep@development",
#    sideEffects = "PredictiveEcology/SpaDES.project@transition/inst/sideEffects.R",
#  
#    # if mode and studyAreaName are not available in the .GlobalEnv, then will use these
#    defaultDots = list(mode = "development",
#                       studyAreaName = "MB"),
#    mode = mode, # may not exist in the .GlobalEnv, so `setup*` will use the defaultDots above
#    studyAreaName = studyAreaName#, # same as previous argument.
#    # params = list("Biomass_borealDataPrep" = list(.useCache = mode))
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----clean5,echo=FALSE--------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_7"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example7,error=FALSE-----------------------------------------------------
#  studyAreaName <- "AB"
#  out <- setupProject(
#    paths = list(projectPath = "example_7"),
#    modules = "PredictiveEcology/Biomass_borealDataPrep@development",
#    defaultDots = list(mode = "development",
#                       studyAreaName = "MB"),
#    mode = "development",
#    studyAreaName = studyAreaName  # <----- pass it here, naming it
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----clean7,echo=FALSE--------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_8"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example8-----------------------------------------------------------------
#  out <- setupProject(
#    options = list(
#      reproducible.useTerra = TRUE,                                 # direct argument # sets one option
#      "PredictiveEcology/SpaDES.project@transition/inst/options.R", # remote file -- sets many options
#      system.file("authentication.R", package = "SpaDES.project")   # local file -- sets gargle options
#    )
#  )

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example9,eval=FALSE------------------------------------------------------
#  out <- setupStudyArea(list("Al|Brit", level = 2))
#  
#  if (interactive()) {
#    library(quickPlot)
#    dev() # open a non-RStudio window; can't use RStudio for `clickValues` below
#    Plot(out)
#    val <- clickValues()
#    out <- setupStudyArea(list("Al|Brit", level = 2, NAME_2 = val$NAME_2))
#    Plot(out, new = TRUE) # need "new = TRUE" to clear previous map
#  }
#  
#  # Can pick multiple parts with partial matching
#  out <- setupStudyArea(list("Al|Brit", level = 2, NAME_2 = "19|18|17|Peace"))
#  

## ----clean9,echo=FALSE--------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_10"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----explanationForExample10,eval=FALSE,echo=FALSE----------------------------
#  # example_10 doesn't run behind some firewalls, so fails too frequently; skip evaluation in vignette

## ----example10,eval=FALSE-----------------------------------------------------
#  out <- setupProject(
#    name = "example_10",
#    studyArea = list("Al|Brit|Sas", level = 2, epsg = "3005")
#  )

## ----clean10,echo=FALSE-------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_11"), create = TRUE ))

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----example11,eval=FALSE-----------------------------------------------------
#  out <- setupProject(
#    paths = list(projectPath = "example_11"), # will deduce name of project from projectPath
#    standAlone = TRUE,
#    require = c(
#      "PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
#      "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"
#    ),
#    modules = c(
#      "PredictiveEcology/Biomass_speciesData@master",
#      "PredictiveEcology/Biomass_borealDataPrep@development",
#      "PredictiveEcology/Biomass_core@master",
#      "PredictiveEcology/Biomass_validationKNN@master",
#      "PredictiveEcology/Biomass_speciesParameters@development"
#    ),
#    objects = list(
#      studyAreaLarge = terra::vect(
#        terra::ext(-598722, -557858,
#                   776827, 837385),
#        crs = terra::crs("epsg:3978")
#      ),
#      studyArea = terra::vect(
#        terra::ext(-598722, -578177,
#                   779252, 809573),
#        crs = terra::crs("epsg:3978")
#      )
#    )
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----echo=FALSE---------------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_12"), create = TRUE ))

## ----example12,eval=FALSE-----------------------------------------------------
#  out <- setupProject(
#    paths = list(projectPath = "example_12"), # will deduce name of project from projectPath
#    standAlone = TRUE,
#    require = c(
#      "PredictiveEcology/reproducible@development (>= 1.2.16.9017)",
#      "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9001)"
#    ),
#    modules = c(
#      "PredictiveEcology/Biomass_speciesData@master",
#      "PredictiveEcology/Biomass_borealDataPrep@development",
#      "PredictiveEcology/Biomass_core@master",
#      "PredictiveEcology/Biomass_validationKNN@master",
#      "PredictiveEcology/Biomass_speciesParameters@development"
#    ),
#    studyAreaLarge = terra::vect(
#      terra::ext(-598722, -557858, 776827, 837385),
#      crs = terra::crs("epsg:3978")
#    ),
#    studyArea = terra::vect(
#      terra::ext(-598722, -578177, 779252, 809573),
#      crs = terra::crs("epsg:3978")
#    )
#  )
#  # Initiate a simInit and spades call:
#  # do.call(SpaDES.core::simInitAndSpades, out)

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----echo=FALSE---------------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "example_13"), create = TRUE ))

## ----example13,eval=FALSE-----------------------------------------------------
#  out <- setupProject(
#    name = "example_13",
#    packages = "terra",
#    updateRprofile = TRUE
#  )

## ----echo=FALSE---------------------------------------------------------------
#  Require::setLibPaths(origLibPath, updateRprofile = FALSE)

## ----echo=FALSE---------------------------------------------------------------
#  # cleanup / restore state
#  knitr::opts_knit$set(root.dir = origGetWd)

## ----cleanup, include = FALSE, eval=TRUE--------------------------------------
Require::setLibPaths(origLibPath, updateRprofile = FALSE)
for (fold in c("cache", "modules", "inputs", "outputs", "m", "here", "myModules"))
  unlink(fold, recursive = TRUE)


