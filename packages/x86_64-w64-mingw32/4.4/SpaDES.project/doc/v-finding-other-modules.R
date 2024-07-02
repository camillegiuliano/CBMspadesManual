## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)

SuggestedPkgsNeeded <- c("SpaDES.core", "igraph", "visNetwork", "ellipsis", "terra", "raster")
hasSuggests <- all(sapply(SuggestedPkgsNeeded, require, character.only = TRUE, quietly = TRUE))
useSuggests <- !(tolower(Sys.getenv("_R_CHECK_DEPENDS_ONLY_")) == "true")

scalls <- sys.calls()
a <- format(scalls)

# Run everything during `pkgdown::build_*` (which uses rmarkdown::render)
knitr::opts_chunk$set(eval = hasSuggests && useSuggests &&
                        Sys.info()["user"] == "emcintir" && 
  any(grepl("rmarkdown::render", a)))# && interactive())

options(Ncpus = 1)
if (Sys.info()["user"] == "emcintir") {
  knitr::opts_knit$set(root.dir = Require::checkPath(file.path("c:/Eliot/tmp", "listModules"), create = TRUE ))
} else { 
  knitr::opts_knit$set(root.dir = Require::checkPath(file.path(tempdir(), "listModules"), create = TRUE ))
}

knitr::opts_chunk$set(fig.width=9, fig.height=7) # sets html figure height and width

# options(Require.clonePkgs = TRUE)


## ----setCRAN,echo=FALSE-------------------------------------------------------
#  ## cleanup / restore state
#  if (is.null(getOption("repos")) || !"CRAN" %in% names(getOption("repos"))) {
#    options(repos = c(CRAN = "https://cloud.r-project.org"))
#  }

## ----install-spades-project---------------------------------------------------
#  if (!require("SpaDES.project")) {
#    {install.packages("SpaDES.project", repos = c("predictiveecology.r-universe.dev", getOption("repos")))
#     require("SpaDES.project")}
#  }

## ----clean13,echo=FALSE-------------------------------------------------------
#  ## cleanup / restore state
#  # SpaDES.project::.teardownProject(out$paths, origLibPaths)

## ----listModulesSmall,result='hide',message=FALSE-----------------------------
#  Account <- "PredictiveEcology"
#  grepListShort <- "Biomass_species|Biomass_core|Biomass_regen"
#  mods <- listModules(grepListShort, accounts = Account)
#  
#  modPath <- normPath(tempdir2())
#  getModule(mods, modulePath = modPath)
#  
#  DT <- moduleDependencies(mods, modulePath = modPath)
#  graph <- moduleDependenciesToGraph(DT)
#  (vn <- PlotModuleGraph(graph))

## ----FireSense,result='hide',message=FALSE------------------------------------
#  Account <- "PredictiveEcology"
#  grepListShort <- "fireSense"
#  mods <- listModules(grepListShort, accounts = Account)
#  
#  getModule(mods, modulePath = modPath)
#  
#  DT <- moduleDependencies(mods, modulePath = modPath)
#  graph <- moduleDependenciesToGraph(DT)
#  (vn <- PlotModuleGraph(graph))

## ----LandR,result='hide',message=FALSE----------------------------------------
#  Account <- "PredictiveEcology"
#  grepListShort <- "Biomass_|LandR"
#  mods <- listModules(grepListShort, accounts = Account)
#  
#  getModule(mods, modulePath = modPath)
#  
#  DT <- moduleDependencies(mods, modulePath = modPath)
#  graph <- moduleDependenciesToGraph(DT)
#  (vn <- PlotModuleGraph(graph))

## ----listModulesAll,result='hide',message=FALSE,error=FALSE,warning=FALSE-----
#  # Can do same, but with long list -- not done here -- can try
#  accountsListLong <- c("PredictiveEcology", "ianmseddy", "achubaty",
#                         "FOR-CAST", "eliotmcintire", "tati-micheletti", "CeresBarros")
#  grepListLong <- c("Biomass", "WBI", "LandR", "fireSense", "CBM",
#                    "LandMine", "LandWeb", "NRV", #"scfm",
#                    "priority", "fire",
#                    "dataPrep", "DataPrep", "RoF", "Ontario", "ROF")
#  modsLong <- listModules(grepListLong, accounts = accountsListLong)
#  
#  # pass to listModules for much larger figure
#  getModule(modsLong, modulePath = modPath)
#  
#  DT <- moduleDependencies(modsLong, modulePath = modPath)
#  graph <- moduleDependenciesToGraph(DT)
#  (vn <- PlotModuleGraph(graph))
#  
#  

## ----listModulesReallyAll,result='hide',message=FALSE,error=FALSE,warning=FALSE,fig.height=12----
#  # Can do same, but with long list -- not done here -- can try
#  accountsListLong <- c("PredictiveEcology", "ianmseddy", "achubaty",
#                         "FOR-CAST", "eliotmcintire", "tati-micheletti", "CeresBarros")
#  modsLong <- listModules(accounts = accountsListLong)
#  
#  # pass to listModules for much larger figure
#  getModule(modsLong, modulePath = modPath)
#  
#  DT <- moduleDependencies(modsLong, modulePath = modPath)
#  graph <- moduleDependenciesToGraph(DT)
#  (vn <- PlotModuleGraph(graph))
#  
#  

