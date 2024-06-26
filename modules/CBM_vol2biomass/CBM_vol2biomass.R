defineModule(sim, list(
  name = "CBM_vol2biomass",
  description = paste("A module to prepare the user-provided growth and yield information for use",
                      "in the family of models spadesCBM - CBM-CFS3-like simulation of forest",
                      "carbon in the platform SpaDES. This module takes in user-provided m3/ha",
                      "and meta data for teh growth curves and returns annual increments for",
                      "the aboveground live c-pools."),
  keywords = "",
  authors = c(
    person("Céline", "Boisvenue", email = "celine.boisvenue@nrcan-rncan.gc.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(CBM_vol2biomass = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = deparse(list("README.txt", "CBM_vol2biomass.Rmd")),
  reqdPkgs = list(
    "PredictiveEcology/CBMutils",
    "ggforce", "ggplot2", "ggpubr", "googledrive", "mgcv", "quickPlot", "robustbase"
  ),
  parameters = rbind(
    defineParameter(
      "outputFigurePath", "character", NA, NA, NA,
      paste("Filepath to a directory where output figures will be saved.",
            "If `NA` (the default), will use 'figures/' inside the module directory.")
    ),
    defineParameter(
      ".plotInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first plot event should occur."
    ),
    defineParameter(
      ".plotInterval", "numeric", NA, NA, NA,
      "Describes the simulation time interval between plot events."
    ),
    defineParameter(
      ".saveInitialTime", "numeric", NA, NA, NA,
      "Describes the simulation time at which the first save event should occur."
    ),
    defineParameter(
      ".saveInterval", "numeric", NA, NA, NA,
      "This describes the simulation time interval between save events."
    ),
    defineParameter(
      ".useCache", "logical", TRUE, NA, NA,
      paste(
        "Should this entire module be run with caching activated?",
        "This is generally intended for data-type modules, where stochasticity",
        "and time are not relevant"
      )
    )
  ),
  inputObjects = bindrows(
    # expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    # this are variables in inputed data.tables:SpatialUnitID, EcoBoundaryID, juris_id, ecozone, jur, eco, name, GrowthCurveComponentID, plotsRawCumulativeBiomass, checkInc
    expectsInput(
      objectName = "curveID", objectClass = "character",
      desc = "Vector of column names that together, uniquely define growth curve id",
      sourceURL = NA
    ),
    expectsInput(
      objectName = "table3", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for merchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table3.csv"
    ),
    expectsInput(
      objectName = "table4", objectClass = "data.frame", desc = "Stem wood biomass model parameters for nonmerchantable-sized trees from Boudewyn et al 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table4.csv"
    ),
    expectsInput(
      objectName = "table5", objectClass = "data.frame",
      desc = "Stem wood biomass model parameters for sapling-sized trees from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table5.csv"
    ),
    expectsInput(
      objectName = "table6", objectClass = "data.frame",
      desc = "Proportion model parameters from Boudewyn et al. 2007",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6.csv"
    ),
    expectsInput(
      objectName = "table7", objectClass = "data.frame",
      desc = "Caps on proportion models from Boudewyn et al. 2007.",
      sourceURL = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7.csv"
    ),
    expectsInput(
      objectName = "cbmAdmin", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries,",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://drive.google.com/file/d/1xdQt9JB5KRIw72uaN5m3iOk8e34t9dyz"
    ),
    expectsInput(
      objectName = "gcMeta", objectClass = "data.frame",
      desc = paste("Provides equivalent between provincial boundaries",
                   "CBM-id for provincial boundaries and CBM-spatial unit ids"),
      sourceURL = "https://docs.google.com/spreadsheets/d/1LYnShgd0Q7idNNKX9hHYju4kMDwMSkW5/"
    ),
    expectsInput(
      objectName = "gcMetaFile", objectClass = "character",
      desc = "File name and location for the user provided `gcMeta` data.frame",
      sourceURL = NA
    ),
    expectsInput(
      objectName = "canfi_species", objectClass = "data.frame",
      desc = paste("File containing the possible species in the Boudewyn table.",
                   "Note that if Boudewyn et al. added species, this should be updated.",
                   "Also note that such an update is very unlikely."),
      sourceURL = "https://docs.google.com/spreadsheets/d/1YpJ9MyETyt1LBFO81xTrIdbhjO7GoK3K/"
    ),
    expectsInput(
      objectName = "userGcM3File", objectClass = "character",
      desc = paste("User filename for the files containing:",
                   "`GrowthCurveComponentID`, `Age`, `MerchVolume`.",
                   "Default name `userGcM3`."),
      sourceURL = NA
    ),
    expectsInput(
      objectName = "userGcM3", objectClass = "data.frame",
      desc = paste("User file containing:",
                   "`GrowthCurveComponentID`, `Age`, `MerchVolume`.",
                   "Default name `userGcM3`."),
      sourceURL = "https://drive.google.com/file/d/1u7o2BzPZ2Bo7hNcC8nEctNpDmp7ce84m"
    ),
    expectsInput(
      objectName = "ecozones", objectClass = "data.table",
      desc = paste("Vector, one for each stand, indicating the numeric representation",
                   "of the Canadian ecozones, as used in CBM-CFS3"),
      sourceURL = NA
    ),
    expectsInput(
      objectName = "gcids", objectClass = "data.table",
      desc = "The identification of which growth curves to use on the specific stands provided by the user.",
      sourceURL = NA
    ),
    expectsInput(
      objectName = "spatialUnits", objectClass = "data.table",
      desc = paste("the table linking the spu id, with the `disturbance_matrix_id` and the events.",
                   "The events are the possible raster values from the disturbance rasters of Wulder and White."),
      sourceURL = NA
    )
  ),
  outputObjects = bindrows(
    # createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "volCurves", objectClass = "plot", desc = "Plot of all the growth curve provided by the user"), ## TODO: invalid class
    createsOutput(objectName = "gcMetaAllCols",
                  objectClass = "data.frame",
                  desc = "`gcMeta` as above plus ecozones"),
    createsOutput(objectName = "plotsRawCumulativeBiomass", objectClass = "plot", ## TODO invalid class
                  desc = paste("Plot of cumulative m3/ha curves",
                               "translated into tonnes of carbon/ha, per AG pool, prior to any smoothing")), ## TODO: not used
    createsOutput(objectName = "checkInc", objectClass = "plot", ## TODO invalid class
                  desc = paste("Plot of 1/2 of the increment per AG pool,",
                              "calculated from the smoothed cumulative tonnes c/ha, derived into increments, per AG pool.")), ## TODO: not used
    createsOutput(objectName = "growth_increments", objectClass = "matrix", desc = "Matrix of the 1/2 increment that will be used to create the `gcHash`"),
    createsOutput(objectName = "gcHash", objectClass = "environment",
                  desc = paste("Environment pointing to each gcID, that is itself an environment,",
                               "pointing to each year of growth for all AG pools.Hashed matrix of the 1/2 growth increment.",
                               "This is used in the c++ functions to increment AG pools two times in an annual event (in the CBM_core module."))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.CBM_vol2biomass <- function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "CBM_vol2biomass", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "CBM_vol2biomass", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # plotFun(sim) # uncomment this, replace with object to plot
      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "CBM_vol2biomass", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
      "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'",
      sep = ""
    ))
  )
  return(invisible(sim))
}

## event functions

Init <- function(sim) {
  # user provides userGcM3: incoming cumulative m3/ha
  # plot

  # user provides userGcM3: incoming cumulative m3/ha
  # plot
  # Test for steps of 1 in the yield curves
  ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = "GrowthCurveComponentID"]
  idsWithJumpGT1 <- ageJumps[jumps > 1]$GrowthCurveComponentID
  if (length(idsWithJumpGT1)) {
    missingAboveMin <- sim$userGcM3[, approx(Age, MerchVolume, xout = setdiff(seq(0, max(Age)), Age)),
                                    by = "GrowthCurveComponentID"]
    setnames(missingAboveMin, c("x", "y"), c("Age", "MerchVolume"))
    missingAboveMin <- na.omit(missingAboveMin)
    sim$userGcM3 <- rbindlist(list(sim$userGcM3, missingAboveMin))
    setorderv(sim$userGcM3, c("GrowthCurveComponentID", "Age"))

    # Assertion
    ageJumps <- sim$userGcM3[, list(jumps = unique(diff(as.numeric(Age)))), by = "GrowthCurveComponentID"]
    idsWithJumpGT1 <- ageJumps[jumps > 1]$GrowthCurveComponentID
    if (length(idsWithJumpGT1) > 0)
      stop("There are still yield curves that are not annually resolved")
  }

  sim$volCurves <- ggplot(data = sim$userGcM3, aes(x = Age, y = MerchVolume, group = GrowthCurveComponentID, colour = GrowthCurveComponentID)) +
    geom_line() ## TODO: move to plotInit event
  message("User: please look at the curve you provided via sim$volCurves")
  ## not all curves provided are used in the simulation - and ***FOR NOW*** each
  ## pixels only gets assigned one growth curve (no transition, no change in
  ## productivity).
  ## To run module independently, the gcID used in this translation can be specified here
  # if(!suppliedElsewhere("level3DT",sim)){
  #   userGcM3 <- sim$userGcM3
  # }else{
  userGcM3 <- sim$userGcM3
  spu <- unique(sim$spatialUnits)
  eco <- unique(na.omit(sim$ecozones))

  thisAdmin <- sim$cbmAdmin[sim$cbmAdmin$SpatialUnitID %in% spu & sim$cbmAdmin$EcoBoundaryID %in% eco, ]

  ####userGcM3 <- sim$userGcM3[GrowthCurveComponentID %in% unique(sim$gcids), ]
  # }

  # START reducing Biomass model parameter tables -----------------------------------------------
  # reducing the parameter tables to the jurisdiction or ecozone we have in the study area
  ## To run module independently, the gcID used in this translation can be specified here
  # if(!suppliedElsewhere("spatialUnits",sim)){
  #   spu  <- ### USER TO PROVIDE SPU FOR EACH gcID###########
  # }else{
  ####spu <- unique(sim$spatialUnits)
  # }
  # if(!suppliedElsewhere("ecozones",sim)){
  #   eco <- ### USER TO PROVIDE SPU FOR EACH gcID###########
  # }else{

  ####eco <- unique(sim$ecozones)
  # }
  ####thisAdmin <- sim$cbmAdmin[sim$cbmAdmin$SpatialUnitID %in% spu & sim$cbmAdmin$EcoBoundaryID %in% eco, ]

  # "s" table for small table3, 4, 5, 6, 7 - tables limited to the targeted
  # ecozones and jurisdictions
  stable3 <- as.data.table(sim$table3[sim$table3$juris_id %in% thisAdmin$abreviation &
    sim$table3$ecozone %in% eco, ])
  stable4 <- as.data.table(sim$table4[sim$table4$juris_id %in% thisAdmin$abreviation &
    sim$table4$ecozone %in% eco, ])
  # table5 is different since there was not have enough data to fit models for
  # all provinces.
  # unique(sim$table5$juris_id)
  # [1] "AB" "BC" "NB" "NL" "NT"
  # Here we are hard-coding the closest equivalent province to
  # have a complete set.
  # This first If-statement is to catch the "no-province" match

  stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin$abreviation, ])
  ## DANGER HARD CODED: if NFIS changes table 5, this will no longer be valid
  # juris_id: there are only 5/13 possible
  # these are the provinces available: AB BC NB NF NT
  # for the non match these would be the equivalent
  # "PE" - NB
  # "QC" - NB
  # "ON" - NB
  # "MB" - AB
  # "SK" - AB
  # "YK" - NT
  # "NU" - NT
  abreviation <- c("PE", "QC", "ON", "MB", "SK", "YK", "NU")

  if (any(thisAdmin$abreviation %in% abreviation)) {
    t5abreviation <- c("NB", "NB", "NB", "AB", "AB", "NT", "NT")
    abreviationReplace <- data.table(abreviation, t5abreviation)
    # replace the abbreviations and select
    thisAdmin5 <- merge(abreviationReplace, thisAdmin)
    thisAdmin5[, c("abreviation", "t5abreviation") := list(t5abreviation, NULL)]
    stable5.2 <- as.data.table(sim$table5[sim$table5$juris_id %in% thisAdmin5$abreviation, ])
  }
  # This second "if-statement" is to catch is the "no-ecozone" match
  ### THIS NEEDS TO BE TESTED
  if (nrow(stable5.2) > 0) {
    stable5 <- stable5.2[ecozone %in% eco, ]
  } else {
    stop(
      "There are no matches found for the parameters needed to execute the Boudewyn models.",
      "Please manually find matches for table 5."
    )
  }

  # there are 12/15 ecozones in table5. Once you narrow the table to admin
  # abreviation (which is done above), there might be more mismatch. This
  # solution only works for SK.
  # These are the ones in table5
  # id               name
  # 4       Taiga Plains
  # 5  Taiga Shield West
  # 6 Boreal Shield West
  # 7  Atlantic Maritime
  # 9      Boreal Plains
  # 10  Subhumid Prairies
  # 12  Boreal Cordillera
  # 13   Pacific Maritime
  # 14 Montane Cordillera

  # these are the ones that are not
  # id               name
  # 8   Mixedwood Plains  - 7  Atlantic Maritime
  # 11   Taiga Cordillera - 4 taiga plains
  # 15      Hudson Plains - 6 Boreal Shield West
  # 16  Taiga Shield East - 5  Taiga Shield West
  # 17 Boreal Shield East - 6 Boreal Shield West
  # 18  Semiarid Prairies - 10  Subhumid Prairies
  ecoNotInT5 <- c(8, 11, 15, 16, 17, 18)
  if (any(eco %in% ecoNotInT5)) {
    EcoBoundaryID <- c(7, 4, 6, 5, 6, 10)
    ecoReplace <- data.table(ecoNotInT5, EcoBoundaryID)
    thisAdmin5.1 <- merge(ecoReplace, thisAdmin5, by = "EcoBoundaryID")
    stable5 <- as.data.table(stable5[stable5$ecozone %in% thisAdmin5.1$EcoBoundaryID, ])
  }
  if (nrow(stable5) < 1) {
    stop("There is a problem finding a parameter match in table 5.")
  }

  stable6 <- as.data.table(sim$table6[sim$table6$juris_id %in% thisAdmin$abreviation &
    sim$table6$ecozone %in% eco, ])
  stable7 <- as.data.table(sim$table7[sim$table7$juris_id %in% thisAdmin$abreviation &
    sim$table6$ecozone %in% eco, ])
  # END reducing Biomass model parameter tables -----------------------------------------------

  # Read-in user provided meta data for growth curves. This could be a complete
  # data frame with the same columns as gcMetaEg.csv OR is could be only curve
  # id and species. This format is necessary to process the curves and use the
  # resulting increments
  gcMeta <- sim$gcMeta

  # checking how many columns in gcMeta, if not 6, columns need to be added
  if (!ncol(gcMeta) == 6) {
    # help the user go from their growth curve id and leading species to the six
    # columns: names(gcMeta)
    # [1] "growth_curve_id"           "growth_curve_component_id"
    # [3] "species"                   "canfi_species"
    # [5] "genus"                     "forest_type_id"
    # the data frame canfi_species.csv (in userData_Defaults_spadesCBM -
    # https://drive.google.com/drive/folders/1OBDTp1v_3b3D3Yvg1pHLiW6A_GRklgpD?usp=sharing)
    # has all the possible options for canfi_species (number), genus (4 letter
    # code) and species (three letter code).
    gcMeta2 <- gcMeta[, .(growth_curve_id, species)]
    gcMeta2[, growth_curve_component_id := growth_curve_id]
    # check if all the species are in the canfi_species table
    ### THIS HAS NOT BEEN TESTED YET
    if (nrow(gcMeta2) == length(which(gcMeta$species %in% sim$canfi_species$name))) {
      spsMatch <- sim$canfi_species[
        , which(name %in% gcMeta2$species),
        .(canfi_species, genus, name, forest_type_id)
      ]
      spsMatch[, V1 := NULL]
      names(spsMatch) <- c("canfi_species", "genus", "species", "forest_type_id")
      setkey(gcMeta2, species)
      setkey(spsMatch, species)
      gcMeta3 <- merge(gcMeta2, spsMatch) # I do not think the order of the columns matter
      gcMeta <- gcMeta3
    }
    ### PUT SOMETHING HERE IF THE SPECIES DONT MATCH...NOT SURE WHAT - ERROR MESSAGE?
  }

  # ### TODO CHECK - this in not tested NOT SURE IF THIS IS NEEDED NOW THAT WE ARE WORKING WITH FACTORS
  # if (!unique(unique(userGcM3$GrowthCurveComponentID) == unique(gcMeta$growth_curve_component_id))) {
  #   stop("There is a missmatch in the growth curves of the userGcM3 and the gcMeta")
  # }

  # assuming gcMeta has now 6 columns, it needs a 7th: spatial_unit_id. This
  # will be used in the convertM3biom() fnct to link to the right ecozone
  # and it only needs the gc we are using in this sim.
  gcThisSim <- unique(sim$spatialDT[,.(growth_curve_component_id, spatial_unit_id, ecozones)])
  #gcThisSim <- as.data.table(unique(cbind(sim$spatialUnits, sim$gcids)))
  #names(gcThisSim) <- c("spatial_unit_id", "growth_curve_component_id")
  setkey(gcThisSim, growth_curve_component_id)
  setkey(gcMeta, growth_curve_component_id)
  gcMeta <- merge(gcMeta, gcThisSim)
  # curveID are the columns use to make the unique levels in the factor gcids.
  # These factor levels are the link between the pixelGroups and the curve to be
  # use to growth their AGB. In this case (SK) the levels of the factor need to
  # come from the gcMeta, not the level3DT. Just in case all growth curves need
  # to be processed. If sim$level3DT exist, its gcids needs to match these.

  curveID <- sim$curveID
######## THESE WERE MY CHANGES BUT THEY SEEM TO CAUS AN ERROR IN THE SPINUP
  # gcids <- factor(gcidsCreate(gcMeta[, ..curveID]))
  # setDT(gcMeta)
  # set(gcMeta, NULL, "gcids", gcids)
  if (!is.null(sim$level3DT)) {
    gcidsLevels <- levels(sim$level3DT$gcids)
    gcids <- factor(gcidsCreate(gcMeta[, ..curveID]), levels = gcidsLevels)
  } else {
    gcids <- factor(gcidsCreate(gcMeta[, ..curveID]))
  }

  set(gcMeta, NULL, "gcids", gcids)

  # if (!is.null(sim$level3DT)) {
  #   gcidsLevels <- levels(gcids)
  #   gcids <- factor(gcidsCreate(sim$levelDT[, ..curveID]), levels = gcidsLevels)
  #  }

  sim$gcMetaAllCols <- gcMeta

  # START processing curves from m3/ha to tonnes of C/ha then to annual increments
  # per above ground biomass pools -------------------------------------------

  # 1. Calculate the translation (result is cumPools or "cumulative AGcarbon pools")

  # Matching is 1st on species, then on gcId which gives us location (admin,
  # spatial unit and ecozone)
  fullSpecies <- unique(gcMeta$species) ## RIA: change this to the canfi_sps or match??
  ####cumPools <- NULL
  cumPools <- Cache(cumPoolsCreate, fullSpecies, gcMeta, userGcM3,
                    stable3, stable4, stable5, stable6, stable7, thisAdmin)
  cbmAboveGroundPoolColNames <- "totMerch|fol|other"
  colNames <- grep(cbmAboveGroundPoolColNames, colnames(cumPools), value = TRUE)

  # 2. MAKE SURE THE PROVIDED CURVES ARE ANNUAL
  ### if not, we need to extrapolate to make them annual
  minAgeId <- cumPools[,.(minAge = max(0, min(age) - 1)), by = "gcids"]
  fill0s <- minAgeId[,.(age = seq(from = 0, to = minAge, by = 1)), by = "gcids"]
  # might not need this
  length0s <- fill0s[,.(toMinAge = length(age)), by = "gcids"]
  # these are going to be 0s
  carbonVars <- data.table(gcids = unique(fill0s$gcids),
                           totMerch = 0,
                           fol = 0,
                           other = 0 )

  fiveOf7cols <- fill0s[carbonVars, on = "gcids"]

  otherVars <- cumPools[,.(id = unique(id), ecozone = unique(ecozone)), by = "gcids"]
  add0s <- fiveOf7cols[otherVars, on = "gcids"]
  cumPoolsRaw <- rbindlist(list(cumPools,add0s), use.names = TRUE)
  set(cumPoolsRaw, NULL, "age", as.numeric(cumPoolsRaw$age))
  setorderv(cumPoolsRaw, c("gcids", "age"))

  # 3. Plot the curves that are directly out of the Boudewyn-translation
  # Usually, these need to be, at a minimum, smoothed out.
  figPath <- checkPath(if (is.na(P(sim)$outputFigurePath)) {
      file.path(modulePath(sim), currentModule(sim), "figures")
  } else {
      if (basename(P(sim)$outputFigurePath) == currentModule(sim)) {
        P(sim)$outputFigurePath
      } else {
        file.path(P(sim)$outputFigurePath, currentModule(sim))
      }
  }, create = TRUE)

  # plotting and save the plots of the raw-translation in the sim$ don't really
  # need this b/c the next use of m3ToBiomPlots fnct plots all 6 curves, 3
  # raw-translation and 3-smoothed curves resulting from the Chapman-Richards
  # parameter finding in the cumPoolsSmooth fnct. Leaving these lines here as
  # exploration tools.
  # if (!is.na(P(sim)$.plotInitialTime))
  # sim$plotsRawCumulativeBiomass <- m3ToBiomPlots( inc = cumPoolsRaw,
  #                                        path = figPath,
  #                                        filenameBase = "rawCumBiomass_")

  # Fixing of non-smooth curves
  ## SK is a great example of poor performance of the Boudewyn et al 2007
  ## models. The "translation" does not work well with white birch (probably
  ## because there was not enough data in SK in the model-building data). So,
  ## the resulting curves are for fol and other are nonsensical. This can be
  ## seen by visually inspecting the curves going into the trasnlations (run
  ## m3ToBiomPlots commented above). Here, the user, decided that after all the
  ## catches in place in the cumSmoothPools failed, a hard fix was needed. The
  ## fol and other columns in gcids 37 and 58, will be replace by the fol and
  ## other of gcids 55.

  birchGcIds <- c("37", "58")
  birchColsChg <- c("fol", "other")
  if (any(cumPoolsRaw$gcids == 55)) {
    cumPoolsRaw[gcids %in% birchGcIds, fol := rep(cumPoolsRaw[gcids == 55, fol],2)]
    cumPoolsRaw[gcids %in% birchGcIds, other := rep(cumPoolsRaw[gcids == 55, other],2)]
  }

  opt <- options(reproducible.useMemoise = FALSE)
  on.exit(options(opt))
  # can't memoise
  cumPoolsClean <- Cache(cumPoolsSmooth, cumPoolsRaw)
  options(opt)

  # a[, totMerch := totMerchNew]
  if (!is.na(P(sim)$.plotInitialTime)) {
    figs <- Cache(m3ToBiomPlots, inc = cumPoolsClean,
                  path = figPath,
                  filenameBase = "cumPools_smoothed_postChapmanRichards")
  }
  set(cumPoolsClean, NULL, colNames, NULL)
  colNamesNew <- grep(cbmAboveGroundPoolColNames, colnames(cumPoolsClean), value = TRUE)
  setnames(cumPoolsClean, old = colNamesNew, new = colNames)

  # 4. Calculating Increments
  incCols <- c("incMerch", "incFol", "incOther")
  cumPoolsClean[, (incCols) := lapply(.SD, function(x) c(NA, diff(x))), .SDcols = colNames,
                by = eval("gcids")]
  colsToUse33 <- c("age", "gcids", incCols)
  if (!is.na(P(sim)$.plotInitialTime)) {
    rawIncPlots <- Cache(m3ToBiomPlots, inc = cumPoolsClean[, ..colsToUse33],
                         path = figPath,
                         title = "Smoothed increments merch fol other by gc id",
                         filenameBase = "Increments")
  }
  message(crayon::red("User: please inspect figures of the raw and smoothed translation of your growth curves in: ",
                      figPath))

  sim$cumPoolsClean <- cumPoolsClean

  colsToUseForestType <- c("growth_curve_component_id", "forest_type_id", "gcids")
  forestType <- unique(gcMeta[, ..colsToUseForestType])
  #       #FYI:
  #       # cbmTables$forest_type
  #       # id           name
  #       # 1  1       Softwood
  #       # 2  2      Mixedwood
  #       # 3  3       Hardwood
  #       # 4  9 Not Applicable

  setkeyv(forestType, "gcids")
  cumPoolsClean <- merge(cumPoolsClean, forestType, by = "gcids",
                                     all.x = TRUE, all.y = FALSE)
  swCols <- c("swmerch", "swfol", "swother")
  hwCols <- c("hwmerch", "hwfol", "hwother")

  totalIncrementsSmooth <- cumPoolsClean[forest_type_id == 1, (swCols) := list((incMerch), (incFol), (incOther))]
  totalIncrementsSmooth <- totalIncrementsSmooth[forest_type_id == 3, (hwCols) := list((incMerch), (incFol), (incOther))]
  totalIncrementsSmooth[is.na(totalIncrementsSmooth)] <- 0
  outCols <- c("incMerch", "incFol", "incOther", "forest_type_id")
  incCols <- c(swCols, hwCols)
  totalIncrementsSmooth[, (outCols) := NULL]
  increments <- totalIncrementsSmooth[, (incCols) := list(
    swmerch / 2, swfol / 2,
    swother / 2, hwmerch / 2, hwfol / 2, hwother / 2
  )]
  setorderv(increments, c("gcids", "age"))

  incColKeep <- c("id", "age", incCols)
  set(increments, NULL, "id", as.numeric(increments[["gcids"]]))
  set(increments, NULL, setdiff(colnames(increments), incColKeep), NULL)
  setcolorder(increments, incColKeep)

  # Assertions
  if (isTRUE(P(sim)$doAssertions)) {
    # All should have same min age
    if (length(unique(increments[, min(age), by = "id"]$V1)) != 1)
      stop("All ages should start at the same age for each curveID")
    if (length(unique(increments[, max(age), by = "id"]$V1)) != 1)
      stop("All ages should end at the same age for each curveID")
  }

  sim$growth_increments <- as.matrix(increments)
  # END process growth curves -------------------------------------------------------------------------------

  sim$gcHash <- matrixHash(sim$growth_increments)
  # create a nested hash (by gcid/by age)
  ## used in SpinUp function later...
  for (item in ls(sim$gcHash)) {
    sim$gcHash[[item]] <- hash(sim$gcHash[[item]])
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  # Plot(sim$object)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  # cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  if (!suppliedElsewhere("gcids", sim)) {
    ## this is where the pixelGroups and their spu eco etc.
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to limit the number of growth curves used.")
    sim$gcids <- c(
      52, 52, 58, 52, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      61, 28, 29, 31, 34, 35, 37, 40, 49, 50, 52, 55, 58, 61, 28, 29,
      31, 34, 37, 40, 49, 50, 52, 55, 56, 58, 61, 28, 29, 31, 34, 40,
      49, 50, 52, 55, 58, 61, 28, 34, 49, 52, 55, 40, 28, 31, 34, 40,
      49, 50, 52, 55, 61, 28, 31, 34, 40, 49, 50, 52, 55, 61, 52, 55,
      58, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 28, 31, 34,
      37, 40, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49, 50, 52,
      55, 61, 28, 31, 34, 40, 49, 52, 55, 61, 28, 61, 52, 61, 62, 28,
      31, 34, 40, 49, 50, 52, 55, 61, 31, 34, 49, 52, 55, 28, 31, 34,
      40, 49, 50, 52, 55, 58, 61, 62, 28, 29, 31, 34, 40, 49, 50, 52,
      55, 61, 28, 34, 40, 49, 50, 52, 55, 61, 62, 28, 34, 40, 61, 49,
      31, 40, 49, 61, 28, 29, 31, 34, 40, 49, 50, 52, 58, 61, 28, 31,
      34, 40, 49, 50, 52, 55, 61, 49, 52, 55, 28, 31, 34, 40, 49, 50,
      52, 55, 58, 61, 28, 31, 34, 40, 49, 50, 52, 55, 61, 40, 49, 50,
      52, 61, 28, 31, 31, 61, 28, 31, 34, 49, 50, 55, 61, 28, 31, 34,
      49, 61, 28, 34, 52, 61, 31, 49, 52, 55, 55, 40, 28, 49, 28, 31,
      34, 49, 52, 28, 31, 58, 61, 28, 31, 34, 49, 50, 61, 52, 49, 52,
      55, 58, 31, 34, 37, 49, 52, 55, 52, 55, 58, 31, 34, 49, 52, 55,
      56, 58, 31, 34, 49, 52, 55, 56, 58, 61, 49, 52, 55, 52, 55, 28,
      34, 49, 55, 28, 31, 34, 37, 52, 55, 49, 52, 55, 28, 31, 34, 37,
      49, 52, 55, 58, 28, 31, 34, 37, 49, 52, 55, 58, 28, 31, 34, 37,
      49, 52, 55, 34, 37, 50, 52, 52, 28, 31, 34, 37, 52, 55, 28, 31,
      34, 37, 49, 52, 55, 58, 52, 55, 28, 31, 34, 37, 40, 49, 52, 55,
      58, 28, 31, 34, 37, 40, 49, 52, 55, 58, 61, 28, 31, 34, 37, 49,
      52, 55, 58, 28, 31, 34, 37, 52, 55, 31, 52, 55, 31, 28, 31, 34,
      37, 40, 49, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      52, 55, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37,
      40, 49, 50, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55, 58,
      28, 31, 34, 37, 49, 52, 55, 58, 34, 49, 55, 28, 31, 28, 31, 34,
      49, 52, 55, 58, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 49, 52,
      28, 31, 34, 37, 40, 49, 50, 52, 55, 58, 28, 29, 31, 34, 35, 37,
      40, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 31, 34, 49, 52, 55, 58, 52, 28, 28, 34, 49, 55, 58,
      61, 28, 34, 37, 49, 50, 52, 55, 58, 61, 28, 31, 34, 37, 40, 49,
      50, 52, 55, 58, 61, 28, 31, 34, 37, 49, 50, 52, 55, 58, 61, 28,
      31, 34, 49, 50, 52, 55, 58, 61, 28, 40, 49, 55, 58, 49, 34, 28,
      31, 34, 49, 50, 52, 55, 58, 28, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 28, 29,
      31, 34, 37, 40, 49, 50, 52, 55, 56, 58, 61, 28, 29, 31, 34, 37,
      40, 49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 40, 49, 50, 52, 55,
      61, 31, 50, 49, 52, 61, 28, 31, 34, 49, 50, 52, 55, 58, 61, 28,
      31, 34, 37, 40, 49, 50, 52, 55, 58, 61, 52, 28, 31, 34, 37, 40,
      49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 37, 40, 49, 50, 52, 55,
      58, 61, 28, 31, 34, 40, 49, 50, 52, 55, 58, 61, 28, 34, 49, 50,
      52, 55, 58, 61, 49, 50, 55, 61, 49, 52, 55, 58, 61, 28, 29, 31,
      34, 40, 49, 50, 52, 55, 58, 61, 28, 29, 31, 34, 37, 40, 49, 50,
      52, 55, 58, 61
    )
  }

  if (!suppliedElsewhere("ecozones", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which ecozones these curves are in.")
    sim$ecozones <- c(
      9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6,
      6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6,
      6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6, 9, 9,
      9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 6, 6, 6, 9, 6,
      6, 6, 9, 9, 9, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 9, 9, 9, 9, 6,
      6, 9, 6, 6, 6, 9, 9, 6, 6, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 6, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6,
      9, 9, 9, 6, 6, 9, 9, 9, 6, 6, 6, 6, 9, 9, 6, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 6, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 6, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6,
      6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6,
      9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9, 9, 9,
      9, 6, 6, 9, 9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6,
      9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9,
      9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 6, 6, 6, 9, 9,
      9, 9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6,
      9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6,
      6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 6, 6, 6, 6, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 6, 6, 6, 9, 9, 9, 6, 9, 6, 6, 6, 9, 9, 9, 9, 6, 6,
      9, 6, 6, 6, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 6, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 6, 6, 9, 9, 9,
      9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9,
      6, 6, 6, 6, 9, 9, 9, 6, 6, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9,
      6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 6, 9, 9,
      9, 6, 9, 9, 9, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
      9
    )
  }
  if (!suppliedElsewhere("spatialUnits", sim)) {
    message("No spatial information was provided for the growth curves.
            The default values (SK simulations) will be used to determine which CBM-spatial units these curves are in.")
    sim$spatialUnits <- c(
      28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 27, 28, 28, 28, 28, 27,
      27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 28, 28,
      27, 27, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 28, 28,
      28, 28, 27, 27, 27, 28, 27, 27, 27, 28, 28, 28, 28, 27, 27, 27,
      28, 28, 27, 27, 28, 28, 27, 28, 28, 28, 28, 27, 27, 28, 27, 27,
      27, 28, 28, 27, 27, 28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28,
      28, 28, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 27,
      27, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 27, 27, 27, 27,
      28, 28, 28, 27, 27, 28, 28, 28, 27, 27, 27, 27, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28,
      28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 28,
      28, 28, 28, 27, 27, 27, 27, 28, 28, 27, 28, 28, 27, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28,
      27, 27, 27, 27, 28, 28, 28, 28, 27, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28,
      27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 28, 28, 28,
      28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28,
      28, 28, 28, 28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28, 28, 28, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 27, 28, 28, 28, 28, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27,
      27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27,
      28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28,
      28, 28, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 28, 28,
      28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 28, 27, 27, 27,
      27, 27, 28, 28, 28, 28, 28, 28, 27, 27, 27, 27, 27, 27, 28, 28,
      28, 28, 28, 28
    )
  }

  # userGcM3 and userGcM3File, these files are the m3/ha and age info by growth
  # curve ID, columns should be GrowthCurveComponentID	Age	MerchVolume
  ## TO DO: add a data manipulation to adjust if the m3 are not given on a yearly basis
  if (!suppliedElsewhere("userGcM3", sim)) {
    if (!suppliedElsewhere("userGcM3File", sim)) {
      sim$userGcM3File <- file.path(dPath, "userGcM3.csv") ## TODO: use prepInputs from url
      sim$userGcM3 <- fread(sim$userGcM3File)
      message(
        "User has not supplied growth curves (m3 by age or the file name for the growth curves). ",
        "The default will be used which is for a region in Saskatchewan."
      )
    }
    names(sim$userGcM3) <- c("GrowthCurveComponentID", "Age", "MerchVolume")
  }

  if (!suppliedElsewhere("curveID", sim)) {
    sim$curveID <- c("growth_curve_component_id")#, "ecozones")
  }

  ## tables from Boudewyn -- all downloaded from the NFIS site.
  ## however, NFIS changes the tables and seems to forget parameter columns at times.
  if (!suppliedElsewhere("table3", sim)) {
    sim$table3 <- prepInputs(url = extractURL("table3"),
                             destinationPath = dPath,
                             fun = "data.table::fread")

    ### NOTE: the .csv previously had a column with commas, which adds an extra col
    # these are the columns needed in the functions for calculating biomass
    t3hasToHave <- c("juris_id", "ecozone", "canfi_species", "genus", "species", "a", "b", "volm")
    if (length(which(colnames(sim$table3) %in% t3hasToHave)) != length(t3hasToHave)) {
      message(
        "The parameter table (appendix2_table3) does not have the expected number of columns. ",
        "This means parameters are missing. The default (older) parameter file will be used instead."
      )
      if (!file.exists(file.path(dPath, "appendix2_table3.csv"))) {
        drive_download(as_id("1CpgyJ1uJYqoOQiMxmPNZPTDKymfpelQC"),
                       path = file.path(dPath, "appendix2_table3.csv"))
      }
      sim$table3 <- fread(file.path(dPath, "appendix2_table3.csv"))
    }
  }

  if (!suppliedElsewhere("table4", sim)) {
    sim$table4 <- prepInputs(url = extractURL("table4"),
                             destinationPath = dPath,
                             fun = "data.table::fread")

    ### NOTE: the .csv previously had a column with commas, which adds an extra col
    t4hasToHave <- c("juris_id", "ecozone", "canfi_species", "genus", "species",
                      "a", "b", "k", "cap", "volm")
    if (!length(which(colnames(sim$table4) %in% t4hasToHave)) == length(t4hasToHave)) {
      message(
        "The parameter table (appendix2_table4) does not have the expected number of columns. ",
        "This means parameters are missing. The default (older) parameter file will be used instead."
      )
      if (!file.exists(file.path(dPath, "appendix2_table4.csv"))) {
        drive_download(as_id("1dGLFuysj1SBMzaJgMoLuVH_Hwo2329jb"),
                       path = file.path(dPath, "appendix2_table4.csv"))
      }

      sim$table4 <- fread(file.path(dPath, "appendix2_table4.csv"))
    }
  }

  if (!suppliedElsewhere("table5", sim)) {
    sim$table5 <- prepInputs(url = extractURL("table5"),
                             destinationPath = dPath,
                             fun = "data.table::fread")

    ### NOTE: the .csv previously had a column with commas, which adds an extra col
    t5hasToHave <- c("juris_id", "ecozone", "canfi_genus", "genus", "a", "b", "k", "cap", "volm")
    if (!length(which(colnames(sim$table5) %in% t5hasToHave)) == length(t5hasToHave)) {
      message(
        "The parameter table (appendix2_table5) does not have the expected number of columns. ",
        "This means parameters are missing. The default (older) parameter file will be used instead."
      )
      if (!file.exists(file.path(dPath, "appendix2_table5.csv"))) {
        drive_download(as_id("12WsNPZUdYudazG9bfLqFJC9tqH_ZRhFN"),
                       path = file.path(dPath, "appendix2_table5.csv"))
      }
      sim$table5 <- fread(file.path(dPath, "appendix2_table5.csv"))
    }
  }

  if (!suppliedElsewhere("table6", sim)) {
    sim$table6 <- prepInputs(url = extractURL("table6"),
                             destinationPath = dPath,
                             fun = "data.table::fread")

    ### NOTE: the .csv previously had a column with commas, which adds an extra col
    t6hasToHave <- c("juris_id", "ecozone", "canfi_species", "a1", "a2", "a3", "b1", "b2", "b3",
                     "c1", "c2", "c3" )
    if (!length(which(colnames(sim$table6) %in% t6hasToHave)) == length(t6hasToHave)) {
      message(
        "The parameter table (appendix2_table6) does not have the expected number of columns. ",
        "This means parameters are missing. The default (older) parameter file will be used instead."
      )
      if (!file.exists(file.path(dPath, "appendix2_table6.csv"))) {
        drive_download(as_id("1FiNyacoLKq96r2tNoXwQZG2YXOl0f6_H"),
                       path = file.path(dPath, "appendix2_table6.csv"))
      }

      sim$table6 <- fread(file.path(dPath, "appendix2_table6.csv"))
    }
  }

  if (!suppliedElsewhere("table7", sim)) {
    sim$table7 <- prepInputs(url = extractURL("table7"),
                             destinationPath = dPath,
                             fun = "data.table::fread")

    ### NOTE: the .csv previously had a column with commas, which adds an extra col
    t7hasToHave <- c("juris_id", "ecozone", "canfi_species", "vol_min", "vol_max", "p_sw_low",
                     "p_sb_low", "p_br_low", "p_fl_low", "p_sw_high", "p_sb_high", "p_br_high",
                     "p_fl_high")
    if (length(which(colnames(sim$table7) %in% t7hasToHave)) != length(t7hasToHave)) {
      message(
        "The parameter table (appendix2_table7) does not have the expected number of columns. ",
        "This means parameters are missing. The default (older) parameter file will be used instead."
      )
      if (!file.exists(file.path(dPath, "appendix2_table7.csv"))) {
        drive_download(as_id("1ObbZzx_reQUB3repilx_30eCX1vjJOl_"),
                       path = file.path(dPath, "appendix2_table7.csv"))
      }
      sim$table7 <- fread(file.path(dPath, "appendix2_table7.csv"))
    }
  }

  if (!suppliedElsewhere("gcMeta", sim)) {
    if (!suppliedElsewhere("gcMetaFile", sim)) {
      if (!file.exists(file.path(dPath, "gcMetaEg.csv"))) {
        drive_download(as_id(extractURL("gcMeta")), path = file.path(dPath, "gcMetaEg.csv"))
      }
    }
    sim$gcMeta <- fread(file.path(dPath, "gcMetaEg.csv"))
  }

  # cbmAdmin: this is needed to match species and parameters. Boudewyn et al 2007
  # abbreviation and cbm spatial units and ecoBoudnary id is provided with the
  # adminName to avoid confusion.
  if (!suppliedElsewhere("cbmAdmin", sim)) {
    if (!suppliedElsewhere("cbmAdminFile", sim)) {
      if (!file.exists(file.path(dPath, "cbmAdmin.csv"))) {
        drive_download(as_id(extractURL("cbmAdmin")), path = file.path(dPath, "cbmAdmin.csv"))
      }
    }
    sim$cbmAdmin <- fread(file.path(dPath, "cbmAdmin.csv"))
  }

  # canfi_species: for the Boudewyn parameters, the species have to be matched
  # with the ones in the Boudewyn tables. The choices HAVE to be one of these.
  # This contains three columns, canfi_species, genus and species form the
  # publication and I added (manually) one more: forest_type_id. That variable is a CBM-CFS3
  # indicator as follows:
  # cbmTables$forest_type
  # id           name
  # 1  1       Softwood
  # 2  2      Mixedwood
  # 3  3       Hardwood
  # 4  9 Not Applicable
  if (!suppliedElsewhere("canfi_species", sim)) {
    if (!file.exists(file.path(dPath, "canfi_species.csv"))) {
      drive_download(as_id(extractURL("canfi_species")), path = file.path(dPath, "canfi_species.csv"))
    }
    sim$canfi_species <- fread(file.path(dPath, "canfi_species.csv")) ## TODO: use prepInputs with url
  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
