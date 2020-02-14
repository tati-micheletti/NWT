# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "LandR_BiomassGMOrig",
  description = "insert module description here",
  keywords = c("insert key words here"),
  authors = person("First", "Last", email = "first.last@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.3.9009", numeric_version("1.3.1.9036")),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "LandR_BiomassGMOrig"),
  reqdPkgs = list("data.table", "raster",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/pemisc@development"), ## TODO: update package list
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description")),
    defineParameter("calibrate", "logical", FALSE, NA, NA, "should the model have detailed outputs?"),
    defineParameter("growthInitialTime", "numeric", default = 10, min = NA_real_, max = NA_real_,
                    desc = "Initial time for the growth event to occur - should be the same as the succession time step used in LBMR"),
    defineParameter("successionTimestep", "numeric", 10, NA, NA,
                    desc = "defines the simulation time step, default is 10 years"),
    defineParameter(".plotInitialTime", "numeric", default = 0, min = NA, max = NA,
                    desc = "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".saveInitialTime", "numeric", default = 0, min = NA, max = NA,
                    desc = paste("This describes the simulation time at which the first save event should occur.",
                                 "Set to NA if no saving is desired.")),
    defineParameter(".useParallel", "ANY", default = parallel::detectCores(),
                    desc = paste("Used only in seed dispersal. If numeric, it will be passed to data.table::setDTthreads,",
                                 "if logical and TRUE, it will be passed to parallel::makeCluster,",
                                 "and if cluster object it will be passed to parallel::parClusterApplyLB"))
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("cohortData", "data.table",
                 desc = "age cohort-biomass table hooked to pixel group map by pixelGroupIndex at
                 succession time step",
                 sourceURL = NA),
    expectsInput("lastReg", "numeric",
                 desc = "time at last regeneration", sourceURL = NA),
    expectsInput("species", "data.table",
                 desc = "a table that has species traits such as longevity...",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession-Archive/master/biomass-succession-archive/trunk/tests/v6.0-2.0/species.txt"),
    expectsInput("speciesEcoregion", "data.table",
                 desc = "table defining the maxANPP, maxB and SEP, which can change with both ecoregion and simulation time",
                 sourceURL = "https://raw.githubusercontent.com/LANDIS-II-Foundation/Extensions-Succession/master/biomass-succession-archive/trunk/tests/v6.0-2.0/biomass-succession-dynamic-inputs_test.txt")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("calculateAgeMortality", "function",
                  desc = "function to calculate aging and mortality"),
    createsOutput("calculateANPP", "function",
                  desc = "function to calculate ANPP"),
    createsOutput("calculateCompetition", "function",
                  desc = "function to calculate competition for light"),
    createsOutput("calculateGrowthMortality", "data.table",
                  desc = "function to calculate growth and mortality"),
    createsOutput("calculateSumB", "function",
                  desc = "function to sum biomass"),
    createsOutput("cohortData", "function",
                  desc = "tree-level data by pixel group"),
    createsOutput("simulationTreeOutput", "data.table",
                  desc = "Summary of several characteristics about the stands, derived from cohortData"),
    createsOutput("updateSpeciesAttributes", "function",
                  desc = "function to add/update species attributes in species cohort table"),
    createsOutput("updateSpeciesEcoregionAttributes", "function",
                  desc = "function to add/update species ecoregion attributes in species cohort table")
  )
))

## event types
#   - type `init` is required for initialiazation
doEvent.LandR_BiomassGMOrig = function(sim, eventTime, eventType, debug = FALSE) {
  if (is.numeric(P(sim)$.useParallel)) {
    a <- data.table::setDTthreads(P(sim)$.useParallel)
    message("Mortality and Growth should be using >100% CPU")
    on.exit(data.table::setDTthreads(a), add = TRUE)
  }
  switch(eventType,
         init = {
           ## do stuff for this event
           sim <- Init(sim)
           sim <- scheduleEvent(sim, start(sim) + P(sim)$growthInitialTime,
                                "LandR_BiomassGMOrig", "mortalityAndGrowth", eventPriority = 5)
         },
         mortalityAndGrowth = {
           sim <- MortalityAndGrowth(sim)
           sim <- scheduleEvent(sim, time(sim) + 1, "LandR_BiomassGMOrig", "mortalityAndGrowth",
                                eventPriority = 5)
         },
         warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                       "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}
## event functions
Init <- function(sim) {
  return(invisible(sim))
}

MortalityAndGrowth <- function(sim) {
  if (is.numeric(P(sim)$.useParallel)) {
    data.table::setDTthreads(P(sim)$.useParallel)
    message("Mortality and Growth should be using >100% CPU")
  }
  if (!all(colnames(sim$cohortData) %in% c("pixelGroup", "ecoregionGroup",
                                     "speciesCode", "age", "B", "mortality", "aNPPAct")))
    sim$cohortData <- sim$cohortData[, .(pixelGroup, ecoregionGroup,
                                         speciesCode, age, B, mortality, aNPPAct)]
  cohortData <- sim$cohortData
  sim$cohortData <- cohortData[0, ]
  pixelGroups <- data.table(pixelGroupIndex = unique(cohortData$pixelGroup),
                            temID = 1:length(unique(cohortData$pixelGroup)))
  groupSize <- 1e5
  cutpoints <- sort(unique(c(seq(1, max(pixelGroups$temID), by = groupSize), max(pixelGroups$temID))))
  #cutpoints <- c(1,max(pixelGroups$temID))
  if (length(cutpoints) == 1) cutpoints <- c(cutpoints, cutpoints + 1)
  pixelGroups[, groups := rep(paste0("Group", seq(length(cutpoints)-1)),
                              each = groupSize, length.out = NROW(pixelGroups))]
  # pixelGroups[, groups1 := cut(temID, breaks = cutpoints,
  #                             labels = paste("Group", 1:(length(cutpoints) - 1), sep = ""),
  #                             include.lowest = FALSE)]
  for (subgroup in paste("Group", 1:(length(cutpoints) - 1), sep = "")) {
    subCohortData <- cohortData[pixelGroup %in% pixelGroups[groups == subgroup, ]$pixelGroupIndex, ]
    #   cohortData <- sim$cohortData
    set(subCohortData, NULL, "age", subCohortData$age + 1L)
    subCohortData <- updateSpeciesEcoregionAttributes(speciesEcoregion = sim$speciesEcoregion,
                                                      time = round(time(sim)),
                                                      cohortData = subCohortData)
    subCohortData <- updateSpeciesAttributes(species = sim$species, cohortData = subCohortData)
    subCohortData <- calculateSumB(cohortData = subCohortData,
                                   lastReg = sim$lastReg,
                                   simuTime = time(sim),
                                   successionTimestep = P(sim)$successionTimestep)
    startNumCohorts <- NROW(subCohortData)

    #########################################################
    # Die from old age -- rm from cohortData
    #########################################################
    subCohortPostLongevity <- subCohortData[age <= longevity,]
    postLongevityDieOffNumCohorts <- NROW(subCohortPostLongevity)
    numCohortsDiedOldAge <- startNumCohorts - postLongevityDieOffNumCohorts
    if ((numCohortsDiedOldAge) > 0) {
      diedCohortData <- subCohortData[!subCohortPostLongevity, on = c("pixelGroup", "speciesCode"),
                                      .(pixelGroup, speciesCode, ecoregionGroup, age)]
      # Identify the PGs that are totally gone, not just an individual cohort that died
      pgsToRm <- diedCohortData[!diedCohortData$pixelGroup %in% subCohortPostLongevity$pixelGroup]
      pixelsToRm <- which(getValues(sim$pixelGroupMap) %in% unique(pgsToRm$pixelGroup))
      # RM from the pixelGroupMap -- since it is a whole pixelGroup that is gone, not just a cohort, this is necessary
      if (isTRUE(getOption("LandR.assertions"))) {
        a <- subCohortPostLongevity$pixelGroup %in% na.omit(getValues(sim$pixelGroupMap))
        if (!all(a)) {
          stop("Post longevity-based mortality, there is a divergence between pixelGroupMap and cohortData pixelGroups")
        }
      }
      if (length(pixelsToRm) > 0) {
        if (getOption("LandR.verbose", TRUE) > 0) {
          numPixelGrps <- sum(sim$pixelGroupMap[]!=0, na.rm = TRUE)
        }
        sim$pixelGroupMap[pixelsToRm] <- 0L
        if (getOption("LandR.verbose", TRUE) > 1) {
          message(blue("Death due to old age:",
                       "\n  ", numCohortsDiedOldAge, "cohorts died of old age (i.e., due to passing longevity); ",
                       sum(is.na(diedCohortData$age)), " of those because age == NA; ",
                       "\n  ", NROW(unique(pgsToRm$pixelGroup)), "pixelGroups to be removed (i.e., ",
                       "\n  ", length(pixelsToRm), "pixels; "))
        }
        if (getOption("LandR.verbose", TRUE) > 0) {
          message(blue("\n   Total number of pixelGroups -- Was:", numPixelGrps,
                       ", Now:", magenta(sum(sim$pixelGroupMap[]!=0, na.rm = TRUE))))
        }
      }
    }
    subCohortData <- subCohortPostLongevity
    subCohortData <- calculateAgeMortality(cohortData = subCohortData)
    set(subCohortData, NULL, c("longevity", "mortalityshape"), NULL)
    subCohortData <- calculateCompetition(cohortData = subCohortData)
    if (!P(sim)$calibrate) {
      set(subCohortData, NULL, "sumB", NULL)
    }
    #### the below two lines of codes are to calculate actual ANPP
    subCohortData <- calculateANPP(cohortData = subCohortData)
    set(subCohortData, NULL, "growthcurve", NULL)
    set(subCohortData, NULL, "aNPPAct", pmax(1, subCohortData$aNPPAct - subCohortData$mAge))
    subCohortData <- calculateGrowthMortality(cohortData = subCohortData)
    set(subCohortData, NULL, "mBio", pmax(0, subCohortData$mBio - subCohortData$mAge))
    set(subCohortData, NULL, "mBio", pmin(subCohortData$mBio, subCohortData$aNPPAct))
    set(subCohortData, NULL, "mortality", subCohortData$mBio + subCohortData$mAge)
    set(subCohortData, NULL, c("mBio", "mAge", "maxANPP", "maxB", "maxB_eco", "bAP", "bPM"), NULL)
    if (P(sim)$calibrate) {
      set(subCohortData, NULL, "deltaB", asInteger(subCohortData$aNPPAct - subCohortData$mortality))
      set(subCohortData, NULL, "B", subCohortData$B + subCohortData$deltaB)
      tempcohortdata <- subCohortData[,.(pixelGroup, Year = time(sim), siteBiomass = sumB, speciesCode,
                                         Age = age, iniBiomass = B - deltaB, ANPP = round(aNPPAct, 1),
                                         Mortality = round(mortality,1), deltaB, finBiomass = B)]
      tempcohortdata <- setkey(tempcohortdata, speciesCode)[
        setkey(sim$species[, .(species, speciesCode)], speciesCode),
        nomatch = 0][, ':='(speciesCode = species, species = NULL, pixelGroup = NULL)]
      setnames(tempcohortdata, "speciesCode", "Species")
      sim$simulationTreeOutput <- rbind(sim$simulationTreeOutput, tempcohortdata)
      set(subCohortData, NULL, c("deltaB", "sumB"), NULL)
    } else {
      set(subCohortData, NULL, "B",
          subCohortData$B + asInteger(subCohortData$aNPPAct - subCohortData$mortality))
    }
    subCohortData[, `:=`(mortality = asInteger(mortality), aNPPAct = asInteger(aNPPAct))]
    sim$cohortData <- rbindlist(list(sim$cohortData, subCohortData), fill = TRUE)
    rm(subCohortData)
    # .gc() # TODO: use .gc()
  }
  rm(cohortData, cutpoints, pixelGroups)

  if (isTRUE(getOption("LandR.assertions"))) {
    if (NROW(unique(sim$cohortData[pixelGroup == 67724]$ecoregionGroup)) > 1) stop()
    if (!identical(NROW(sim$cohortData), NROW(unique(sim$cohortData, by = c("pixelGroup", "speciesCode", "age", "B"))))) {
      stop("sim$cohortData has duplicated rows, i.e., multiple rows with the same pixelGroup, speciesCode and age")
    }

  }
  assertCohortData(sim$cohortData, sim$pixelGroupMap)
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  cacheTags <- c(currentModule(sim), "function:.inputObjects")
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  # read species txt and convert it to data table
  if (!suppliedElsewhere("species", sim)) {
    mainInput <- prepInputsMainInput(url = NULL, dPath, cacheTags) ## uses default URL
    sim$species <- prepInputsSpecies(url = extractURL("species"), dPath, cacheTags)
  }
  if (!suppliedElsewhere("speciesEcoregion", sim)) {
    sim$speciesEcoregion <- prepInputsSpeciesEcoregion(url = extractURL("speciesEcoregion"),
                                                       dPath = dPath, cacheTags = cacheTags)
  }
  ## export local functions to simList
  sim$updateSpeciesEcoregionAttributes <- updateSpeciesEcoregionAttributes
  sim$updateSpeciesAttributes <- updateSpeciesAttributes
  sim$calculateSumB <- calculateSumB
  sim$calculateAgeMortality <- calculateAgeMortality
  sim$calculateANPP <- calculateANPP
  sim$calculateGrowthMortality <- calculateGrowthMortality
  sim$calculateCompetition <- calculateCompetition
  return(invisible(sim))
}
