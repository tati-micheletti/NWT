defineModule(sim, list(
  name = "posthocLandR",
  description = "Posthoc module for LandR. Prepares the biomass, leading species, average forest age",
  keywords = c("forest succession", "vegetation", "posthoc analysis", "plots", "leading species"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.6.9005", posthocLandR = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "posthocLandR.Rmd"),
  reqdPkgs = list("raster","tati-micheletti/usefulFuns", "future", "future.apply"),
  parameters = rbind(
    defineParameter("plotFireStats", "logical", FALSE, NA, NA, paste0("If fire was ran on the landscape, set this parameter to TRUE to generate burn summaries")),
    defineParameter("addCaribousuitability", "logical", FALSE, NA, NA, paste0("If TRUE, adds to the forest age plot the colors indicating suitability for caribou:",
                                                                              "red = worse, yellow = mid, green = better")),
    defineParameter("years", "numeric", start(sim):end(sim), NA, NA, paste0("Sequence of years to be used in the plots",
                                                                  "Make sure you have all necessary '.rds' files saved in the 'resultsFolders'")),
    defineParameter("saveRAS", "logical", FALSE, NA, NA, paste0("Should the rasters be saved?")),
    defineParameter("overwriteLeadingMap", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("overwriteBiomassMap", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("totalBiomass", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("totalBiomassProp", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("totalBiomassOverstory", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("totalBiomassOverstoryProp", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("overwriteForestAge", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("overwriteBurnSumm", "logical", FALSE, NA, NA, paste0("Should be overwritten?")),
    defineParameter("futurePlan", "character", "multiprocess", NA, NA, 
                    paste0("Which plan should be used for parallelize?")),
    defineParameter("sppEquivCol", "character", "KNN", NA, NA, paste0("Column of sppEquivalencies_CA to be used"))
    ),
  inputObjects = bind_rows(
    expectsInput(objectName = "resultsFolders", objectClass = "list", 
                 desc = paste0("This is a named list of the folders where the results should be.",
                               " (i.e. `folders[['LandR.CS_fS']] = file.path(getwd(),'outputs/DATE/LandR.CS_fS')`)",
                               " Pass as a vector of locations for running the functions in more than one location, as it",
                               " lapplies through it internally. The names should be the type of simulation ran"), 
                 sourceURL = NA),
    expectsInput(objectName = "sppEquivalencies_CA", objectClass = "data.table", 
                 desc = paste0("Equivalencies table with all species names. Defaults to LandR::sppEquivalencies_CA")),
    expectsInput(objectName = "sppColorVect", objectClass = "character", 
                 desc = paste0("Named color vector"))
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "LeadingVegetationType", objectClass = "", desc = NA),
    createsOutput(objectName = "vegetationBiomassMap", objectClass = "", desc = NA),
    createsOutput(objectName = "biomassPerSpeciesPlot", objectClass = "", desc = NA),
    createsOutput(objectName = "biomassPerSpeciesProportionalPlot", objectClass = "", desc = NA),
    createsOutput(objectName = "overstoryBiomassPerSpeciesPlot", objectClass = "", desc = NA),
    createsOutput(objectName = "overstoryBiomassPerSpeciesProportionalPlot", objectClass = "", desc = NA),
    createsOutput(objectName = "forestAgePlot", objectClass = "", desc = NA),
    createsOutput(objectName = "burnSummary", objectClass = "", desc = NA),
    createsOutput(objectName = "allPlots", objectClass = "list", desc = NA)
  )
))

doEvent.posthocLandR = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim$LeadingVegetationType <- list()
      sim$vegetationBiomassMap <- list()
      sim$biomassPerSpeciesPlot <- list()
      sim$biomassPerSpeciesProportionalPlot <- list()
      sim$overstoryBiomassPerSpeciesPlot <- list()
      sim$overstoryBiomassPerSpeciesProportionalPlot <- list()
      sim$forestAgePlot <- list()
      sim$burnSummary <- list()
      
      sim <- scheduleEvent(sim, start(sim), "posthocLandR", "createPlots")
    },
    createPlots = {
      sim$allPlots <- future.apply::future_lapply(names(sim$resultsFolders), function(typeSim){  
        # [ FIX ] still need to make use of the overwrite! # 
        sim$LeadingVegetationType[[typeSim]] <- plotLeadingVegetationType(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          saveRAS = P(sim)$saveRAS,
          overwrite = P(sim)$overwriteLeadingMap,
          sppEquivCol = P(sim)$sppEquivCol,
          sppEquivalencies_CA = sim$sppEquivalencies_CA,
          sppColorVect = sim$sppColorVect)
        
        sim$vegetationBiomassMap[[typeSim]] <- plotVegetationBiomass(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim,
          saveRAS = P(sim)$saveRAS, 
          years = c(P(sim)$years[1], 
                    P(sim)$years[length(P(sim)$years)]),
          overwrite = P(sim)$overwriteBiomassMap)
        
        sim$biomassPerSpeciesPlot[[typeSim]] <- totalBiomassPerSpecies(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          proportional = FALSE, 
          overstory = FALSE,
          overwrite = P(sim)$totalBiomass,
          sppEquivCol = P(sim)$sppEquivCol,
          sppEquivalencies_CA = sim$sppEquivalencies_CA,
          sppColorVect = sim$sppColorVect)
        
        sim$biomassPerSpeciesProportionalPlot[[typeSim]] <- totalBiomassPerSpecies(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          proportional = TRUE, 
          overstory = FALSE,
          overwrite = P(sim)$totalBiomassProp,
          sppEquivCol = P(sim)$sppEquivCol,
          sppEquivalencies_CA = sim$sppEquivalencies_CA,
          sppColorVect = sim$sppColorVect)
        
        sim$overstoryBiomassPerSpeciesPlot[[typeSim]] <- totalBiomassPerSpecies(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          proportional = FALSE, 
          overstory = TRUE,
          overwrite = P(sim)$totalBiomassOverstory,
          sppEquivCol = P(sim)$sppEquivCol,
          sppEquivalencies_CA = sim$sppEquivalencies_CA,
          sppColorVect = sim$sppColorVect)
        
        sim$overstoryBiomassPerSpeciesProportionalPlot[[typeSim]] <- totalBiomassPerSpecies(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          proportional = TRUE, 
          overstory = TRUE,
          overwrite = P(sim)$totalBiomassOverstoryProp,
          sppEquivCol = P(sim)$sppEquivCol,
          sppEquivalencies_CA = sim$sppEquivalencies_CA,
          sppColorVect = sim$sppColorVect)
        
        sim$forestAgePlot[[typeSim]] <- forestAgePlot(
          dataPath = sim$resultsFolders[[typeSim]], 
          typeSim = typeSim, 
          addCaribousuitability = P(sim)$addCaribousuitability,
          overwrite = P(sim)$overwriteForestAge)
        
        if (P(sim)$plotFireStats)
          sim$burnSummary[[typeSim]] <- plotBurnSummary(
            sim$resultsFolders[[typeSim]], 
            typeSim = typeSim, 
            lastYear = P(sim)$years[length(P(sim)$years)],
            overwrite = P(sim)$overwriteBurnSumm) 
        # theObject = LandR.CS_fS$burnSummary # if we have the object
        
        allPlotsScenario <- list(LeadingVegetationType = sim$LeadingVegetationType[[typeSim]],
                                 vegetationBiomassMap = sim$vegetationBiomassMap[[typeSim]],
                                 biomassPerSpeciesPlot = sim$biomassPerSpeciesPlot[[typeSim]],
                                 biomassPerSpeciesProportionalPlot = sim$biomassPerSpeciesProportionalPlot[[typeSim]],
                                 overstoryBiomassPerSpeciesPlot = sim$overstoryBiomassPerSpeciesPlot[[typeSim]],
                                 overstoryBiomassPerSpeciesProportionalPlot = sim$overstoryBiomassPerSpeciesProportionalPlot[[typeSim]],
                                 forestAgePlot = sim$forestAgePlot[[typeSim]],
                                 burnSummary = sim$burnSummary[[typeSim]])
        return(allPlotsScenario)
      })
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere(resultsFolders, "sim")){
    stop("Need to supply 'resultsFolders' that contains the .rds files saved from a run")
  }
  
  if (!suppliedElsewhere(sppEquivalencies_CA, "sim")){
  sim$sppEquivalencies_CA <- LandR::sppEquivalencies_CA 
  }
  
  if (!suppliedElsewhere(sppColorVect, "sim")){
    sim$sppColorVect <- colors()[sample(x = 1:length(colors()), size = length(unique(LandR::sppEquivalencies_CA$KNN)), replace = FALSE)]
    names(sim$sppColorVect) <- unique(LandR::sppEquivalencies_CA[,P(sim)$sppEquivCol])[[1]]
  }
  return(invisible(sim))
}
