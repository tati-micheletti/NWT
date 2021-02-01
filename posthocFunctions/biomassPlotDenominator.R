biomassPlotDenominator <- function(year = 2011,
                         pathData,
                         pathOutputs,
                         Scenarios = c("LandR_SCFM", "LandR.CS_SCFM", 
                                       "LandR_fS", "LandR.CS_fS"),
                         runs = paste0("run", 1:10),
                         flammableRTM,
                         comparisons = list(vegetation = c("LandR.CS_", "LandR_"),
                                            fire = c("SCFM", "fS"),
                                            netEffect = c("LandR.CS_fS", "LandR_SCFM"))){
  Require::Require("reproducible")
  outputFolder <- checkPath(file.path(pathOutputs, "vegetationPlots"), create = TRUE)
  
  # 1. For each of below, make one map of total biomass
  # 2. Load cohort data for all runs (n=10), for all scenarios (n=4) = 40 maps
  # 3. Mix-match depending on the comparison, do the 
  # non-climate-sensitive - climate-sensitive (still in map format) per run (20 maps)
  # 4. Average these 20 maps and create one average difference map 
  
  allScenarios <- lapply(Scenarios, function(scenario){
    tic(paste0("Calculating biomass change for ", scenario))
    allRuns <- lapply(runs, function(RUN){
      # FOR YEAR 2011
        coh <- bringObjectTS(path = file.path(pathData, scenario, RUN),
                             rastersNamePattern = c("cohortData", year))[[1]]
        ras <- bringObjectTS(path = file.path(pathData, scenario, RUN), 
                             rastersNamePattern = c("pixelGroupMap", year))[[1]]
        # FOR EACH RUN, I NEED TO EXTRACT RASTERS OF BIOMASS FOR ALL SPECIES.
        cohortReduced <- coh[, list(sumBio = sum(B, na.rm = TRUE)), by = "pixelGroup"]
        biomassStack <- SpaDES.tools::rasterizeReduced(reduced = cohortReduced, 
                                                       fullRaster = ras,
                                                       newRasterCols = "sumBio", 
                                                       mapcode = "pixelGroup")
        biomassStack[is.na(biomassStack[])] <- 0
      
      names(biomassStack) <- paste("biomassMapDenomin", scenario, RUN, sep = "_")
      return(biomassStack)
    })
    names(allRuns) <- runs
    return(allRuns)
  })
  names(allScenarios) <- Scenarios
  allScenariosUnlisted <- unlist(allScenarios)
  allComparisons <- names(allScenariosUnlisted)
  
  # Now I need to mix-match the ones that are the "replicates" (i.e. average the combinations that
  # separate each one of the effects -- fire, vegetation and netEffect)
  factorialRasters <- lapply(names(comparisons), FUN = function(eachComparison){
    tic(paste0("Biomass denominator calculated for ", eachComparison))
    biomassDiffPath <- file.path(outputFolder, paste("difference",
                                                     eachComparison, "BiomassDenominator",
                                                     sep = "_"))
    climateDiffAverage <- calc(x = stack(allScenariosUnlisted), fun = mean,
                               filename = biomassDiffPath,
                               overwrite = TRUE,
                               format = "GTiff")
    climateDiffAverage[is.na(flammableRTM)] <- NA
    
    toc()
    return(climateDiffAverage)
  })
  names(factorialRasters) <- names(comparisons)
  return(factorialRasters)
}
