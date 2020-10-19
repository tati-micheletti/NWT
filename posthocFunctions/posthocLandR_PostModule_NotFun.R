############################################
############################################
#            TEMP POSTHOC SCRIPT           #  
############################################
############################################

runName <- "NWT_BCR6"
originalDateAnalysis <- "SIMULATIONS"
source("1_generalSetup.R")
library("data.table")
library("ggplot2")

SpaDES.core::setPaths(outputPath = file.path(getwd(), "outputs", originalDateAnalysis))

tableCombinations <- data.table(expand.grid(vegetation = c("LandR", "LandR.CS"),
                                            fire = c("SCFM", "fS")))
allTypeSim <- copy(tableCombinations)
allTypeSim[, typeSim := paste(vegetation, fire, sep = "_")]
allTypeSim <- allTypeSim$typeSim

# 1. Average the biomass of runs per year and per scenario so I can use this averaged raster below
source("functions/getSppColorVect_NWT.R")
source("posthocFunctions/totalBiomassPerSpeciesReps.R")
biomassTables <- lapply(allTypeSim, function(typeSim){
  biomassTables <- totalBiomassPerSpeciesReps(dataPath = Paths$outputPath,
                                              typeSim = typeSim,
                                              sppColorVect = getSppColorVect_NWT())
})

source('~/projects/NWT/posthocFunctions/loadBiomassLayers.R')
ras <- loadBiomassLayers(scenarios = tableCombinations, 
                         path = Paths$outputPath, years = c(2011, 2100))
source('~/projects/NWT/posthocFunctions/fixNAsLastYear.R')
ras <- fixNAsLastYear(rasterList = ras, years = c(2011, 2100))

nFailedRegeneration <- data.table(scenario = names(ras), 
                                  failedRegeneration = unlist(lapply(ras, 
                                                                     function(scenario){
                                                                       attr(scenario[["year2100"]], 
                                                                            "totalNAto0")})))
# Climate effects: fire and vegetation
source('~/projects/NWT/posthocFunctions/calculateDifferences.R')
climateEffect <- calculateDifferences(effects = c("vegetation", "fire"), 
                                               rasList = ras, 
                                               years = c(2011, 2100), 
                                               limits = 0.05)
netClimateEffect <- calculateDifferences(effects = c("extremes"), 
                                      rasList = ras, 
                                      years = c(2011, 2100), 
                                      limits = 0.05)

source('~/projects/NWT/posthocFunctions/makeHistogramClimateEffect.R')
climEffectHist <- makeHistogramClimateEffect(fireDiff = climateEffect$fireDifferenceRaster, 
                                             vegetationDiff = climateEffect$vegetationDifferenceRaster)
# Averaged climate effects on Biomass
mean(c(climateEffect$meanDifferenceVegetation, 
       climateEffect$meanDifferenceFire))

netClimateEffect$meanDifferenceClimate

# Failed regeneration
par(mfrow = c(1, 4))
# Climate Sensitive Vegetation
failedRegeneration_LandR.CS_fS <- raster(ras[["LandR.CS_fS"]][["year2011"]])
failedRegeneration_LandR.CS_fS[attr(ras[["LandR.CS_fS"]][["year2100"]], 
                                    "indexNA")] <- 1
plot(failedRegeneration_LandR.CS_fS, col = "red", 
     main = "LandR.CS + fireSense", sub = paste0(attr(ras[["LandR.CS_fS"]][["year2100"]], 
                                                      "totalNAto0"), " pixels"))

failedRegeneration_LandR.CS_SCFM <- raster(ras[["LandR.CS_SCFM"]][["year2011"]])
failedRegeneration_LandR.CS_SCFM[attr(ras[["LandR.CS_SCFM"]][["year2100"]], 
                                      "indexNA")] <- 1
plot(failedRegeneration_LandR.CS_SCFM, col = "red", 
     main = "LandR.CS + SCFM", sub = paste0(attr(ras[["LandR.CS_SCFM"]][["year2100"]], 
                                                 "totalNAto0"), " pixels"))

# Non climate Sensitive Vegetation
failedRegeneration_LandR_fS <- raster(ras[["LandR_fS"]][["year2011"]])
failedRegeneration_LandR_fS[attr(ras[["LandR_fS"]][["year2100"]], 
                                    "indexNA")] <- 1
plot(failedRegeneration_LandR_fS, col = "red", 
     main = "LandR + fireSense", sub = paste0(attr(ras[["LandR_fS"]][["year2100"]], 
                                                   "totalNAto0"), " pixels"))

failedRegeneration_LandR_SCFM <- raster(ras[["LandR_SCFM"]][["year2011"]])
failedRegeneration_LandR_SCFM[attr(ras[["LandR_SCFM"]][["year2100"]], 
                                      "indexNA")] <- 1
plot(failedRegeneration_LandR_SCFM, col = "red", 
     main = "LandR + SCFM", sub = paste0(attr(ras[["LandR_SCFM"]][["year2100"]], 
                                              "totalNAto0"), " pixels"))
par(mfrow = c(1, 1))

# Fire summaries
source('~/projects/NWT/posthocFunctions/plotBurnSummaryReps.R')
burns_LandR.CS_fS <- plotBurnSummaryReps(dataPath = Paths$outputPath, 
                                  typeSim = "LandR.CS_fS",
                                  lastYear = 2100, overwrite = TRUE)
burns_LandR_fS <- plotBurnSummaryReps(dataPath = Paths$outputPath,
                                typeSim = "LandR_fS",
                                lastYear = 2100, overwrite = TRUE)
burns_LandR.CS_SCFM <- plotBurnSummaryReps(dataPath = Paths$outputPath, 
                                  typeSim = "LandR.CS_SCFM",
                                  lastYear = 2100, overwrite = TRUE)
burns_LandR_SCFM <- plotBurnSummaryReps(dataPath = Paths$outputPath,
                                typeSim = "LandR_SCFM",
                                lastYear = 2100, overwrite = TRUE)
