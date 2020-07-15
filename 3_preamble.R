############################################
############################################
#            P r e a m b l e               #  
############################################
############################################

# The preamble will run modules that will produce needed inputs for the simulation.
# The first module to run is 

stepCacheTag <- c(paste0("cache:3_preamble"), 
                  paste0("runName:", runName))

SpaDES.core::setPaths(cachePath = preambleCache,
                      outputPath = Paths$inputPath)
getPaths()

if (!exists("prepCohortData")) prepCohortData <- FALSE
# Preamble. If already ran (i.e. objs pxlGrpMap+cohortData2011, spLays+pxlGrpMp+cohortData2001
# exist in inputs folder) this should NOT be run i.e. FALSE)
if (prepCohortData){
  message("prepCohortData is TRUE. Running preamble.")
outputsPreamble2001 <- data.frame(objectName = c("cohortData", 
                                             "pixelGroupMap",
                                             "speciesLayers",
                                             "standAgeMap",
                                             "rawBiomassMap"),
                              saveTime = 2001,
                              file = c("cohortData2001_fireSense.rds",
                                       "pixelGroupMap2001_fireSense.rds",
                                       "speciesLayers2001_fireSense.rds",
                                       "standAgeMap2001_borealDataPrep.rds",
                                       "rawBiomassMap2001_borealDataPrep.rds"))

# 1. Run borealBiomassDataPrep ALONE and save: cohortData + pixelGroupMap: will be used
# in fireSense_SizeFit and fireSense_SpreadFit (later on, will be also used in Ignition and 
# Escape fits)
# 271 unique using ecoregion
# 973 unique using ecodistrict

biomassMaps2001 <- simInitAndSpades(times = list(start = 2001, end = 2001),
                         params = parameters,
                         modules = list("Biomass_borealDataPrep"),
                         objects = objects,
                         paths = getPaths(),
                         loadOrder = "Biomass_borealDataPrep",
                         outputs = outputsPreamble2001,
                         userTags = c(stepCacheTag, 
                                      "objective:preambleBiomassDataPrep",
                                      "time:year2001", "version:fixedZeros"))
# 2. Load these:
# devtools::load_all(file.path(dirname(getwd()), "LandR"))
speciesLayers2011 <- loadkNNSpeciesLayersValidation(dPath = Paths$inputPath,
                           rasterToMatch = rasterToMatch,
                           studyArea = studyArea, 
                           sppEquiv = sppEquivalencies_CA,
                           knnNamesCol = "KNN",
                           sppEquivCol = sppEquivCol,
                           thresh = 10,
                           url = paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                                        "canada-forests-attributes_attributs-forests-canada/2011-",
                                        "attributes_attributs-2011/"))#,
                           # userTags = c(stepCacheTag, "preamble", 
                                        # "speciesLayers2011"))

outputsPreamble2011 <- data.frame(objectName = c("cohortData", 
                                             "pixelGroupMap", 
                                             "speciesLayers",
                                             "standAgeMap",
                                             "rawBiomassMap"),
                              saveTime = 2011,
                              file = c("cohortData2011_fireSense.rds", 
                                       "pixelGroupMap2011_fireSense.rds",
                                       "speciesLayers2011_fireSense.rds",
                                       "standAgeMap2011_borealDataPrep.rds",
                                       "rawBiomassMap2011_borealDataPrep.rds")) # Currently not needed
objectsPre <- objects
objectsPre$speciesLayers <- speciesLayers2011

# and pass as object to a second call of Biomass_borealDataPrep. Save cohortData + pixelGroupMap.
biomassMaps2011 <- simInitAndSpades(times = list(start = 2011, end = 2011),
                         params = parameters,
                         modules = list("Biomass_borealDataPrep"),
                         objects = objectsPre,
                         paths = getPaths(),
                         loadOrder = "Biomass_borealDataPrep",
                         clearSimEnv = TRUE,
                         outputs = outputsPreamble2011,
                         userTags = c(stepCacheTag, 
                                      "objective:preambleBiomassDataPrep", 
                                      "time:year2011"))

cohortData2001 <- biomassMaps2001[["cohortData"]]
pixelGroupMap2001 <- biomassMaps2001[["pixelGroupMap"]]
speciesLayers2001 <- biomassMaps2001[["speciesLayers"]]
cohortData2011 <- biomassMaps2011[["cohortData"]]
pixelGroupMap2011 <- biomassMaps2011[["pixelGroupMap"]]
rawBiomassMap2001 <- biomassMaps2001[["rawBiomassMap"]] 
standAgeMap2001 <- biomassMaps2001[["standAgeMap"]] 

} else {
  cohortData2001 <- readRDS(file.path(Paths$inputPath, "cohortData2001_fireSense_year2001.rds"))
  pixelGroupMap2001 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2001_fireSense_year2001.rds"))
  cohortData2011 <- readRDS(file.path(Paths$inputPath, "cohortData2011_fireSense_year2011.rds"))
  pixelGroupMap2011 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2011_fireSense_year2011.rds"))
  speciesLayers2001 <- readRDS(file.path(Paths$inputPath, "speciesLayers2001_fireSense_year2001.rds"))
  standAgeMap2001 <- readRDS(file.path(Paths$inputPath, "standAgeMap2001_borealDataPrep_year2001.rds"))
  rawBiomassMap2001 <- readRDS(file.path(Paths$inputPath, "rawBiomassMap2001_borealDataPrep_year2001.rds"))
}

##################################### Update objects

objects <- c(objects, list(
  "standAgeMap" = standAgeMap2001,
  "rawBiomassMap" = rawBiomassMap2001,
  "speciesLayers" = speciesLayers2001,
  "cohortData2001" = cohortData2001,
  "cohortData2011" = cohortData2011,
  "pixelGroupMap2001" = pixelGroupMap2001,
  "pixelGroupMap2011" = pixelGroupMap2011)
)

