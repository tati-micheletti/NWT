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
                           overwrite = TRUE,
                           useCache = "overwrite",
                           paths = getPaths(),
                           loadOrder = "Biomass_borealDataPrep",
                           outputs = outputsPreamble2001)#,
  # 06JAN21: Having some sort of caching problem...
                           # userTags = c(stepCacheTag,
                           #              "objective:preambleBiomassDataPrep",
                           #              "time:year2001",
                           #              "version:fixedZeros"))
  # 2. Load these:
  speciesLayers2011 <- loadkNNSpeciesLayersValidation(
                             dPath = Paths$inputPath,
                             rasterToMatch = rasterToMatch,
                             studyArea = studyArea, 
                             overwrite = TRUE,
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
                                      useCache = "overwrite",
                                      paths = getPaths(),
                                      loadOrder = "Biomass_borealDataPrep",
                                      outputs = outputsPreamble2011)
  
  cohortData2001 <- biomassMaps2001[["cohortData"]]
  pixelGroupMap2001 <- biomassMaps2001[["pixelGroupMap"]]
  speciesLayers2011 <- biomassMaps2011[["speciesLayers"]]
  cohortData2011 <- biomassMaps2011[["cohortData"]]
  pixelGroupMap2011 <- biomassMaps2011[["pixelGroupMap"]]
  rawBiomassMap2011 <- biomassMaps2011[["rawBiomassMap"]] 
  standAgeMap2011 <- biomassMaps2011[["standAgeMap"]] 
  
} else {
  cohortData2001 <- readRDS(file.path(Paths$inputPath, "cohortData2001_fireSense_year2001.rds"))
  pixelGroupMap2001 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2001_fireSense_year2001.rds"))
  cohortData2011 <- readRDS(file.path(Paths$inputPath, "cohortData2011_fireSense_year2011.rds"))
  pixelGroupMap2011 <- readRDS(file.path(Paths$inputPath, "pixelGroupMap2011_fireSense_year2011.rds"))
  speciesLayers2011 <- readRDS(file.path(Paths$inputPath, "speciesLayers2011_fireSense_year2011.rds"))
  standAgeMap2011 <- readRDS(file.path(Paths$inputPath, "standAgeMap2011_borealDataPrep_year2011.rds"))
  rawBiomassMap2011 <- readRDS(file.path(Paths$inputPath, "rawBiomassMap2011_borealDataPrep_year2011.rds"))
}

##################################### Update objects

# I will "improve" species layer with EOSD by using the  deciduous leading and conifer leading.
#  While total pixel biomass is kept constant, the proportions are adjusted based on 
#  the EOSD class. 
#  If deciduous leading, we set/adjust deciduous biomass to 75% of the total biomass.
if (!exists("adjustSpeciesLayer")) adjustSpeciesLayer <- TRUE
if (adjustSpeciesLayer){
  message(crayon::yellow("speciesLayer being adjusted based on land cover layer..."))
  speciesLayers2011 <- Cache(adjustSpeciesLayersWithEOSD, 
                             rstLCC = rstLCC,
                             speciesLayers = speciesLayers2011, 
                             userTags = "updateSpeciesLayersWithEOSD")
}

objects <- c(objects, list(
  "standAgeMap" = standAgeMap2011,
  "rawBiomassMap" = rawBiomassMap2011,
  "speciesLayers" = speciesLayers2011,
  "cohortData2001" = cohortData2001,
  "cohortData2011" = cohortData2011,
  "pixelGroupMap2001" = pixelGroupMap2001,
  "pixelGroupMap2011" = pixelGroupMap2011)
)

