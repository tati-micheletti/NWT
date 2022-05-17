# SCFM parameters and more (from Ian's global)

# #scfm global parameters
# defaultInterval <- 1.0
# defaultPlotInterval <- 1.0
# defaultInitialSaveTime <- NA #don't be saving nuffink

# shpStudyRegionFull <- shapefile("C:/Ian/Tati/Edehzhie/Edehzhie_subset.shp")

# rasterToMatch <- reproducible::Cache(pemisc::prepInputsLCC, 
#                        studyArea = studyArea, year = 2005, 
#                        filename2 = "rasterToMatch.tif", overwrite = TRUE)
# rasterToMatch <- raster("rasterToMatch.tif") #debugging cache
# studyArea <- spTransform(x = studyArea, CRSobj = crs(rasterToMatch))
# studyAreaLarge <- spTransform(x = studyAreaLarge, CRSobj = crs(rasterToMatch))

# PARAMETERS
# BiomassSpeciesData = list( #proprietary data
#   .useCache = TRUE,
#   omitNonTreePixels = FALSE),

#SCFM MODULES
# ageModule = list(
#   initialAge = 100,
#   maxAge = 200,
#   returnInterval = defaultInterval,
#   startTime = times$start,
#   .plotInitialTime =  NA,
#   .plotInterval = defaultPlotInterval,
#   .saveInitialTime = defaultInitialSaveTime,
#   .saveInterval = defaultInterval),
# scfmIgnition = list(
#   pIgnition = 0.0001,
#   returnInterval = defaultInterval,
#   startTime = times$start,
#   .plotInitialTime = NA,
#   .plotInterval = defaultPlotInterval,
#   .saveInitialTime = defaultInitialSaveTime,
#   .saveInterval = defaultInterval),
# scfmEscape = list(
#   p0 = 0.05,
#   returnInterval = defaultInterval,
#   startTime = times$start,
#   .plotInitialTime = NA,
#   .plotInterval = defaultPlotInterval,
#   .saveInitialTime = defaultInitialSaveTime,
#   .saveInterval = defaultInterval),
# scfmSpread = list(
#   pSpread = 0.235,
#   returnInterval = defaultInterval,
#   startTime = times$start,
#   .plotInitialTime = times$start,
#   .plotInterval = defaultPlotInterval,
#   .saveInitialTime = defaultInitialSaveTime,
#   .saveInterval = defaultInterval),
# scfmRegime = list(fireCause=c("L", "H"))

# myObjectSynonyms <- list(c("LCC2005", "vegMap")) # I assume this is for SCFM ~TM
