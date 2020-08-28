#########################################################
##                  P O S T H O C                      ##
#########################################################

stepCacheTag <- c(paste0("cache:6_posthocAnalysis"), 
                  paste0("runName:", runName))

SpaDES.core::setPaths(cachePath = posthocCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       "posthoc"),
                                             create = TRUE))

#########################################################
##                    B I R D S                        ##
#########################################################

if (runPosthocBirds){
  if (!exists("originalDateAnalysis")) originalDateAnalysis <- "14AUG20" # Default if not provided
  
  comparisons <- list(climateChange = 
                        c("LandR_SCFM", 
                          "LandR.CS_fS")) # Not sure this works...
  if (!exists("birdModelVersion")) birdModelVersion <- c("4", "6a") # Default if not provided
  
}
# Need to pass all down to the module. I only need :
# pathToResults ("14AUG20")
# names of the folders I want to compare (LandR.CS_fS, LandR_SCFM)
# Bird models (V4, V6a)
# 
# The module should know I have the following structure
#
#originalDateAnalysis _____ LandR.CS_fS_____run1_____ birdPredictionsV4 ___ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |              |           |                       |    (...)
#                   |              |           |                       |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |              |           | 
#                   |              |           |_____ birdPredictionsV6a ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |              |                                    |    (...)
#                   |              |                                    |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |              |  
#                   |              |      (...)
#                   |              |
#                   |              |_____ run10 _____ birdPredictionsV4  ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |                          |                        |    (...)
#                   |                          |                        |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |                          |
#                   |                          |_____ birdPredictionsV6a ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |                                                  |    (...)
#                   |                                                  |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |
#                   |__ LandR_SCFM _____ run1  _____ birdPredictionsV4  ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                                 |           |                        |    (...)
#                                 |           |                        |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                                 |           |
#                                 |           |_____ birdPredictionsV6a ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                                 |                                    |    (...)
#                                 |                                    |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif


if (!exists("compat")) pathToBirdModelsCC <- "LandR.CS_fS/run1/birdPredictionsV6a" # Default 
if (!exists("pathToBirdModelsNoCC")) pathToBirdModelsNoCC <- "LandR_SCFM/run1/birdPredictionsV4" # Default 
if (!exists("pathToResults")) pathToResults <- file.path(getwd(), "outputs", 
                                                                   originalDateAnalysis) # Default


Species <- unique(sort(substrBoth(strng = substrBoth(strng = tools::file_path_sans_ext(
  grepMulti(x = basename2(list.files(file.path(pathToResults,
                                               pathToBirdModelsCC))),
            patterns = "predicted")),
  howManyCharacters = 12,
  fromEnd = TRUE),
  howManyCharacters = 4,
  fromEnd = FALSE)))

######### ASSERTION #########

SpeciesNoClim <- unique(sort(substrBoth(strng = substrBoth(strng = tools::file_path_sans_ext(
  grepMulti(x = basename2(list.files(file.path(pathToResults,
                                               pathToBirdModelsNoCC))),
            patterns = "predicted")),
  howManyCharacters = 12,
  fromEnd = TRUE),
  howManyCharacters = 4,
  fromEnd = FALSE)))

testthat::expect_true(Species %in% SpeciesNoClim)

#############################

source('/mnt/data/Micheletti/NWT/posthocFunctions/makeDiffAnalysis2.R')
plts <- future_lapply(seq_along(comparisons), function(index){ #future_
  pl <- makeDiffAnalysis2(resultsFolder = resultsFolder, # Should test makeDiffAnalysis2 in this one too!!
                          Run = paste0("run", 1:10),
                          Species = Species,
                          Year = c(seq(2011, 2091, by = 20), 2100),
                          typeOfSpecies = "bird",
                          Scenario = c("LandR.CS_fS", "LandR_SCFM"),
                          SpeciesScenario = c("V4", "V6a"),
                          comparisons = comparisons[index], 
                          writeRas = TRUE 
                          # , overwrite = TRUE
  )
})
source(file.path(getwd(), '/posthocFunctions/makeAveragePlotTime.R'))
pth <- file.path(resultsFolder, "effectsRasters")
birds <- Species #c("CAWA", "OSFL", "RUBL")
scenarios <- c("climate", "fire", "vegetation")
# shp <- prepInputs(url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV", 
#                   destinationPath = pth, 
#                   filename2 = "caribouArea2")
studyArea <- prepInputs(url = runNamesList()[RunName == runName, studyArea],
                        destinationPath = Paths$inputPath,
                        filename2 = NULL,
                        userTags = c("objectName:studyArea", stepCacheTag), 
                        omitArgs = c("destinationPath", "filename2"))

shp <- Cache(prepInputs, 
             targetFile = "ecoregions.shp",
             archive = "ecoregion_shp.zip",
             url = "http://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
             alsoExtract = "similar",
             destinationPath = Paths$inputPath,
             studyArea = studyArea,
             useSAcrs = TRUE, # this is required to make ecoZone be in CRS of studyArea
             fun = "raster::shapefile",
             # filename2 = TRUE,
             userTags = c("prepInputsEcoRegion_SA", "where:fromGlobal"), # use at least 1 unique userTag
             omitArgs = c("destinationPath", "targetFile", "overwrite", "alsoExtract", "userTags"))
plt <- makeAveragePlotTime(dataFolder = pth, 
                           years =  c(seq(2011, 2091, by = 20), 2100),
                           Species = birds,
                           scenarios = scenarios, 
                           shp = shp)

# BIRDS!!
listOfRasters <- lapply(runs, function(RUN){
  listOfRasterPaths <- list(CAWA = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                                      paste0(RUN, "_", noCC[1],"predictedCAWAYear", yearToCompare,".tif")))), #filename
                                         raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                                      paste0(RUN, "_", CC[1],"predictedCAWAYear", yearToCompare,".tif"))))),
                            OSFL = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                                      paste0(RUN, "_", noCC[1],"predictedOSFLYear", yearToCompare,".tif")))), #filename
                                         raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                                      paste0(RUN, "_", CC[1],"predictedOSFLYear", yearToCompare,".tif"))))),
                            RUBL = stack(raster(file.path(pth, paste0(noCC[1], "/", RUN, "/birdPredictions", noCC[2], "/", #Path
                                                                      paste0(RUN, "_", noCC[1],"predictedRUBLYear", yearToCompare,".tif")))), #filename
                                         raster(file.path(pth, paste0(CC[1], "/", RUN, "/birdPredictions", CC[2], "/", #Path
                                                                      paste0(RUN, "_", CC[1],"predictedRUBLYear", yearToCompare,".tif"))))))
  
  names(listOfRasterPaths) <- c("CAWA", "OSFL", "RUBL")
  return(listOfRasterPaths)
})
names(listOfRasters) <- paste0("cumulativeEffect_", runs)

foldID <- as.list(c(rep("1ymCZq7cPfXB2hA6rDpRd6J3lYkioQJxZ", times = 5)))
names(foldID) <- paste0("cumulativeEffect_", runs)

source('/mnt/data/Micheletti/NWT/modules/rastersPosthoc/R/makeDeltaRasters.R')
dRas <- makeDeltaRasters(listOfRasters = listOfRasters,
                         relativeDelta = FALSE,
                         outputFolder = file.path(pth, "effectsRasters"),
                         lightLoad = TRUE,
                         overwrite = FALSE,
                         upload = TRUE,
                         folderID = foldID,
                         email = "tati.micheletti@gmail.com")

source('/mnt/data/Micheletti/NWT/posthocFunctions/createCumEffRasters.R')
createCumEffRasters(species = c("CAWA", "OSFL", "RUBL"),
                    rasFolder = "/mnt/data/Micheletti/NWT/outputs/PAPER/effectsRasters",
                    googlefolderID = "1ymCZq7cPfXB2hA6rDpRd6J3lYkioQJxZ")

