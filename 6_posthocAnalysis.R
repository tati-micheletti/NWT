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
if (!exists("runPosthocBirds")) runPosthocBirds <- FALSE
if (runPosthocBirds){
  if (!exists("originalDateAnalysis")) originalDateAnalysis <- "SIMULATIONS" # Default if not provided 
  if (!exists("birdModelVersion")) birdModelVersion <- c("4", "6a") # Default if not provided 
  comparisons <- list(climate = c("V6a", "V4"),
                      vegetation = c("LandR.CS_", "LandR_"),
                      fire = c("fS", "SCFM"))
  # IMPORTANT: In the comparisons, always the climate sensitives need to come first!! This is
  # expected by the internal functions
}
# Structure of the results from the simulations
# originalDateAnalysis --> comparison --> runs --> birdModels
#originalDateAnalysis _____ LandR.CS_fS____ run1 _____ birdPredictionsV6a ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |              |                                     |    (...)
#                   |              |                                     |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |              |      (...)
#                   |              |
#                   |              |_____ run10 _____ birdPredictionsV6a ____ run1_LandR.CS_fSpredictedBIRDYear2011.tif
#                   |                                                   |    (...)
#                   |                                                   |____ run1_LandR.CS_fSpredictedBIRDYear2100.tif
#                   |
#                   |__ LandR_SCFM _____ run1  _____ birdPredictionsV4  ____ run1_LandR_SCFMpredictedBIRDYear2011.tif
#                                  |                                   |    (...)
#                                  |                                   |____ run1_LandR_SCFMpredictedBIRDYear2100.tif
#                                  |      (...)
#                                  |
#                                  |_____ run10 _____ birdPredictionsV4 ____ run1_LandR_SCFMpredictedBIRDYear2011.tif
#                                                                      |    (...)
#                                                                      |____ run1_LandR_SCFMpredictedBIRDYear2100.tif
#                                            

Species <- unique(sort(substrBoth(strng = substrBoth(strng = tools::file_path_sans_ext(
  grepMulti(x = basename2(list.files(path = file.path(dirname(Paths$outputPath), 
                                               originalDateAnalysis, 
                                               "LandR.CS_fS/run1/birdPredictionsV6a"), 
                                     recursive = TRUE)),
            patterns = "predicted")),
  howManyCharacters = 12,
  fromEnd = TRUE),
  howManyCharacters = 4,
  fromEnd = FALSE)))

######### ASSERTION #########

SpeciesNoClim <- unique(sort(substrBoth(strng = substrBoth(strng = tools::file_path_sans_ext(
  grepMulti(x = basename2(list.files(path = file.path(dirname(Paths$outputPath), 
                                                      originalDateAnalysis, 
                                                      "LandR_SCFM/run1/birdPredictionsV4"), 
                                     recursive = TRUE)),
            patterns = "predicted")),
  howManyCharacters = 12,
  fromEnd = TRUE),
  howManyCharacters = 4,
  fromEnd = FALSE)))

testthat::expect_true(all(Species %in% SpeciesNoClim))


#############################

# Shapefile to summarize
urlGNWTEcoregions <- "https://drive.google.com/file/d/1Cf--HP0Zq1_VziN_BLN7erBFHnzxvUsy/view?usp=sharing"
shpSummary <- prepInputs(url = urlGNWTEcoregions,
                        destinationPath = Paths$inputPath,
                        studyArea = studyArea,
                        filename2 = NULL,
                        userTags = c("objectName:shpSummary", stepCacheTag), 
                        omitArgs = c("destinationPath"))
#############################
runs <- paste0("run", 1:10)
TIME <- 1

parameters <- list(
  posthocBirdsNWT = list(
    "simulationStamp" = toupper(format(Sys.time(), "%d%b%y")),
    "species" = Species,
    "years" = c(seq(2011, 2091, by = 20), 2100), 
    "relativeDelta" = FALSE,
    "runs" = runs,
    "useFuture" = TRUE,
    "shpFieldToUse" = "ECO3_NAM_1",
    "eventsToSchedule" = c("makeSummary",
                           "averageThroughTimeComparison"),
    # "nBootReps" = 50 # To be uncommented if want bootstrapping. Not done yet.
    # "uploadPlots" = TRUE  # To be uncommented if want uploading. Not done yet.
    "birdModels" = c("V4", "V6a")
    # , overwriteBootstrap = TRUE
    )
  )

vegetationFireModels <- expand.grid(vegetation = c("LandR_", "LandR.CS_"), 
                                    fire = c("fS", "SCFM"))
vegetationFireModels <- paste0(vegetationFireModels$vegetation, vegetationFireModels$fire)

dataFolder <- lapply(vegetationFireModels, function(scenario){
    dataFolderRuns <- lapply(c("V4", "V6a"), function(birdModel){
      dataFolderRuns <- lapply(parameters[['posthocBirdsNWT']][['runs']], function(run){
        pth <- file.path(dirname(Paths$outputPath),
                          originalDateAnalysis,
                          scenario,
                          run,
                          paste0("birdPredictions", birdModel))
      return(pth)
    })
    names(dataFolderRuns) <- parameters[['posthocBirdsNWT']][['runs']]
    return(dataFolderRuns)
  })
  names(dataFolderRuns) <- c("V4", "V6a")
  return(dataFolderRuns)
})
names(dataFolder) <- vegetationFireModels

source("modules/posthocBirdsNWT/R/retrieveRasters.R")
listOfRasters <- retrieveRasters(dataFolder = dataFolder,
                                 years = c(seq(2011, 2091, by = 20), 2100),
                                 patternsToRetrieveRasters = c("predicted", ".tif"),
                                 species = Species)

objects <- list(
  "dataFolder" = dataFolder,
  "rasterToMatch" = rasterToMatch,
  "comparisons" = comparisons,
  "studyAreaPosthoc" = shpSummary,
  "listOfRasters" = listOfRasters,
  "predictedRastersFolder" = file.path(dirname(Paths$outputPath),
                                       originalDateAnalysis)
)

outputsBirds <- data.frame(
  objectName = c("pixelsSummaries",
                     "differenceRasters",
                     "colonizationRasters",
                     "deltaRasters",
                     "significantChanges",
                     "averageTimePlot",
                     "cummEffRas"),
  saveTime = TIME)

posthocBirdsAnalysis <- simInitAndSpades(times = list(start = TIME, end = TIME),
                                         params = parameters, 
                                         outputs = outputsBirds,
                                         modules = list("posthocBirdsNWT"),
                                         objects = objects,
                                         paths = Paths,
                                         loadOrder = "posthocBirdsNWT",
                                         debug = 1
                                         )
  