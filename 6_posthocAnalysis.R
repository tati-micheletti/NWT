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
  if (all(!exists("originalDateAnalysis"),
          !(runLandR))) originalDateAnalysis <- "14AUG20" # Default if not provided
  if (!exists("birdModelVersion")) birdModelVersion <- c("4", "6a") # Default if not provided 
  comparisons <- list(climateChange = 
                        c("LandR_SCFM", 
                          "LandR.CS_fS")) # Not sure this works...
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
# runs <- paste0("run", 1:10) # Uncomment when testing is complete
runs <- paste0("run", 1:2)

parameters <- list(
  posthocBirdsNWT = list(
    "simulationStamp" = "Testing2", #toupper(format(Sys.time(), "%d%b%y"))
    # "species" = Species, # To be uncommented when the tests are over
    "species" = c("HOLA", "PAWA"), # To be removed when the tests are over
    # "years" = c(seq(2011, 2091, by = 20), 2100), # To be uncommented when the tests are over
    "years" = c(2011, 2051, 2100), # To be removed when the tests are over
    "relativeDelta" = FALSE,
    "runs" = runs,
    "useFuture" = FALSE,
    # "patternsUsedForGrouping" = paste0("run", 1:10), # To be uncommented when the tests are over
    # "nBootReps" = 50 # To be uncommented when the tests are over
    # "uploadPlots" = TRUE # To be uncommented when the tests are over
    "comparisonID" = "climateChangeEffects",
    "birdModels" = c("V4", "V6a")
    # , overwriteBootstrap = TRUE
    )
  )

dataFolder <- lapply(comparisons[['climateChange']], function(scenario){
  dataFolderRuns <- lapply(parameters[['posthocBirdsNWT']][['runs']], function(run){
                             birdModel <- ifelse(scenario == "LandR.CS_fS", 
                                                 "birdPredictionsV6a", 
                                                 "birdPredictionsV4")
                             pth <- file.path(dirname(Paths$outputPath),
                                              originalDateAnalysis,
                                              scenario,
                                              run,
                                              birdModel)
                             return(pth)
  })
  names(dataFolderRuns) <- parameters[['posthocBirdsNWT']][['runs']]
  return(dataFolderRuns)
})
names(dataFolder) <- comparisons[['climateChange']]

# Loading the bird rasters I want for comparison. As I am not interested in the predictions of 
# V6a with LandR_SCFM, I need to do this. If I was interested in that prediction, I would just 
# leave the birdRasters object as NULL

birdRasters <- getBirdPredictedRasters(parameters = parameters, 
                                       predictedRastersFolder = file.path(dirname(Paths$outputPath),
                                                                          originalDateAnalysis))

objects <- list(
  # "googleFolders" = "" # Pass where to upload the files to when finished testing
  "dataFolder" = dataFolder,
  "studyArea" = studyArea,
  "rasterToMatch" = rasterToMatch,
  "comparisons" = comparisons,
  "birdRasters" = birdRasters,
  "predictedRastersFolder" = file.path(dirname(Paths$outputPath),
                                       originalDateAnalysis)
)

# Maybe we should apply a treatment to the predicted rasters 
# to convert "virtual zeros" into zeros...

posthocBirdsAnalysis <- simInitAndSpades(times = list(start = 1, end = 1),
                                         params = parameters,
                                         modules = list("posthocBirdsNWT"),
                                         objects = objects,
                                         paths = Paths,
                                         loadOrder = "posthocBirdsNWT",
                                         debug = 1
                                         )
