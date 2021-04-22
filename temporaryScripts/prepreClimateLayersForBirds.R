
t1 <- Sys.time()
source('/mnt/data/Micheletti/usefun/R/prepareClimateLayers.R')
sim$MDC06 <- prepareClimateLayers(authEmail = "tati.micheletti@gmail.com",
                                                       pathInputs = getPaths()$inputPath, studyArea = studyArea,
                                                       rasterToMatch = rasterToMatch,
                                                       variables = "fireSense", model = "fireSense", 
                                  returnCalculatedLayersForFireSense = TRUE)
t2 <- Sys.time()

birdsClimateStack <- prepareClimateLayers(authEmail = "tati.micheletti@gmail.com", 
                                          pathInputs = getPaths()$inputPath, studyArea = studyArea,
                                          rasterToMatch = rasterToMatch,
                                          variables = "birdsModel", model = "birds")
t2 <- Sys.time()

# HERE <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# IMPLEMENT THE FUNCTION INTO FIRE SENSE (fireSense_NWT_DataPrep)







# FIXING .7z
# googledrive::drive_auth(email = "tati.micheletti@gmail.com")
# library(raster)
# library(reproducible)
# test7zPath <- "https://drive.google.com/open?id=1Mg308YKPaVjEoLFjDpII92gWKblZKR0s" # .7z file
# # test7zPath <- "https://drive.google.com/open?id=1AXNgQ77OMcsMHifD_XOnR6L5sUGffvXC" # .zip file
# 
# test7zPRE <- reproducible::preProcess(url = test7zPath, filename2 = NULL,
#                                    targetFile = "stackTestGrd.grd", alsoExtract = "stackTestGrd.gri",
#                                    destinationPath = tempdir())
# list.files(tempdir())
