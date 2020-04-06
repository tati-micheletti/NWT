birdsOutputsFolder <- "/mnt/data/Micheletti/NWT/outputs/2019/18JUL19/birdPredictionsV3" # 2011 and 2100 (convert 2021 into 2011)
caribouOutputsFolder <- "/mnt/data/Micheletti/NWT/outputs/2019/18JUN19_CS_SCFM/caribouRSF" #  2011 and 2100
library("usefun")

# Birds VERSION3: birdsOutputsFolder
allBirdsPaths <- list.files(path = birdsOutputsFolder, pattern = "2021.tif|2100.tif", full.names = TRUE)
allBirdsPaths2011 <- grepMulti(x = allBirdsPaths, patterns = "2021")
allBirdsNames <- usefun::substrBoth(usefun::substrBoth(strng = tools::file_path_sans_ext(allBirdsPaths2011), 
                                    howManyCharacters = 12, fromEnd = TRUE), howManyCharacters = 4, fromEnd = FALSE) 
allBirdsPaths2100 <- grepMulti(x = allBirdsPaths, patterns = "2100")
birds2011 <- lapply(X = 1:length(allBirdsPaths2011), FUN = function(index){
  ras <- raster::raster(allBirdsPaths2011[[index]])
  ras[] <- ras[]
  names(ras) <- paste0("predicted", allBirdsNames[[index]], "2011")
  return(ras)
})
birds2100 <- lapply(X = 1:length(allBirdsPaths2100), FUN = function(index){
  ras <- raster::raster(allBirdsPaths2100[[index]])
  ras[] <- ras[]
  names(ras) <- paste0("predicted", allBirdsNames[[index]], "2100")
  return(ras)
})
names(birds2011) <- names(birds2100) <- allBirdsNames

birdPrediction <- list(Year2011 = birds2011,
                       Year2100 = birds2100)

# Caribou VERSION3: caribouOutputsFolder
caribouPaths <- list.files(path = caribouOutputsFolder, pattern = "2011.tif|2100.tif", full.names = TRUE)
allCaribouPaths2011 <- grepMulti(x = caribouPaths, patterns = "Year2011.tif")[1]
allCaribouPaths2100 <- grepMulti(x = caribouPaths, patterns = "Year2100.tif")[1]
caribous2011 <- lapply(X = 1:length(allCaribouPaths2011), FUN = function(index){
  ras <- raster::raster(allCaribouPaths2011[[index]])
  ras[] <- ras[]
  names(ras) <- paste0("predictedcaribou2011")
  return(ras)
})
caribous2100 <- lapply(X = 1:length(allCaribouPaths2100), FUN = function(index){
  ras <- raster::raster(allCaribouPaths2100[[index]])
  ras[] <- ras[]
  names(ras) <- paste0("predictedcaribou2100")
  return(ras)
})
names(caribous2011) <- names(caribous2100) <- "predictedcaribou"

predictedPresenceProbability <- list(Year2011 = caribous2011,
                                     Year2100 = caribous2100)
