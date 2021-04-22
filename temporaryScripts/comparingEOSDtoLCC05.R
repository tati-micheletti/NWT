# Comparing EOSD to LCC05

require("rgdal")
require("rgeos")
require("dplyr")
require("raster")
require("reproducible")

RUN <- "TEST"
usrEmail <- "tati.micheletti@gmail.com" # Your e.mail for GDrive authorization
runName <- "NWT_BCR6"
source("1_generalSetup.R")
source("2_generatingInputs.R")

# > Paths$outputPath
# [1] "/home/tmichele/projects/NWT/outputs/19NOV20/LandR_SCFM/run1"

LCC05 <- LandR::prepInputsLCC(year = 2005, destinationPath = Paths$inputPath, 
                              rasterToMatch = rasterToMatch, 
                              studyArea = studyArea)

EOSDlayer <- Cache(prepInputs, url = "https://drive.google.com/file/d/1XTZTEaZUQQcddYsSUOUvLDUvOfA2fXi2/view?usp=sharing",
                   targetFile = "EOSD_RSFclass.tif", 
                   alsoExtract = "similar", rasterToMatch = rasterToMatch, 
                   studyArea = studyArea,
                   destinationPath = Paths$inputPath, 
                   fun = "raster::raster",
                   .tempPath = scratchDir,
                   userTags = "EOSD")

studyAreaUnified <- gUnaryUnion(studyArea, id = studyArea@data$COUNTRY)

plot(EOSDlayer)
plot(studyAreaUnified, add = TRUE, border = "red", lwd = 2)

LCC05cropped <- postProcess(LCC05, rasterToMatch = EOSDlayer, maskWithRTM = TRUE)

# Get land cover classification for EOSD
RSFclassTable <- prepInputs(url = "https://drive.google.com/file/d/1-aZKyu6ROtOcI_3ixAIOj-Smj75LjJn3/view?usp=sharing",
                            destinationPath = Paths$inputPath, 
                            fun = "data.table::fread")

firstFilter <- RSFclassTable[RSF_class %in% c("Deciduous", "Treed Wetland", "Dense Conifer", 
                               "Open Conifer", "Sparse Conifer", "Mixedwood")]

secondFilter <- firstFilter[!CovType %in% c("Shrub Tall", "Shrub Low"),]

forestValuesInLCC <- c(1:15, 20, 32, 34:35)
forestValuesInEOSDlayer <- secondFilter[["Value"]]
  
DT <- na.omit(data.table(EOSD = getValues(EOSDlayer), 
                         LCC05 = getValues(LCC05cropped)))

DT[, forestEOSD := EOSD %in% forestValuesInEOSDlayer]
DT[, forestLCC05 := LCC05 %in% forestValuesInLCC]

DT[, forestInEOSDnonForestInLCC05 := forestEOSD == TRUE & forestLCC05 != TRUE]

totalPixelsWeAreMissing <- sum(DT[["forestInEOSDnonForestInLCC05"]])
percentagePixelsMissed <- 100*(totalPixelsWeAreMissing/NROW(DT))

# > percentagePixelsMissed
# [1] 8.135776