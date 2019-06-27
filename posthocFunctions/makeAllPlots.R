# Make all plots

# /!\ PUSH ALL NEW FUNCTIONS from functions folder!!!

setwd("/home/tmichele/Documents/GitHub/NWT")
library("reproducible")
library("SpaDES")
R.utils::sourceDirectory(file.path(getwd(), "functions"))

# ~~~~~~~~~~~~~~~~~~  LandR
biomassPerSpeciesYearGRAPH(pathData = file.path(getwd(), "outputs/19MAR19"), 
                           times = list(start = 0, end = 100),
                           version = "V3",
                           uploadFiles = FALSE)

# ~~~~~~~~~~~~~~~~~~  BIRDS
createBirdsGIFFromList(species = "",
                       pathData = "",
                       version = "V4",
                       uploadFiles = FALSE)

# createBirdsGIF(simList = birdsFireCaribouV2)
