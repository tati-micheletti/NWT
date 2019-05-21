# Generating plots for NWT project
# invisible(sapply(X = list.files(file.path(getwd(), "functions/"), 
#                                 full.names = TRUE), FUN = source))

biomassPerSpeciesYearGRAPH(pathData = file.path(getwd(), "Documents/GitHub/NWT/outputs/19MAR19"), 
                           times = list(start = 0, end = 100), version = "V3", overwriteRasters = TRUE, 
                           uploadFiles = FALSE, whereToReport = "Edehzhie")

createCaribouGIFFromList(pathData = file.path(getwd(), "outputs/28MAR19/"), 
                         uploadFiles = FALSE)

species <- c(
  "AMRE", "BLPW", "CAWA", "FOSP",
  "OVEN", "PAWA", "RCKI", "WCSP",
            "RUBL", "OSFL")
createBirdsGIFFromList(pathData = file.path(getwd(), "modules/birdsNWT/data"), 
                       species = species, 
                       version = "V3", uploadFiles = TRUE, whereToReport = "BCR6_NWT")

WhichToUp <- c("/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.0_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.50_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedAMREYear.100_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.0_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.50_30MAR19.png",
            "/mnt/data/Micheletti/NWT/outputs/PredictedOVENYear.100_30MAR19.png")

  lapply(WhichToUp, function(each){
    googledrive::drive_upload(each,
                              path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))
  })
