# Generating plots for NWT project

biomassPerSpeciesYearGRAPH(pathData = file.path(getwd(), "outputs/03JUN19/"), 
                           times = list(start = 2001, end = 2100), 
                           version = "V3_CS", overwriteRasters = TRUE, 
                           uploadFiles = TRUE, whereToReport = "BCR6_NWT")

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
