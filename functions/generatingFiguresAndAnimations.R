# Generating plots for NWT project
invisible(sapply(X = list.files(file.path(getwd(), "Documents/GitHub/NWT/functions/"), 
                                full.names = TRUE), FUN = source))

biomassPerSpeciesYearGRAPH(pathData = file.path(getwd(), "Documents/GitHub/NWT/outputs/19MAR19"), 
                           times = list(start = 0, end = 100), version = "V3", overwriteRasters = TRUE, 
                           uploadFiles = FALSE, whereToReport = "Edehzhie")

createCaribouGIFFromList(pathData = file.path(getwd(), "outputs/28MAR19/"), 
                         uploadFiles = FALSE)

createBirdsGIFFromList(pathData = file.path(getwd(), "outputs/28MAR19/"), 
                       species = c("AMRE", "BLPW", "CAWA", "FOSP", 
                                   "OVEN", "PAWA", "RCKI", "RUBL", "WCSP", "OSFL"), 
                       version = "V3", uploadFiles = FALSE)