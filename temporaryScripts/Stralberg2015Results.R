# How our results compare to Stralberg et al. 2015 
# IMPORTANT: These respond to different spatial scale, so are not directly comparable. 

library(data.table)
library(reproducible)

ourResults <- prepInputs(url = "https://drive.google.com/file/d/1sNWmBXIzCkBKoHq_eYIiVgheQ5UD7Vav/view?usp=sharing",
                         targetFile = "proportionsTableSUPMAT.csv",
                         destinationPath = tempdir(),
                         fun = "data.table::fread")

Species <- prepInputs(url = "https://drive.google.com/file/d/1vfI_diZRiAllVE8juuxaMDOsGlLKas4i/view?usp=sharing",
                      targetFile = "allSpeciesNames.csv",
                      destinationPath = tempdir(),
                      fun = "data.table::fread")
names(Species) <- c("species", "commonName")

stralberg <- prepInputs(url = "https://drive.google.com/file/d/1B5dJb-W-e_yR7wq9WDwg_9Nt3-RkXsoz/view?usp=sharing",
                        targetFile = "stralbergRefugiaResults.csv",
                        destinationPath = tempdir(),
                        fun = "data.table::fread")

fullOurResults <- merge(ourResults, Species, on = "commonName")
fullOurResults[, V1 := NULL]
fullResults <- stralberg[fullOurResults, on = "commonName"]
toKeep <- c("commonName", "modifiedRefugia", "proportionOfAreaChanged")
fullResults <- fullResults[, ..toKeep]
fullResults <- na.omit(fullResults)

