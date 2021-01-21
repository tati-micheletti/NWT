defineRun <- function(replicateNumber = NULL, vegetation = "LandR.CS", fire = "fS"){ # vegetation = "LandR"; fire = "SCFM"
  
 LandR <-c("Biomass_borealDataPrep",
           "PSP_Clean", # New parameters
           "Biomass_speciesParameters", # New parameters
           "Biomass_regeneration",
           "Biomass_core")
 
 LandR.CS <- c("Biomass_borealDataPrep",
               "PSP_Clean", # New parameters
               "Biomass_speciesParameters", # New parameters
               "Biomass_regeneration",
               "Biomass_core",
               # CS
               "gmcsDataPrep")
 
 fS <- c("fireSense_dataPrep",
         # "fireSense_SpreadFit", # Being fitted separately
                "fireSense_IgnitionPredict",
                "fireSense_EscapePredict",
                "fireSense_SpreadPredict",
                "LBMR2LCC_DataPrep",
                "fireSense")
 
 SCFM <- c("scfmLandcoverInit",
           "scfmRegime",
           "scfmDriver",
           "scfmIgnition",
           "scfmEscape",
           "scfmSpread")
 
  return(list(whichRUN = paste(vegetation, fire, sep = "_"),
              whichReplicate = replicateNumber,
              growthAndMortalityDrivers = vegetation,  
              modules = c(get(vegetation), get(fire))))
}
