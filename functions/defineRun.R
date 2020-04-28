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
 
 fS <- c("fireSense_NWT_DataPrep",
                "fireSense_IgnitionPredict",
                "fireSense_EscapePredict",
                "LBMR2LCC_DataPrep",
                "fireSense",
                "fireSense_SpreadPredict")
 
 SCFM <- c("scfmLandcoverInit",
           "scfmRegime",
           "scfmDriver",
           "scfmIgnition",
           "scfmEscape",
           "scfmSpread")
 
  return(list(whichRUN = paste(vegetation, fire, sep = "_"),
              whichReplicate = replicateNumber,
              growthAndMortalityDrivers = vegetation,  
              modules = c(get(vegetation), get(fire), "caribouPopGrowthModel")))
}
