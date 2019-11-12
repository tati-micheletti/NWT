defineRun <- function(replicateNumber = NULL, vegetation = "LandR.CS", fire = "fS"){ # vegetation = "LandR"; fire = "SCFM"
  
 LandR <-c("Boreal_LBMRDataPrep",
           "Biomass_regeneration",
           "LBMR")
 
 LandR.CS <- c("Boreal_LBMRDataPrep",
               "Biomass_regeneration",
               "LBMR",
               # CS
               "PSP_Clean",
               "gmcsDataPrep")
 
 fS <- c("fireSense_NWT_DataPrep",
                "fireSense_IgnitionPredict",
                "fireSense_EscapePredict",
                "LBMR2LCC_DataPrep",
                "fireSense_NWT",
                #SCFM
                "scfmLandcoverInit",
                "scfmRegime",
                "scfmDriver",
                "scfmSpread")
 
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