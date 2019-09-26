defineRun <- function(replicate = NULL, vegetation = "LandR.CS", fire = "fS"){ # vegetation = "LandR"; fire = "SCFM"
  
 LandR <-c("Boreal_LBMRDataPrep",
           "Biomass_regeneration",
           "LBMR")
 
 LandR.CS <- c("Boreal_LBMRDataPrep",
               "Biomass_regeneration",
               "LBMR",
               # CS
               "PSP_Clean",
               "gmcsDataPrep")
 
 fS <- c("climate_NWT_DataPrep",
                "MDC_NWT_DataPrep",
                "fireSense_NWT_DataPrep",
                "fireSense_FrequencyPredict",
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
 
  return(list(whichRUN = paste(vegetation, fire, replicate, sep = "_"), 
              growthAndMortalityDrivers = vegetation,  
              modules = c(get(vegetation), get(fire), "caribouPopGrowthModel")))
}