definePPtargetsAndLayersII <- function(experimentName = NULL, 
                                       speciesClass,
                                       targetSAR = 0.65, 
                                       targetNonSAR = 0.4){
  if (is.null(experimentName)){
    stop("Please provide experimentName")
  }
  # targets = c(physiographic, landscapeUnits(soil), VegetationTypes, Caribou, SAR, non-SAR)
  # targets = c(NA, NA, NA, NA, 0.65, 0.4) # I
  # targets = c(NA, NA, NA, 0.2, 0.65, 0.4) # II
  # targets = c(NA, NA, NA, 0.4, 0.65, 0.4) # III
  # targets = c(NA, NA, NA, 0.6, 0.65, 0.4) # IV 
  # targets = c(NA, NA, NA, 0.8, 0.65, 0.4) # V
  # targets = c(NA, NA, NA, 1, 0.65, 0.4) # VI
  
  # REMOVE
  dontRemove <- NULL
  removeCoarseBoo <- "I"
  removeCoarse <- c("II", "III", "IV", "V", "VI")
  
  removeLayers <- if (experimentName %in% dontRemove) NA else
                    if (experimentName %in% removeCoarseBoo)
                      c("caribou", "physiographicUnits", "vegetationTypes", "landscapeUnits") else 
                        if (experimentName %in% removeCoarse)
                          c("physiographicUnits", "vegetationTypes", "landscapeUnits") else
                            stop("Scenario ", experimentName, " doesn't exist")
      
  # TARGETS
  targets <- switch(EXPR = experimentName, 
                    I = c(rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                          rep(targetNonSAR, times = length(speciesClass[["nonSAR"]]))),
                    II = c(0.2, rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                           rep(targetNonSAR, times = length(speciesClass[["nonSAR"]]))),
                    III = c(0.4, rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                            rep(targetNonSAR, times = length(speciesClass[["nonSAR"]]))),
                    IV = c(0.6, rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                           rep(targetNonSAR, times = length(speciesClass[["nonSAR"]]))),
                    V = c(0.8, rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                          rep(targetNonSAR, times = length(speciesClass[["nonSAR"]]))),
                    VI = c(1, rep(targetSAR, times = length(speciesClass[["SAR"]])), 
                           rep(targetNonSAR, times = length(speciesClass[["nonSAR"]])))
                    )
  
  return(list(targets = targets,
              removeLayers = removeLayers))
  
}
