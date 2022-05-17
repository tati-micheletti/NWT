definePPtargetsAndLayersIII <- function(experimentName = NULL,
                                        planningUnit, 
                                        withoutPenalties = TRUE,
                                        noBirds = TRUE){
  
  # targets = c(physiographic, landscapeUnits(soil), VegetationTypes, Caribou, Stream3, Stream4, Stream5)

  if (is.null(experimentName)){
    stop("Please provide experimentName")
  }
  
  totalArea <- sum(planningUnit[[1]][], na.rm = TRUE) # in Ha!
  
  # Scenarios
  # I: Target Area = 22.97% (current size of protected areas), Penalty = 0.01
  # II: Target Area = 25% (current size of protected areas), Penalty = 0.01
  # III: Target Area = 35% (current size of protected areas), Penalty = 0.01
  # IV: Target Area = 45% (current size of protected areas), Penalty = 0.01
  # V: Target Area = 55% (current size of protected areas), Penalty = 0.01
  # VI: Target Area = 65% (current size of protected areas), Penalty = 0.01
  # VII: Target Area = 75% (current size of protected areas), Penalty = 0.01
  
  # VIII: Target Area = 22.97% (current size of protected areas), Penalty = 0.03
  # IX: Target Area = 25% (current size of protected areas), Penalty = 0.03
  # X: Target Area = 35% (current size of protected areas), Penalty = 0.03
  # XI: Target Area = 45% (current size of protected areas), Penalty = 0.03
  # XII: Target Area = 55% (current size of protected areas), Penalty = 0.03
  # XIII: Target Area = 65% (current size of protected areas), Penalty = 0.03
  # XIV: Target Area = 75% (current size of protected areas), Penalty = 0.03

  # XV: Target Area = 22.97% (current size of protected areas), Penalty = 0.05
  # XVI: Target Area = 25% (current size of protected areas), Penalty = 0.05
  # XVII: Target Area = 35% (current size of protected areas), Penalty = 0.05
  # XVIII: Target Area = 45% (current size of protected areas), Penalty = 0.05
  # XIX: Target Area = 55% (current size of protected areas), Penalty = 0.05
  # XX: Target Area = 65% (current size of protected areas), Penalty = 0.05
  # XXI: Target Area = 75% (current size of protected areas), Penalty = 0.05

  # TARGETS MS #1
  # XXII: Target Area = 5%, Penalty = NULL
  # XXIII: Target Area = 15%, Penalty = NULL
  # XXIV: Target Area = 25%, Penalty = NULL
  # XXV: Target Area = 35%, Penalty = NULL
  # XXVI: Target Area = 45%, Penalty = NULL
  # XXVII: Target Area = 55%, Penalty = NULL
  # XXVIII: Target Area = 65%, Penalty = NULL
  # XXIX: Target Area = 75%, Penalty = NULL
  # XXX: Target Area = 85%, Penalty = NULL 
  # XXXI: Target Area = 95%, Penalty = NULL 
  
  # REMOVE
  dontRemove <- NA
  removeCoarse <- NA
  removeCoarseBirds <- as.character(as.roman(1:31))
  removeCoarseBoo <- as.character(as.roman(32:41))
  
  removeLayers <- if (experimentName %in% dontRemove) NA else
    if (experimentName %in% removeCoarseBirds)
      c("stream2", "stream3", "stream4","stream5", "physiographicUnits", "vegetationTypes", "landscapeUnits") else 
        if (experimentName %in% removeCoarse)
          c("physiographicUnits", "vegetationTypes", "landscapeUnits") else
            if (experimentName %in% removeCoarseBoo)
              c("physiographicUnits", "vegetationTypes", "landscapeUnits", "stream1", "stream3", "stream4", "stream5") else
                stop("Scenario ", experimentName, " doesn't exist")
  
  # TARGETS, PENALTY and AREA TO CONSERVE
  targets <- NULL
  if (withoutPenalties){ 
    penalty <- NULL
  } else {
    penalty <- if (experimentName %in% c(as.character(as.roman(1:7)))) 0.01 else
      if (experimentName %in% c(as.character(as.roman(8:14)))) 0.03 else 
        if (experimentName %in% c(as.character(as.roman(15:21)))) 0.05 else 0
      }
    
  areaToConserve <- switch(EXPR = experimentName, 
                    # I = 0.2297*totalArea,
                    # II = 0.25*totalArea,
                    # III = 0.35*totalArea,
                    # IV = 0.45*totalArea,
                    # V = 0.55*totalArea,
                    # VI = 0.65*totalArea,
                    # VII = 0.75*totalArea,
                    # VIII = 0.2297*totalArea,
                    # IX = 0.25*totalArea,
                    # X = 0.35*totalArea,
                    # XI = 0.45*totalArea,
                    # XII = 0.55*totalArea,
                    # XIII = 0.65*totalArea,
                    # XIV = 0.75*totalArea,
                    # XV = 0.2297*totalArea,
                    # XVI = 0.25*totalArea,
                    # XVII = 0.35*totalArea,
                    # XVIII = 0.45*totalArea,
                    # XIX = 0.55*totalArea,
                    # XX = 0.65*totalArea,
                    # XXI = 0.75*totalArea,
                    # MS #1
                    XXII = 0.05*totalArea,
                    XXIII = 0.15*totalArea,
                    XXIV = 0.25*totalArea,
                    XXV = 0.35*totalArea,
                    XXVI = 0.45*totalArea,
                    XXVII = 0.55*totalArea,
                    XXVIII = 0.65*totalArea,
                    XXIX = 0.75*totalArea,
                    XXX = 0.85*totalArea,
                    XXXI = 0.95*totalArea,
                    XXXII = 0.05*totalArea,
                    XXXIII = 0.15*totalArea,
                    XXXIV = 0.25*totalArea,
                    XXXV = 0.35*totalArea,
                    XXXVI = 0.45*totalArea,
                    XXXVII = 0.55*totalArea,
                    XXXVIII = 0.65*totalArea,
                    XXXIX = 0.75*totalArea,
                    XL = 0.85*totalArea,
                    XLI = 0.95*totalArea
  )
  
  return(list(targets = targets,
              removeLayers = removeLayers,
              penalty = penalty,
              areaToConserve = areaToConserve,
              noBirds = noBirds))
  
}
