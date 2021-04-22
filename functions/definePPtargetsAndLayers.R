definePPtargetsAndLayers <- function(experimentName){
  # targets = c(physiographic, landscapeUnits(soil), VegetationTypes, Caribou, Stream3, Stream4, Stream5)
  # targets = c(0.3, 0.3, 0.3, 0.7, 0.7, 0.1, 0.1) # I
  # targets = c(0.3, 0.3, 0.3, 0.7, 0.3, 0.1, 0.1) # II
  # targets = c(0.3, 0.3, 0.3, 0.3, 0.7, 0.1, 0.1) # III
  # targets = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.1) # IV
  
  # Remove Boo
  # targets = c(0.3, 0.3, 0.3, NA, 0.7, 0.1, 0.1) # VI
  # targets = c(0.3, 0.3, 0.3, NA, 0.3, 0.1, 0.1) # VII
  
  # Remove Birds
  # targets = c(0.3, 0.3, 0.3, 0.7, NA, NA, NA) # V
  # targets = c(0.3, 0.3, 0.3, 0.3, NA, NA, NA) # VIII
  
  # Remove Boo and Birds
  # targets = c(0.3, 0.3, 0.3, NA, NA, NA, NA) # IX
  
  # REMOVE
  dontRemove <- c("I", "II", "III", "IV")
  removeBirds <- c("V", "VIII")
  removeBoo <- c("VI", "VII")
  removeBoth <- "IX"
  removeCoarseBirds <- c("X", "XIII")
  removeCoarseBoo <- c("XI", "XIV")  
  removeCoarse <- c("XII")

  removeLayers <- if (experimentName %in% dontRemove)
    NA else if (experimentName %in% removeBirds)
      c("stream3", "stream4", "stream5") else 
        if (experimentName %in% removeBoo)
        c("stream1") else 
          if (experimentName %in% removeCoarse)
          c("physiographicUnits", "vegetationTypes", "landscapeUnits") else 
            if (experimentName %in% removeCoarseBoo)
            c("stream1", "physiographicUnits", "vegetationTypes", "landscapeUnits") else 
              if (experimentName %in% removeCoarseBirds)
                c("stream3", "stream4","stream5", "physiographicUnits", "vegetationTypes", "landscapeUnits") else 
                  if (experimentName %in% removeBoth)
                    c("stream1", "stream3", "stream4", "stream5") else 
                      stop("Scenario ", experimentName, " doesn't exist")

  # TARGETS
  targets <- switch(EXPR = experimentName, 
                    I = c(0.3, 0.3, 0.3, 0.7, 0.7, 0.1, 0.1),
                    II = c(0.3, 0.3, 0.3, 0.7, 0.3, 0.1, 0.1),
                    III = c(0.3, 0.3, 0.3, 0.3, 0.7, 0.1, 0.1),
                    IV = c(0.3, 0.3, 0.3, 0.3, 0.3, 0.1, 0.1),
                    V = c(0.3, 0.3, 0.3, 0.7),
                    VI = c(0.3, 0.3, 0.3, 0.7, 0.1, 0.1),
                    VII = c(0.3, 0.3, 0.3, 0.3, 0.1, 0.1),
                    VIII = c(0.3, 0.3, 0.3, 0.3),
                    IX = c(0.3, 0.3, 0.3),
                    X = c(0.3),
                    XI = c(0.3, 0.1, 0.1),
                    XII = c(0.3, 0.3, 0.1, 0.1),
                    XIII = c(0.7),
                    XIV = c(0.7, 0.1, 0.1)
                    )
  
  return(list(targets = targets,
              removeLayers = removeLayers))
  
}
