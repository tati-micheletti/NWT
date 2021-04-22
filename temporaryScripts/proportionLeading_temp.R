if (useProportionLeading){
  data("sppEquivalencies_CA", package = "LandR")
  runName <- "NWT"
  # Make NWT spp equivalencies
  sppEquivalencies_CA[, paste0(runName) := c(Betu_Pap = "Betu_Pap", 
                                             Lari_Lar = "Lari_Lar", 
                                             Pice_Gla = "Pice_Gla",
                                             Pice_Mar = "Pice_Mar", 
                                             Pinu_Ban = "Pinu_Ban", 
                                             Popu_Tre = "Popu_Tre")[Boreal]]
  
  sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(get(runName))]
  sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA[[paste0(runName)]]
  sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, 
                                   sppEquivCol = "NWT",
                                   palette = "Set3")
  mixed <- structure("#D0FB84", names = "Mixed")
  sppColorVect[length(sppColorVect)+1] <- mixed
  attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"
  
  spLeadingMap <- LandR::vegTypeMapGenerator(x = coh, pixelGroupMap = ras,
                                             vegLeadingProportion = 0.5,
                                             mixedType = 2, 
                                             sppEquiv = sppEquivalencies_CA, 
                                             sppEquivCol = runName, 
                                             colors = sppColorVect,
                                             pixelGroupColName = "pixelGroup",
                                             doAssertion = getOption("LandR.assertions", TRUE))
} else {
  
}