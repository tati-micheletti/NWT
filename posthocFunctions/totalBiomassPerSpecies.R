totalBiomassPerSpecies <- function(years = NULL, 
                                   folderData, 
                                   typeSim, 
                                   proportional = FALSE,
                                   columnsType = FALSE){
  
  library("data.table")
  library("usefun")
  library("LandR")
  library("reproducible")
  library("raster")
  folder <- folderData #"04JUL19" #18JUN19_CS_SCFM" #"08JUN19" #"29JUN19" 12JUL19 --> NoCS  12JUL19 --> CS
  simul <- typeSim
  folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
  
  cohortDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
  pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")
  
  sppEquivCol <- "NWT"
  data("sppEquivalencies_CA", package = "LandR")
  sppEquivalencies_CA[, NWT := c(Abie_Bal = "Abie_Bal", 
                                 Betu_Pap = "Betu_Pap", 
                                 Lari_Lar = "Lari_Lar", 
                                 Pice_Gla = "Pice_Gla",
                                 Pice_Mar = "Pice_Mar", 
                                 Pinu_Ban = "Pinu_Ban",
                                 Popu_Tre = "Popu_Tre")[Boreal]]
  
  sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(NWT)]
  sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA$NWT
  sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, sppEquivCol = sppEquivCol,
                                   palette = "Set1")
  mixed <- structure("#D0FB84", names = "Mixed")
  sppColorVect[length(sppColorVect)+1] <- mixed
  attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"
  biomassBySpecies <- rbindlist(lapply(X = names(cohortDataList), FUN = function(yr){
    cohort <- cohortDataList[[yr]]
    pixelGroup <- pixelGroupList[[yr]]
    if (NROW(cohort[duplicated(cohort)]) != 0)
      cohort <- cohort[!duplicated(cohort)]
    pixelCohortData <- LandR::addNoPixel2CohortData(cohort, pixelGroup)
    pixelCohortData[, B := as.double(B)]
    thisPeriod <- pixelCohortData[, list(year = as.numeric(substrBoth(string = yr, 
                                                                      howManyCharacters = 4, 
                                                                      fromEnd = TRUE)),
                                         BiomassBySpecies = sum(B*noPixels, na.rm = TRUE)),
                                  by = .(speciesCode)]
    
    # For proportional
if (proportional){
  # stop("This still need to be debug. Not working") # [ FIX ]
  thisPeriod$propBiomassBySpecies <- 100*(thisPeriod$BiomassBySpecies/sum(thisPeriod$BiomassBySpecies))
}
    return(thisPeriod)
  })
)
  prop <- NULL
  if (isTRUE(proportional)){
    prop <- "prop"
    y <- biomassBySpecies$propBiomassBySpecies
  } else {
    y <- biomassBySpecies$BiomassBySpecies
  }

  png(filename = file.path(folderPath, paste0("biomassMapStack_", simul, prop, ".png")), height = 600, width = 900)
  library("ggplot2")
  if (columnsType){
    plot2 <- ggplot(data = biomassBySpecies, aes(x = year, y = y, 
                                                 fill = speciesCode), position = "fill") +
      geom_col(aes(y = y)) +
      scale_fill_viridis_d() +
      labs(x = "Year", y = "Total Biomass", title = paste0("Total biomass by species\n",
                                                     "across pixels")) +
      theme_bw() +
      theme(legend.text = element_text(size = 20), legend.title = element_blank(),
            text = element_text(size=20),
            axis.text.x = element_text(size = 20),
            title = element_text(size = 22))
    quickPlot::clearPlot()
    print(plot2)
    dev.off()
    
  } else {
    plot2 <- ggplot(data = biomassBySpecies, aes(x = year, y = y,
                                                 fill = speciesCode, group = speciesCode)) +
      geom_area(position = "stack") +
      scale_fill_manual(values = sppColorVect) +
      labs(x = "Year", y = "Total Biomass", title = paste0("Total biomass by species\n",
                                              "across pixels")) +
      theme(legend.text = element_text(size = 16), legend.title = element_blank(),
            text = element_text(size=16),
            axis.text.x = element_text(size = 16))
    quickPlot::clearPlot()
    quickPlot::Plot(plot2, new = TRUE)
    dev.off()
  }
  
return(plot2)
}