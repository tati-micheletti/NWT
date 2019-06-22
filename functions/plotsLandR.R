# # LBMRplots
# sim <- NWT_CS_SCFM
# 
# folder <- "18JUN19_CS_SCFM" #"08JUN19"
# simul <- "CS_SCFM"
# folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder,"/")
# 
# source("/mnt/data/Micheletti/NWT/posthocFunctions/bringObjectTS.R")
# 
# cohorDataList <- bringObjectTS(path = folderPath, rastersNamePattern = "cohortData")
# pixelGroupList <- bringObjectTS(path = folderPath, rastersNamePattern = "pixelGroupMap")
# 
# pl <- plotsLandR(pixelGroupMapList = pixelGroupList, cohortDataList = cohorDataList)
# 
# plotsLandR <- function(pixelGroupMapList, cohortDataList, 
#                        vegTypeMap = NULL, 
#                        rasterToMatchReporting = NULL){
#   browser()
#   if (length(pixelGroupMapList) != length(cohortDataList)) # data sanity check
#     stop("The length of pixelGroupMap maps is not the same as cohortData tables. Do you have all data?")
#   lapply(X = length(pixelGroupMapList), function(index){ # lapply through the years to get the plots running
#     cohortData <- cohortDataList[[index]]
#     pixelGroupMap <- pixelGroupMapList[[index]]
#     
#     if (!is.null(rasterToMatchReporting)){
#       maxNpixels <- sum(!is.na(rasterToMatchReporting[]))
#     } else {
#       maxNpixels <- sum(!is.na(pixelGroupMap[]))
#     }
#     
#     ## MEAN NO. PIXELS PER LEADING SPECIES
#     if (!is.null(vegTypeMap) && !is.null(studyAreaReporting)){
#       vtm <- raster::mask(vegTypeMap, studyAreaReporting)
#       
#     }
#     # timeSim <- 
#     freqs <- table(na.omit(factorValues2(vtm, vtm[], att = 2)))
#     tabl <- as.vector(freqs)
#     summaryBySpecies1 <- data.frame(year = rep(floor(timeSim), length(freqs)),
#                                     leadingType = names(freqs),
#                                     #freqs = freqs,
#                                     counts = tabl,
#                                     stringsAsFactors = FALSE)
#     
#     whMixedLeading <- which(summaryBySpecies1$leadingType == "Mixed")
#     summaryBySpecies1$leadingType <- equivalentName(summaryBySpecies1$leadingType,
#                                                     sim$sppEquiv,
#                                                     "EN_generic_short")
#     summaryBySpecies1$leadingType[whMixedLeading] <- "Mixed"
#     
#     colours <- equivalentName(names(sim$sppColorVect), sim$sppEquiv, "EN_generic_short")
#     whMixedSppColors <- which(names(sim$sppColorVect) == "Mixed")
#     colours[whMixedSppColors] <- "Mixed"
#     
#     colorIDs <- match(summaryBySpecies1$leadingType, colours)
#     summaryBySpecies1$cols <- sim$sppColorVect[colorIDs]
#     
#     if (is.null(sim$summaryBySpecies1)) {
#       sim$summaryBySpecies1 <- summaryBySpecies1
#     } else {
#       sim$summaryBySpecies1 <- rbindlist(list(sim$summaryBySpecies1, summaryBySpecies1))
#     }
#     
#     cols3 <- sim$summaryBySpecies1$cols
#     names(cols3) <- sim$summaryBySpecies1$leadingType
#     
#     plot3 <- ggplot(data = sim$summaryBySpecies1, aes(x = year, y = counts, fill = leadingType)) +
#       scale_fill_manual(values = cols3) +
#       labs(x = "Year", y = "Count") +
#       geom_area() +
#       theme(legend.text = element_text(size = 6), legend.title = element_blank()) +
#       geom_hline(yintercept = maxNpixels, linetype = "dashed", color = "darkgrey", size = 1)
#     
#     Plot(plot3, title = "Number of pixels, by leading type", new = TRUE)
#   })
#   
# }

