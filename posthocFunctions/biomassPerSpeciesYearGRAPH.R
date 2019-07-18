# POTENTIALLY WRONG?!

# biomassPerSpeciesYearGRAPH <- function(pathData, times, version, 
#                                        overwriteRasters = TRUE, 
#                                        uploadFiles = FALSE,
#                                        whereToReport){
#   
#   # Bring the tables, bring the pixel groups, maske the rasters, mask them to the RTM masked to the forest pixels
#   reproducible::Require("raster")
#   reproducible::Require("ggplot2")
#   reproducible::Require("magrittr")
#   reproducible::Require("data.table")
#   library("SpaDES")
# 
#   y <- seq(times$start, times$end, by = 10)
#   years <- paddedFloatToChar(y, padL = 3)
#   listCohort <- lapply(X = years, FUN = function(yr){
#     if (!existsFunction("createModObject")){
#       cM <- grepMulti(patterns = "createModObject", x = list.files(getwd()))
#       if (length(cM)!=0)
#         source(cM) else
#           stop("Couldn't find `createModObject` function. Please debug the code.")
#     }
#     cohortData <- createModObject(data = "cohortData", sim = NULL,
#                                   pathInput = pathData, currentTime = as.numeric(yr))
#     if (is.null(cohortData))
#         stop(paste0("`creatModObject` function did not find the needed file.",
#                        "\n Did you pass the correct time used in the simulation?"))
#     return(cohortData)
#   })
#   
#   listPixelGroupMap <- lapply(X = years, FUN = function(yr){
#     pixelGroupMap <- createModObject(data = "pixelGroupMap", sim = NULL,
#                                      pathInput = pathData, currentTime = as.numeric(yr))
#     return(pixelGroupMap)
#   })
#   
#   if (whereToReport == "Edehzhie"){
#     urlSA <- "https://drive.google.com/open?id=1VP91AyIeGCFwJS9oPSEno4_SbtJQJMh7"
#   } else {
#     if (whereToReport == "BCR6_NWT"){
#       urlSA <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
#     } else stop("Please provide as 'whereToReport' either 'Edehzhie' or 'BCR6_NWT'")
#   }
# 
#   message(paste0("Reporting biomass per species for ", whereToReport))
#     studyArea <- Cache(prepInputs,
#                        url = urlSA,
#                        destinationPath = tempdir(),
#                        omitArgs = "destinationPath", filename2 = NULL)
# 
#     rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
#                            studyArea = studyArea,
#                            targetFile = "RTM.tif", destinationPath = tempdir(),
#                            filename2 = NULL,
#                            omitArgs = "destinationPath")
# 
#     LCC05 <- LandR::prepInputsLCC(destinationPath = tempdir(),
#                                   studyArea = studyArea,
#                                   rasterToMatch = rasterToMatch)
#     forestClasses <- c(1:15, 34:35)
#     rasterToMatch[!LCC05 %in% forestClasses] <- NA
#     speciesYearList <- rbindlist(lapply(X = seq_len(length(listCohort)), FUN = function(y){
#     
#     listPixelGroupMap[[y]] <- postProcess(x = listPixelGroupMap[[y]],
#                                           rasterToMatch = rasterToMatch,
#                                           maskWithRTM = TRUE,
#                                           destinationPath = tempdir(), 
#                                           filename2 = NULL)
#     valsPixelGroup <- data.table::data.table(pixelGroup = unique(raster::getValues(listPixelGroupMap[[y]])))
#     
#     setkey(valsPixelGroup, pixelGroup)
#     setkey(listCohort[[y]], pixelGroup)
#     excCohorts <- valsPixelGroup[listCohort[[y]]]
#     sumSpB <- excCohorts[, .(totalBiomass = sum(B)), by = c("pixelGroup", "speciesCode")] 
# 
#     spBioList <- rbindlist(lapply(X = as.character(unique(sumSpB$speciesCode)), FUN = function(sp){
#       spT <- sumSpB[speciesCode == sp]
#       pxG <- data.table::data.table(pixelID = seq_len(NROW(raster::getValues(listPixelGroupMap[[y]]))), 
#                                     pixelGroup = raster::getValues(listPixelGroupMap[[y]]))
#       
#       setkey(spT, pixelGroup)
#       setkey(pxG, pixelGroup)
#       BTable <- spT[pxG]
#       BTable <- BTable[order(pixelID)]
#       BTable$totalBiomass[is.na(BTable$totalBiomass)] <- 0
#       BTable$totalBiomass[is.na(BTable$pixelGroup)] <- NA
#       
#       spRas <- raster::setValues(x = raster(listPixelGroupMap[[y]]), values = BTable$totalBiomass)
#       biomassPath <- checkPath(file.path(dirname(pathData), "biomassRasters"), create = TRUE)
#       if ((file.exists(file.path(biomassPath, paste0(sp, "Biomass", "Year", years[y]))) & isTRUE(overwriteRasters))|
#           !file.exists(file.path(biomassPath, paste0(sp, "Biomass", "Year", years[y]))))
#         writeRaster(x = spRas, filename = file.path(biomassPath, paste0(sp, "Biomass", "Year", years[y])),
#                     overwrite = TRUE, format = "GTiff")
#       nonNaBio <- BTable$totalBiomass[!is.na(BTable$totalBiomass)]
#       if (!3295673) {
#         message(paste0("NonNABio is different for ", sp, ". Check the code!"))
#         browser()
#         }
#       sumBio <- sum(nonNaBio)*6.25/length(nonNaBio)*10000 # Original biomass unit is g/m2, it was converted here to tons/ha and multiplied by the ha/pixel (6.25)
#       avrBiomass <- sumBio/(length(nonNaBio)*6.25)
#       bioTable <- data.table(Species = sp, Year = y * 10, averageBiomassPerHa = avrBiomass, totalBiomass = sumBio)
#       return(bioTable)
#       })
#     )
#     return(spBioList)
#     })
#     )
#     dt <- as.data.frame(speciesYearList)
#     library("ggplot2")
#     averageBiomassHa <-
#       ggplot(dt, aes(x = Year, y = averageBiomassPerHa, group = Species)) +
#       geom_line(size = 1.2, aes(color = Species)) +
#       ggtitle(label = "Average biomass per hectare per species per year")
#     
#     totalBiomassPerHa <-
#       ggplot(dt, aes(x = Year, y = totalBiomass, group = Species)) +
#       geom_line(size = 1.2, aes(color = Species)) +
#       ggtitle(label = "Total biomass per species per year")
#     
#    lapply(X = c("totalBiomassPerHa", "averageBiomassHa"),
#                    function(plotting){
#                      gifName <- file.path(pathData, paste0(plotting, version, "_", 
#                                                            toupper(format(Sys.time(), "%d%b%y")),".png"))
#                      png(gifName,
#                          width = 700, height = 480)
#                      print(get(plotting))
#                      dev.off()
#                      if (uploadFiles){
#                        googledrive::drive_upload(gifName,
#                                                  path = googledrive::as_id("1yM3v8BdpXbNPfnNZqr4q34D52dH1nEcb")) 
#                      }
#                    })
#     return(list(averageBiomassHa = averageBiomassHa, totalBiomassPerHa = totalBiomassPerHa))
# }
