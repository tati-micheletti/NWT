utils::globalVariables(c(".", "..cols", ":=",
                         "age", "B", "bWeightedAge", "noPixels",
                         "speciesCode", "year"))

#' Plots biomass per species: proportional or absolute, and total or just overstory
#'
#' @param dataPath character. Path to data
#' @param typeSim character. Which typeSimulation is it? i.e. 'LandR_SCFM' | 'LandR.CS_fS'
#' @param overstory logical. Should the plot be of only the overstory biomass?
#' @param overwrite logical.
#' @param maxVal numeric. Max value for y axis. Passing this ensures that both overstory
#'               and all biomass plots are comparable Default to 1e10.
#' @param sppColorVect named character vector with the species colors in hex format
#'
#' @return plot
#'
#' @author Tati Micheletti
#' @export
#' @importFrom data.table := data.table getDTthreads rbindlist setDTthreads
#' @importFrom googledrive drive_upload
#' @importFrom grDevices dev.off png
#' @importFrom LandR sppColors vegTypeMapGenerator
#' @importFrom quickPlot clearPlot Plot
#' @importFrom raster writeRaster
#' @importFrom SpaDES.core paddedFloatToChar
#' @importFrom SpaDES.tools rasterizeReduced
#' @importFrom utils data
#' @include bringObjectTS.R
#' @rdname totalBiomassPerSpeciesReps
totalBiomassPerSpeciesReps <- function(dataPath,
                                       typeSim,
                                       proportional = FALSE,
                                       columnsType = FALSE,
                                       overstory = FALSE,
                                       overwrite = FALSE,
                                       maxVal = NULL,
                                       sppColorVect,
                                       stacked = TRUE) {
# ONLY WORKS IN RSTUDIO!
  prop <- NULL
  overS <- NULL
  if (isTRUE(proportional)) prop <- "_Prop"
  if (isTRUE(overstory)) overS <- "_Overstory"
  if (!isTRUE(overwrite)){
    pat <- c("biomassMapStackReps_", typeSim, prop, overS)
    fileName <- grepMulti(x = list.files(dataPath, full.names = TRUE),
                          patterns = pat)
    if (length(fileName) != 0){
      message("Plots exist and overwrite is FALSE. Returning paths")
      return(fileName)
    }
  }
  
  if (proportional)
    maxVal <- 100
  
  cohortListFilename <- file.path(dataPath, typeSim, paste0("cohortDataList", typeSim, ".qs"))
  whichSimulPath <- file.path(dataPath, typeSim)
  runs <- list.dirs(whichSimulPath, recursive = FALSE, full.names = FALSE)
  if (!file.exists(cohortListFilename)){
    cohortDataList <- rbindlist(lapply(X = runs, function(RUN){
      runPath <- file.path(whichSimulPath, RUN)
      cohortData <- bringObjectTS(path = runPath, rastersNamePattern = "cohortData")
      cohortDataList <- rbindlist(lapply(names(cohortData), function(YEAR){
        cohortDataYear <- cohortData[[YEAR]]
        cohortDataYear[, year := usefulFuns::substrBoth(YEAR, 4, TRUE)]
        return(cohortDataYear)
      }), use.names = TRUE)
      cohortDataList[, run := RUN]
      return(cohortDataList)
    }), use.names = TRUE)
    qs::qsave(x = cohortDataList, file = cohortListFilename)
  }  else {
    cohortDataList <- qs::qread(cohortListFilename)
  }
  
  pixelGroupListFilename <- file.path(dataPath, typeSim, paste0("pixelGroupList", typeSim, ".qs"))
  if (!file.exists(pixelGroupListFilename)){
  pixelGroupList <- lapply(X = runs, function(RUN){
    runPath <- file.path(whichSimulPath, RUN)
    pixelGroup <- bringObjectTS(path = runPath, rastersNamePattern = "pixelGroupMap")
    return(pixelGroup)
  })
  names(pixelGroupList) <- runs
  rearrangedNames <- names(pixelGroupList[[1]])
  totalYears <- length(pixelGroupList[[1]])
  lengthVector <- 1:totalYears
  pixelGroupList <- lapply(X = lengthVector, FUN = function(YEAR){
    sbset <- unlist(lapply(pixelGroupList, `[[`, YEAR), use.names = TRUE)
    return(sbset)
  })
  names(pixelGroupList) <- rearrangedNames
  qs::qsave(x = pixelGroupList, file = pixelGroupListFilename)
  } else {
    pixelGroupList <- qs::qread(pixelGroupListFilename)
  }
  
  biomassBySpecies <- rbindlist(lapply(X = names(pixelGroupList), FUN = function(yr) {
    cohort <- cohortDataList[year == usefulFuns::substrBoth(yr, 4, TRUE), ]
    pixelGroup <- pixelGroupList[[yr]]
        if (NROW(cohort[duplicated(cohort)]) != 0)
      cohort <- cohort[!duplicated(cohort)]
    thisPeriodAllRuns <- rbindlist(lapply(runs, function(RUN){
      pixelCohortData <- LandR::addNoPixel2CohortData(cohort[run == RUN,], pixelGroup[[RUN]])
      pixelCohortData[, B := as.double(B)]
      thisPeriod <- pixelCohortData[, list(year = as.numeric(substrBoth(strng = yr,
                                                                        howManyCharacters = 4,
                                                                        fromEnd = TRUE)),
                                           BiomassBySpecies = sum(B*noPixels, na.rm = TRUE)),
                                    by = .(speciesCode)]
      
      if (overstory) {
        pixelCohortData[, bWeightedAge := floor(sum(age*B)/sum(B)/10)*10, .(pixelGroup)]
        overstory <- pixelCohortData[age >= bWeightedAge, .(overstoryBiomass = sum(B * noPixels)), .(speciesCode)]
        thisPeriod <- thisPeriod[overstory, on = 'speciesCode']
      }
      thisPeriod[, Run := RUN]
      return(thisPeriod)
    }))
    return(thisPeriodAllRuns)
  }))
  
  # Here. Now I need to make sure I can do the averages of the runs, the deviation, 
  # calculate if it is within the Coefficient of Variation under 20% I need 
  # return the plot and as attributes, the Coeff of variation for each year?
  # cv <- sd(x, na.rm=TRUE)/mean(x, na.rm=TRUE)*100
  if (overstory){
    biomassBySpecies[, c("averageBySpecies", "deviationBySpecies") := list(mean(overstory), 
                                                                           sd(overstory)),
                     by = c("speciesCode", "year")]
  } else {
    biomassBySpecies[, c("averageBySpecies", "deviationBySpecies") := list(mean(BiomassBySpecies), 
                                                                           sd(BiomassBySpecies)),
                     by = c("speciesCode", "year")]
  }
  biomassBySpecies[, cv := deviationBySpecies/averageBySpecies]
  message(paste0("The maximum coefficient of variation for ", typeSim, " for ", 
                 ifelse(overstory, "overstory only", "total biomass (over + understory)"),
                 " was ", 
                 round(max(biomassBySpecies$cv), 3)*100, "%."))
  if (FALSE){
    plot2 <- ggplot(data = biomassBySpecies, aes(x = year, y = y,
                                                 fill = speciesCode), position = "fill") +
      geom_col(aes(y = y)) +
      scale_fill_viridis_d() +
      labs(x = "Year", y = "Total Biomass", title = paste0("Total biomass by species\n",
                                                           "across pixels - ", typeSim, " ", overS)) +
      theme_bw() +
      theme(legend.text = element_text(size = 20), legend.title = element_blank(),
            text = element_text(size=20),
            axis.text.x = element_text(size = 20),
            title = element_text(size = 22)) +
      ylim(0, maxVal)
    clearPlot()
    print(plot2)
    dev.off()
  } else {
    
    dtColor <- data.table::data.table(color = sppColorVect, speciesCode = names(sppColorVect))
    biomassBySpecies <- merge(biomassBySpecies, dtColor, by = "speciesCode")
    
    # library(plyr) # DIDN'T WORK!
    # biomassBySpeciesSimple <- biomassBySpecies[Run == "run1", c("speciesCode", "year", "averageBySpecies",
    #                                                             "deviationBySpecies")]
    # 
    # biomassBySpeciesSimple <- ddply(biomassBySpeciesSimple, .(year), transform, 
    #                            ybegin = cumsum(averageBySpecies) - deviationBySpecies,
    #                            yend = cumsum(averageBySpecies) + deviationBySpecies)
    #                            
    #                                  # scale_fill_manual(values = sppColorVect) +
    # geom_ribbon(aes(ymin = averageBySpecies-deviationBySpecies,
    #                 ymax = averageBySpecies+deviationBySpecies,
    #                 fill = speciesCode), alpha = 0.5) +
    
    biomassBySpeciesSimple <- biomassBySpecies[Run == "run1", c("speciesCode", "year", "averageBySpecies",
                                                                "deviationBySpecies", "color")]
    if (is.null(maxVal))
      maxVal <- max(biomassBySpeciesSimple$averageBySpecies)+max(biomassBySpeciesSimple$averageBySpecies)*.10
      
    plot2 <- ggplot(data = biomassBySpeciesSimple, aes(x = year, y = averageBySpecies,
                                                 fill = speciesCode, 
                                                 group = speciesCode))
    if (stacked){
      plot2 <- plot2 + geom_area(position = "stack")
    } else {
      plot2 <- plot2 + geom_line(colour = biomassBySpeciesSimple$color, size = 1.6) +
        geom_errorbar(aes(ymax = averageBySpecies + deviationBySpecies,
                          ymin = averageBySpecies - deviationBySpecies,
                          width=.5))
    }
  
    plot2 <- plot2 + scale_fill_manual(values = sppColorVect) +
      labs(x = "Year", y = "Total Biomass", 
           title = paste0("Total biomass by species\n",
                          "across pixels - ", typeSim, " ", 
                          overS)) +
      theme(legend.text = element_text(size = 16), 
            legend.title = element_blank(),
            text = element_text(size = 16),
            axis.text.x = element_text(size = 16),
            legend.position = "right") 
    tryCatch({
      dev.off()
    }, error = function(e){
      warning("dev.off(): cannot shut down device 1 (the null device)")
    })
    fl <- file.path(dataPath, paste0("biomassMapStackReps_", typeSim, prop, overS, ".png"))
    plot2
    ggsave(filename = fl,
        height = 8)
    
    dev.off()
  }
    return(fl)
}
