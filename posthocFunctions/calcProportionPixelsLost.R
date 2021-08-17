# PLOT 4 ####
calcProportionPixelsLost <- function(listOfRasters = NULL,
                                     newSortOrder,
                                     species,
                                     outputFolder,
                                     folderToSave,
                                     noCorner,  # <~~~~~~~~ TEMPORARY!!!
                                     netChangeTable, # Needs to have the netChange column! This col is in areas 6.25*(sum(probabilities))
                                     # netChangeTable comes from plotsPaper_NotFun.R
                                     useFuture = TRUE,
                                     tableThreshold,
                                     percentToDiscard = 0.3){

  fileNamePath <- file.path(outputFolder, "proportionPixelsChangedTable.qs")
  if (!file.exists(fileNamePath)){ 
    if (is.null(listOfRasters))
      stop("If final object does not exist. listOfRasters must be supplied")
    if (useFuture) plan("multiprocess", workers = length(species))
    allBirds <- rbindlist(future_lapply(names(listOfRasters), function(sp){ ########### future_lapply <~~~~~~~~~~~~~~~~~~~~
      allScenarios <- raster::stack(lapply(names(listOfRasters[[sp]]), function(scenario){
        tic(paste0("Calculating colonization/extirpation for ", paste(sp, scenario, sep = " ")))
        allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
          allRuns <- raster::stack(lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
            colRasPath <- file.path(outputFolder, paste("occupancy", "firstYear", sp, scenario, bmod, runs, sep = "_"))
            if (!file.exists(paste0(colRasPath, ".tif"))){
            # NEED TO CALCULATE THE PROBABILITY OF PRESENCE (i.e. predicted raster and applying the threshold)  
              ras2011 <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]][[1]]
              thresholdVal <- tableThreshold[spec == sp, meanDensity]
              ras2011[ras2011[] < thresholdVal] <- 0
              ras2011[ras2011[] >= thresholdVal] <- 1
              writeRaster(ras2011, filename = paste0(colRasPath, ".tif"), 
                          format = "GTiff")
              colRas <- raster::raster(paste0(colRasPath, ".tif"))
            } else {
              colRas <- raster::raster(paste0(colRasPath, ".tif"))
            }
            return(colRas)
          }))
          names(allRuns) <- names(listOfRasters[[sp]][[scenario]][[bmod]])
          return(allRuns)
        })
        names(allMods) <- names(listOfRasters[[sp]][[scenario]])
        toc()
        return(raster::stack(allMods))
      }))
      fileNamePath <- file.path(outputFolder, paste0("probabilityPresence_2011_", sp))
      tic(paste0("Calculating probability of presence in 2011 for ", sp))
      if (!file.exists(paste0(fileNamePath, ".tif"))){
        probPresence <- calc(x = allScenarios, fun = mean, na.rm = TRUE,
                             filename = fileNamePath,
                             overwrite = TRUE,
                             format = "GTiff")
        # TEMPORARY ####################
        # ### TODO >>>> HERE CUT THE CORNER
        probPresence[is.na(noCorner)] <- NA # <~~~~~REMOVE
        # SAVE AGAIN!
        writeRaster(probPresence,
                    filename = fileNamePath,
                    overwrite = TRUE, format = "GTiff")
        message(crayon::green(paste0("Corner removed for ", sp)))
        # TEMPORARY ####################
        rm(allScenarios)
        gc()
      } else {
        probPresence <- raster(paste0(fileNamePath, ".tif"))
        # TEMPORARY ####################
        # ### TODO >>>> HERE CUT THE CORNER
        probPresence[is.na(noCorner)] <- NA # <~~~~~REMOVE
        # SAVE AGAIN!
        writeRaster(probPresence,
                    filename = fileNamePath,
                    overwrite = TRUE, format = "GTiff")
        message(crayon::green(paste0("Corner removed for ", sp)))
        # TEMPORARY ####################
      }
      expectedArea2011m2 <- sum(probPresence[], na.rm = TRUE)
      expectedArea2011ha <- 6.25*expectedArea2011m2
      expectedAreaChange2100ha <- sum(netChangeTable[species == sp & effect != "netEffect", netChange])
      proportionOfAreaChanged <- expectedAreaChange2100ha/expectedArea2011ha
      # These 2100 values are already in ha [double checked] and are already expected area change
      # meaning: (Colonization - Extirpation) in 2100
      DT <- data.table(species = sp,
                       expectedAreaHa2011 = expectedArea2011ha,
                       expectedAreaChange2100ha = expectedAreaChange2100ha,
                       proportionOfAreaChanged = proportionOfAreaChanged
                         )
      toc()
      return(DT)
    }))
    plan("sequential")
    qs::qsave(allBirds, fileNamePath)
  } else {
    allBirds <- qs::qread(fileNamePath)
  }
  library("ggplot2")
  library("data.table")
  library("raster")
  library("tictoc")
  
  # Need to fix slight differences in total proportion due to large variation among 
  # 2011 initital scenarios and huge increases due to VERY low presence
  allBirds[proportionOfAreaChanged < -1, proportionOfAreaChanged := -1]
  allBirds[proportionOfAreaChanged > 5, proportionOfAreaChanged := 5]
  
  allBirds[, colonization := ifelse(proportionOfAreaChanged > 0, "increase", "decrease")]

  lapply(X = species, function(sp){
    DT <- allBirds[species == sp,]
    signal <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, "> 0", "< 0")
    jit <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, 0.4, -0.4)
    S <-  sum(DT[eval(parse(text = paste0("proportionOfAreaChanged", 
                                          signal))), 
                 proportionOfAreaChanged])
    pos <- S + jit
    allBirds[species == sp, labelMark := pos]
    return("OK")
  })
  # Cleanup based on species
  allBirds <- na.omit(allBirds)
  
  # Now the plot
  allBirds[, species := factor(species, levels = newSortOrder)]
  
  p4 <- ggplot(data = allBirds, mapping = aes(x = round(proportionOfAreaChanged*100, 0), y = species, 
                                              fill = colonization, group = colonization,
                                              color = colonization)) +
    geom_col() +
    geom_vline(xintercept = 0, color = "black") + 
    xlab("(C) Expected percentage of habitat area colonized\nor extirpated due to climate change") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank(),
          text = element_text(size = 16, 
                              family = "Consolas"),
          axis.title.x = element_text(family = "Arial"),
          axis.text.x = element_text(family = "Arial"),
          axis.text.y = element_text(family = "Consolas")) + #,
          # plot.margin = unit(c(5.5,5.5,5.5,0), "pt")) +
    scale_color_manual(values = c("increase" = "slateblue3", 
                                  "decrease" = "goldenrod3")) + 
    scale_fill_manual(values = c("increase" = "slateblue1", 
                                 "decrease" = "goldenrod1")) +
    geom_text(aes(x = labelMark*100, 
                  label = round(100*proportionOfAreaChanged, 0)), 
              family = "Arial",
              size = 4, color = "grey10", check_overlap = TRUE) +
    scale_x_continuous(breaks = seq(-100, 500, by = 100)) +
    scale_y_discrete(position = "right")
  p4
  
  ggsave(device = "png", filename = file.path(folderToSave, 
                                              "proportionalChangeInArea.png"), 
         width = 8, height = 11)
  
  return(p4)
  }
