# PLOT 4 ####
calcProportionPixelsLost <- function(listOfRasters = NULL,
                                     newSortOrder,
                                     species,
                                     outputFolder,
                                     noCorner,  # <~~~~~~~~ TEMPORARY!!!
                                     netChangeTable, # Needs to have the netChange column! This col is in areas 6.25*(sum(probabilities))
                                     # netChangeTable comes from plotsPaper_NotFun.R
                                     useFuture = TRUE,
                                     percentToDiscard = 0.3){
  
  fileNamePath <- file.path(outputFolder, "proportionPixelsChangedTable.qs")
  if (!file.exists(fileNamePath)){
    if (is.null(listOfRasters))
      stop("If final object does not exist. listOfRasters must be supplied")
    if (useFuture) plan("multiprocess", workers = length(species))
    allBirds <- rbindlist(future_lapply(names(listOfRasters), function(sp){ ########### future_lapply <~~~~~~~~~~~~~~~~~~~~future_
      allScenarios <- raster::stack(lapply(names(listOfRasters[[sp]]), function(scenario){
        tic(paste0("Calculating colonization/extirpation for ", paste(sp, scenario, sep = " ")))
        allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
          allRuns <- raster::stack(lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
            colRasPath <- file.path(outputFolder, paste("colonization", "firstYear", sp, scenario, bmod, runs, sep = "_"))
            if (!file.exists(paste0(colRasPath, ".tif"))){
              rasStk <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]]
              colRas <- calcColonization(rasT0Path = rasStk[[1]], 
                                         rasName = colRasPath)
              gc()
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
                             format = "GTiff")
        # TEMPORARY ####################
        # ### TODO >>>> HERE CUT THE CORNER
        # probPresence[is.na(noCorner)] <- NA # <~~~~~REMOVE
        # # SAVE AGAIN!
        # writeRaster(probPresence, 
        #             filename = fileNamePath, 
        #             overwrite = TRUE, format = "GTiff")
        # message(crayon::green(paste0("Corner removed for ", sp)))
        # TEMPORARY ####################
        rm(allScenarios)
        gc()
      } else {
        probPresence <- raster(paste0(fileNamePath, ".tif"))

      }
      m2ExpectedArea <- sum(probPresence[], na.rm = TRUE)
      DT <- data.table(species = sp,
                       expectedAreaHa2011 = 6.25*m2ExpectedArea,
                       expectedAreaHa2100 = sum(netChangeTable[species == sp & effect != "netEffect", netChange]),
                       proportionOfAreaChanged = (sum(netChangeTable[species == sp & effect != "netEffect", netChange]))/
                         (6.25*m2ExpectedArea))
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
  
  allBirds[, colonization := ifelse(proportionOfAreaChanged > 0, "increase", "decrease")]

  lapply(X = species, function(sp){
    DT <- allBirds[species == sp,]
    signal <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, "> 0", "< 0")
    jit <- ifelse(unique(DT[["proportionOfAreaChanged"]]) > 0, 0.2, -0.2)
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
  p4 <- ggplot(data = allBirds, mapping = aes(x = proportionOfAreaChanged, y = species, 
                                              fill = colonization, group = colonization,
                                              color = colonization)) +
    geom_col() +
    geom_vline(xintercept = 0, color = "black") + 
    xlab("(C) Expected proportion of habitat area \ncolonized or lost due to climate change") +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          # axis.text.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.y = element_blank()) + #,
          # plot.margin = unit(c(5.5,5.5,5.5,0), "pt")) +
    scale_color_manual(values = c("increase" = "slateblue3", 
                                  "decrease" = "goldenrod3")) + 
    scale_fill_manual(values = c("increase" = "slateblue1", 
                                 "decrease" = "goldenrod1")) +
    geom_text(aes(x = labelMark, 
                  label = round(proportionOfAreaChanged, 2)), 
              size = 2.5, color = "grey10", check_overlap = TRUE) +
    scale_x_continuous(breaks = seq(-1, 5.3, by = 0.5)) +
    scale_y_discrete(position = "right")
  p4
  
  ggsave(device = "png", filename = file.path(outputFolder, 
                                              "proportionalChangeInArea.png"), 
         width = 8, height = 11)
  
  return(p4)
  }

calcColonization <- function(rasT0Path, rasT1Path = NULL, 
                             percentToDiscard = 0.3, rasName){
  if (is(rasT0Path, "character")){
    rasT0Path <- raster::raster(rasT0Path)
  }
  if (!is.null(rasT1Path)){
    if (is(rasT1Path, "character")){
      rasT1Path <- raster::raster(rasT1Path)
    }
  }
  rasT0 <- .presenceAbsenceRas(rasT0Path, percentToDiscard)
  if (!is.null(rasT1Path)){
    rasT1 <- .presenceAbsenceRas(rasT1Path, percentToDiscard)
    rasCol <- rasT1 - rasT0
  } else {
    rasCol <- rasT0
  }
  writeRaster(rasCol, filename = rasName, format = "GTiff")
  return(raster::raster(paste0(rasName, ".tif")))
}

.presenceAbsenceRas <- function(ras, percentToDiscard = 0.3){
  CSdt <- data.table::data.table(pixelID = 1:ncell(ras),
                                 val = getValues(ras))
  CSdt <- na.omit(CSdt)
  data.table::setkey(CSdt, val)
  CSdt[, CUM := cumsum(val)]
  CSdt[, CUMstd := CUM/sum(val)]
  CSdt[, PA := CUMstd > percentToDiscard]
  BIRDpres <- raster(ras)
  BIRDpres[CSdt[PA == TRUE, pixelID]] <- 1
  BIRDpres[CSdt[PA == FALSE, pixelID]] <- 0
  return(BIRDpres)
}
