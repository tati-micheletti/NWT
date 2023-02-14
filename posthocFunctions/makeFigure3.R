
makeFigure3 <- function(DTx1, yearsSelected = NULL, climateModel = NULL, areaChosen = NULL, 
                        grid = FALSE){
  
  plot1TableReady <- lapply(unique(DTx1$Species), function(sp){
    message(paste0("Processing ", sp, " (", which(unique(DTx1$Species) == sp),
                   " of ", length(unique(DTx1$Species)), ")"))
    
    habitat <- unique(DTx1[Species == sp, Habitat])
    toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                "Habitat", "caribou", "random", sp)
    DT <- DTx1[Species == sp, ..toKeep]
    if (!grid){
      if (!is.null(yearsSelected)){
        DT <- DT[Year %in% yearsSelected, ]
      }
      if (!is.null(climateModel)){
        DT <- DT[ClimateScenario %in% climateModel, ]
      }
      if (!is.null(areaChosen)){
        DT <- DT[ProportionAreaChosen %in% areaChosen, ]
      }
    } else {
      newDT <- rbindlist(lapply(seq_along(yearsSelected), function(index){
        DTr <- DT[ProportionAreaChosen %in% areaChosen[index] &
                    Year %in% yearsSelected[index], ]
        return(DTr)
      }))
      DT <- newDT
    }
    if (!grid){
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L", type = "deviation"),
                                    CI(umbrellaIndex, toReturn = "U", type = "deviation"))]
      DT <- unique(DT[, c("Species", "Habitat",
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    } else {
      DT2 <- rbindlist(lapply(yearsSelected, function(Y){
        # Need to keep year
        boo <- DT[Year == Y, caribou]
        low <- DT[Year == Y, random]
        upp <- DT[Year == Y, get(sp)]
        umbrellaIndex <- numeric(NROW(boo))
        for (i in 1:length(umbrellaIndex)){
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
        DT3 <- data.table(umbrellaMean = mean(umbrellaIndex),
                          umbrellaLCI = CI(umbrellaIndex, toReturn = "L", type = "deviation"), 
                          umbrellaUCI = CI(umbrellaIndex, toReturn = "U", type = "deviation"),
                          Year = Y)
        return(DT3)
      }))
      DT <- merge(DT, DT2)
      DT <- unique(DT[, c("Species", "Habitat", "Year",
                               "umbrellaMean", "umbrellaLCI", 
                               "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    }
  })
  plot1TableReady <- rbindlist(lapply(plot1TableReady, `[[`, "tb"))
  plot1TableReady[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                          "mixedwood", "shrub", "wetland"))]
  
  cols <- gg_color_hue(length(unique(plot1TableReady[["Habitat"]])))
  
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland")
  
  plot1TableReady[, Species := as.character(Species)]
  setkey(plot1TableReady, "Habitat")
  if (!grid){
  plot1TableReady[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
  setkey(plot1TableReady, Habitat, revorder)
  levs <- unique(plot1TableReady[, Species])
    plot1TableReady[, Species := factor(Species, levels = rev(levs))]
  } else {
  plot1TableReady <- rbindlist(lapply(yearsSelected, function(Y){
    A <- plot1TableReady[Year == Y, ]
    A[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
    setkey(A, Habitat, revorder)
    levs <- unique(A[, Species])
    A[, Species := factor(Species, levels = rev(levs))]
  return(A)
  }))
  }
  
  plot1 <- ggplot(data = plot1TableReady, aes(y = Species,
                                              fill = Habitat)) +
    geom_col(aes(x = umbrellaMean)) + 
    scale_fill_manual(values = setNames(cols,
                                        sps)) +
    geom_errorbar(aes(xmin = umbrellaLCI,
                      xmax = umbrellaUCI), 
                  color = "black") +
    labs(y = "Landbird Species", x = "Umbrella Index",
         fill = 'Landbird Species Group') + 
    coord_cartesian(xlim = c(-0.8, 0.8)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
  if (grid){
    plot1 <- plot1 + facet_grid(. ~ Year)
  }
  # if (!is.null(climateModel)){
  #   plot1 <- plot1 + facet_grid(. ~ ClimateScenario)
  # }
  # if (!is.null(areaChosen)){
  #   plot1 <- plot1 + facet_grid(. ~ ProportionAreaChosen)
  # }
  return(plot1)
}

makeFigure3.1 <- function(DTx1, yearsSelected = NULL, climateModel = NULL, areaChosen = NULL, 
                        grid = FALSE){
  
  plot1TableReady <- lapply(unique(DTx1$Species), function(sp){
    message(paste0("Processing ", sp, " (", which(unique(DTx1$Species) == sp),
                   " of ", length(unique(DTx1$Species)), ")"))
    
    habitat <- unique(DTx1[Species == sp, Habitat])
    toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                "Habitat", "caribou", "random", sp)
    DT <- DTx1[Species == sp, ..toKeep]
    if (!grid){
      if (!is.null(yearsSelected)){
        DT <- DT[Year %in% yearsSelected, ]
      }
      if (!is.null(climateModel)){
        DT <- DT[ClimateScenario %in% climateModel, ]
      }
      if (!is.null(areaChosen)){
        DT <- DT[ProportionAreaChosen %in% areaChosen, ]
      }
    } else {
      newDT <- rbindlist(lapply(seq_along(areaChosen), function(index){
        DTr <- DT[ProportionAreaChosen %in% areaChosen[index] &
                    Year %in% yearsSelected[index], ]
        return(DTr)
      }))
      DT <- newDT
    }
    if (!grid){
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L", type = "deviation"),
                                    CI(umbrellaIndex, toReturn = "U", type = "deviation"))]
      DT <- unique(DT[, c("Species", "Habitat",
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    } else {
      DT2 <- rbindlist(lapply(areaChosen, function(Y){
        # Need to keep year
        boo <- DT[ProportionAreaChosen == Y, caribou]
        low <- DT[ProportionAreaChosen == Y, random]
        upp <- DT[ProportionAreaChosen == Y, get(sp)]
        umbrellaIndex <- numeric(NROW(boo))
        for (i in 1:length(umbrellaIndex)){
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
        DT3 <- data.table(umbrellaMean = mean(umbrellaIndex),
                          umbrellaLCI = CI(umbrellaIndex, toReturn = "L", type = "deviation"), 
                          umbrellaUCI = CI(umbrellaIndex, toReturn = "U", type = "deviation"),
                          ProportionAreaChosen = as.character(Y))
        return(DT3)
      }))
      DT[, ProportionAreaChosen := as.character(ProportionAreaChosen)]
      DT <- merge(DT, DT2)
      DT <- unique(DT[, c("Species", "Habitat", "ProportionAreaChosen",
                          "umbrellaMean", "umbrellaLCI", 
                          "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    }
  })
  plot1TableReady <- rbindlist(lapply(plot1TableReady, `[[`, "tb"))
  plot1TableReady[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                          "mixedwood", "shrub", "wetland"))]
  
  cols <- gg_color_hue(length(unique(plot1TableReady[["Habitat"]])))
  
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland")
  
  plot1TableReady[, Species := as.character(Species)]
  setkey(plot1TableReady, "Habitat")
  if (!grid){
    plot1TableReady[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
    setkey(plot1TableReady, Habitat, revorder)
    levs <- unique(plot1TableReady[, Species])
    plot1TableReady[, Species := factor(Species, levels = rev(levs))]
  } else {
    plot1TableReady <- rbindlist(lapply(areaChosen, function(Y){
      A <- plot1TableReady[ProportionAreaChosen == Y, ]
      A[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
      setkey(A, Habitat, revorder)
      levs <- unique(A[, Species])
      A[, Species := factor(Species, levels = rev(levs))]
      return(A)
    }))
  }
  plot1TableReady[ProportionAreaChosen == "0.15", ProportionAreaChosen := "Year 2011 - scenario 15%"] # Rename for plot
  plot1TableReady[ProportionAreaChosen == "0.45", ProportionAreaChosen := "Year 2091 - scenario 45%"]
  plot1 <- ggplot(data = plot1TableReady, aes(y = Species,
                                              fill = Habitat)) +
    geom_col(aes(x = umbrellaMean)) + 
    scale_fill_manual(values = setNames(cols,
                                        sps)) +
    geom_errorbar(aes(xmin = umbrellaLCI,
                      xmax = umbrellaUCI), 
                  color = "black") +
    labs(y = "Landbird Species", x = "Umbrella Index",
         fill = 'Landbird Species Group') + 
    coord_cartesian(xlim = c(-1, 1)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
  if (grid){
    plot1 <- plot1 + facet_grid(. ~ ProportionAreaChosen)
  }
  # if (!is.null(climateModel)){
  #   plot1 <- plot1 + facet_grid(. ~ ClimateScenario)
  # }
  # if (!is.null(areaChosen)){
  #   plot1 <- plot1 + facet_grid(. ~ ProportionAreaChosen)
  # }
  return(plot1)
}

makeFigure3.2 <- function(DTx1, yearsSelected = NULL, climateModel = NULL, areaChosen = NULL, 
                          grid = FALSE){
  
  plot1TableReady <- lapply(unique(DTx1$Species), function(sp){
    message(paste0("Processing ", sp, " (", which(unique(DTx1$Species) == sp),
                   " of ", length(unique(DTx1$Species)), ")"))
    
    habitat <- unique(DTx1[Species == sp, Habitat])
    toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                "Habitat", "caribou", "random", sp)
    DT <- DTx1[Species == sp, ..toKeep]
    if (!grid){
      if (!is.null(yearsSelected)){
        DT <- DT[Year %in% yearsSelected, ]
      }
      if (!is.null(climateModel)){
        DT <- DT[ClimateScenario %in% climateModel, ]
      }
      if (!is.null(areaChosen)){
        DT <- DT[ProportionAreaChosen %in% areaChosen, ]
      }
    } else {
      DT <- DT[ProportionAreaChosen %in% areaChosen, ]
      newDT <- rbindlist(lapply(seq_along(climateModel), function(index){
        DTr <- DT[Year %in% yearsSelected[index] &
                    ClimateScenario %in% climateModel[index], ]
        return(DTr)
      }))
      DT <- newDT
    }
    if (!grid){
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L", type = "deviation"),
                                    CI(umbrellaIndex, toReturn = "U", type = "deviation"))]
      DT <- unique(DT[, c("Species", "Habitat",
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    } else {
      DT2 <- rbindlist(lapply(climateModel, function(Y){
        # Need to keep year
        boo <- DT[ClimateScenario == Y, caribou]
        low <- DT[ClimateScenario == Y, random]
        upp <- DT[ClimateScenario == Y, get(sp)]
        umbrellaIndex <- numeric(NROW(boo))
        for (i in 1:length(umbrellaIndex)){
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
        DT3 <- data.table(umbrellaMean = mean(umbrellaIndex),
                          umbrellaLCI = CI(umbrellaIndex, toReturn = "L", type = "deviation"), 
                          umbrellaUCI = CI(umbrellaIndex, toReturn = "U", type = "deviation"),
                          ClimateScenario = Y)
        return(DT3)
      }))
      DT <- merge(DT, DT2)
      DT <- unique(DT[, c("Species", "Habitat", "ClimateScenario",
                          "umbrellaMean", "umbrellaLCI", 
                          "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NULL))
    }
  })
  plot1TableReady <- rbindlist(lapply(plot1TableReady, `[[`, "tb"))
  plot1TableReady[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                          "mixedwood", "shrub", "wetland"))]
  
  cols <- gg_color_hue(length(unique(plot1TableReady[["Habitat"]])))
  
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland")
  plot1TableReady[, Species := as.character(Species)]
  setkey(plot1TableReady, "Habitat")
  if (!grid){
    plot1TableReady[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
    setkey(plot1TableReady, Habitat, revorder)
    levs <- unique(plot1TableReady[, Species])
    plot1TableReady[, Species := factor(Species, levels = rev(levs))]
  } else {
    plot1TableReady <- rbindlist(lapply(climateModel, function(Y){
      A <- plot1TableReady[ClimateScenario == Y, ]
      A[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
      setkey(A, Habitat, revorder)
      levs <- unique(A[, Species])
      A[, Species := factor(Species, levels = rev(levs))]
      return(A)
    }))
  }
  plot1 <- ggplot(data = plot1TableReady, aes(y = Species,
                                              fill = Habitat)) +
    geom_col(aes(x = umbrellaMean)) + 
    scale_fill_manual(values = setNames(cols,
                                        sps)) +
    geom_errorbar(aes(xmin = umbrellaLCI,
                      xmax = umbrellaUCI), 
                  color = "black") +
    labs(y = "Landbird Species", x = "Umbrella Index",
         fill = 'Landbird Species Group') + 
    coord_cartesian(xlim = c(-0.5, 0.5)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
  if (grid){
    plot1 <- plot1 + facet_grid(. ~ ClimateScenario)
  }
  # if (!is.null(climateModel)){
  #   plot1 <- plot1 + facet_grid(. ~ ClimateScenario)
  # }
  # if (!is.null(climateModel)){
  #   plot1 <- plot1 + facet_grid(. ~ ClimateScenario)
  # }
  return(plot1)
}

makeFigure3.3 <- function(finalTable4){
  
  finalTableSpecies <- merge(finalTable4,
                             birdsGroupingTable, by = "Species", all.x = TRUE)
  finalTableSpecies[, c("Run", "ClimateScenario", "Year", 
                        "ProportionAreaChosen") := NULL]
  scientificTable <- prepInputs(url = "https://drive.google.com/file/d/1XwQ64r4_xP2o2XJcUDEJHnbvzjp_cZMw/view?usp=share_link",
                     destinationPath = Paths$inputPath, fun = "data.table::fread")
  names(scientificTable)[names(scientificTable) == "Species"] <- "CommonName"

  scientificTable[Code == "PHVI", `Scientific Name` := "Vireo philadelphicus"] # Needed shortcut as for some reason the darn cache is not letting go of my wrong file
  finalTableSpecies3[, Habitat := "average"]
  finalTableSpecies3[, Species := "average"]
  
  finalTableSpecies2 <- rbind(finalTableSpecies, finalTableSpecies3)
  finalTableSpecies2[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                             "mixedwood", "shrub", "wetland", "average"))]
  
  scientificTable <- rbind(scientificTable, data.table(Code = "average", 
                                                       CommonName = "average",
                                                       "Scientific Name" = "average"))
  finalTableSpecies2 <- merge(finalTableSpecies2, scientificTable, 
                              by.x = "Species", 
                              by.y = "Code")

  # finalTableSpecies2[, Species.y := NULL]
  
  names(finalTableSpecies2)[names(finalTableSpecies2) == "Scientific Name"] <- "ScientificName"
  
  pal <- gg_color_hue(length(unique(finalTableSpecies2[["Habitat"]]))-1)
  cols <- c(pal, "grey")
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland", "average")
  
  finalTableSpecies2[, Species := as.character(Species)]
  setkey(finalTableSpecies2, "Habitat")
  finalTableSpecies2[, umbrellaIndexAverage := mean(umbrellaIndex), by = "Species"]
  setkey(finalTableSpecies2, Species, Habitat, umbrellaIndex)
  finalTableSpecies2[, revorder := frankv(umbrellaIndexAverage,
                                            order=-1, ties.method = "first"),
                       by = "Habitat"]
    setkey(finalTableSpecies2, Habitat, revorder)
    levs <- unique(finalTableSpecies2[, Habitat])
    finalTableSpecies2[, Habitat := factor(Habitat, levels = rev(levs))]
    levs2 <- unique(finalTableSpecies2$Species)
    finalTableSpecies2[, Species := factor(Species, levels = rev(levs2))]
    
    # Coloring the species
    speciesColors <- data.table(Species = levels(finalTableSpecies2$Species))
    speciesColors[, speciesColors := fifelse(Species %in% c("CAWA", "OSFL", "RUBL"), "red", "black")]
    speciesColors <- speciesColors$speciesColors
    
    sciency <- unique(finalTableSpecies2$ScientificName)
    sciency <- unique(finalTableSpecies2$CommonName)
    
    pB <- ggplot(finalTableSpecies2, aes(x = umbrellaIndex, 
                                         y = Species, 
                                         fill = Habitat)) +
      geom_boxplot() +
      scale_fill_manual(values = setNames(cols,
                                          sps)) +
      theme_classic()  +
      labs(y = "Landbird Species", x = "Umbrella Index",
           fill = 'Landbird Species Group') + 
      scale_y_discrete(labels = rev(sciency)) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 16),
            axis.text = element_text(size = 13),
            axis.title = element_text(size = 16),
            # axis.text.y = element_text(colour = speciesColors,
            #                            face = "italic"))
            # axis.text.y = element_text(colour = speciesColors, family = "Consolas"))
            axis.text.y = element_text(colour = speciesColors))

  nm <- file.path(Paths$outputPath, 
                  "individualSp_boxplot_redo3.png")
  ggsave(device = "png", filename = nm, 
         width = 28, height = 30, units = "cm")
  drive_upload(nm, as_id("1D8-H4h-59vD3nea9sn3aICa4SqVRulQO"))
  
  return(pB)
}

makeFigure3.4 <- function(finalTable4, 
                          yearsSelected = NULL,
                          areaChosen = NULL, 
                          grid = FALSE,
                          uploadTo = NULL){
  finalTableSpecies <- merge(finalTable4,
                             birdsGroupingTable, by = "Species", all.x = TRUE)

  finalTableSpecies3 <- copy(finalTableSpecies)
    finalTableSpecies3[, Habitat := "average"]
    finalTableSpecies3[, Species := "average"]

  finalTableSpecies2 <- rbind(finalTableSpecies, finalTableSpecies3)
  finalTableSpecies2[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                             "mixedwood", "shrub", "wetland", "average"))]
  
  pal <- gg_color_hue(length(unique(finalTableSpecies2[["Habitat"]]))-1)
  cols <- c(pal, "grey")
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland", "average")
  
  finalTableSpecies2[, Species := as.character(Species)]
  setkey(finalTableSpecies2, "Habitat")
  
  y <- if (is.null(yearsSelected)) NULL else "Year"
  # cm <- if (is.null(climateModel)) NULL else "ClimateScenario"
  pac <- if (is.null(areaChosen)) NULL else "ProportionAreaChosen"
  byCat <- c(y, pac)
  
  if (!is.null(byCat)){
    
    if (!is.null(yearsSelected)) finalTableSpecies2 <- finalTableSpecies2[Year %in% yearsSelected, ]
    # if (!is.null(climateModel)) finalTableSpecies2 <- finalTableSpecies2[ClimateScenario %in% climateModel, ]
    if (!is.null(areaChosen)) finalTableSpecies2 <- finalTableSpecies2[ProportionAreaChosen %in% areaChosen, ]
    
    y <- if (is.null(yearsSelected)) "Year" else NULL
    # cm <- if (is.null(climateModel)) "ClimateScenario" else NULL
    pac <- if (is.null(areaChosen)) "ProportionAreaChosen" else NULL

    whichNeedsToBeNull <- c("Run", y, pac)
    finalTableSpecies2[, paste0(whichNeedsToBeNull) := NULL]
    finalTableSpecies2[, c("ClimateScenario") := NULL]
    finalTableSpecies2[, umbrellaIndexAverage := mean(umbrellaIndex), by = c("Species", byCat)]
  } else {
    
    if (is.null(yearsSelected)) finalTableSpecies2[, c("Year") := NULL]
    if (is.null(areaChosen)) finalTableSpecies2[, c("ProportionAreaChosen") := NULL]
    
    finalTableSpecies2[, c("ClimateScenario") := NULL]
    finalTableSpecies2[, umbrellaIndexAverage := mean(umbrellaIndex), by = "Species"]
  }
# Need to organize the order by the order in the specific year 1 and scenario 1
  finalTableSpecies3 <- finalTableSpecies2[Year == yearsSelected[1] & ProportionAreaChosen == areaChosen[1] | 
                                             Year == yearsSelected[2] & ProportionAreaChosen == areaChosen[2], ]
  setkey(finalTableSpecies3, Species, Habitat, umbrellaIndex)
  orderTb <- Copy(unique(finalTableSpecies3[Year == yearsSelected[1], c("Species", "Habitat", "umbrellaIndexAverage")]))
  orderTb[, revorder := frankv(umbrellaIndexAverage,
                                          order=-1, ties.method = "first"),
                     by = "Habitat"]
  DTtoMerge <- orderTb[, c("Species", "revorder")]
  finalTableSpecies3 <- merge(finalTableSpecies3, DTtoMerge, all.x = TRUE, by = "Species")
  setkey(finalTableSpecies3, Habitat, revorder)
  levs <- unique(finalTableSpecies3[, Habitat])
  finalTableSpecies3[, Habitat := factor(Habitat, levels = rev(levs))]
  levs2 <- unique(finalTableSpecies3$Species)
  finalTableSpecies3[, Species := factor(Species, levels = rev(levs2))]

  if (all(!is.null(yearsSelected),
          !is.null(areaChosen))){
    finalTableSpecies3[Year %in% yearsSelected[1] &
                         ProportionAreaChosen %in% areaChosen[1], 
                       pl := paste0(yearsSelected[1], " at ", areaChosen[1], "%")]
    finalTableSpecies3[Year %in% yearsSelected[2] &
                         ProportionAreaChosen %in% areaChosen[2], 
                       pl := paste0(yearsSelected[2], " at ", areaChosen[2], "%")]
    finalTableSpecies3 <- finalTableSpecies3[!is.na(pl), ]
  }
  
  pB <- ggplot(finalTableSpecies3, aes(x = umbrellaIndex, 
                                       y = Species, 
                                       fill = Habitat)) +
    geom_boxplot() +
    scale_fill_manual(values = setNames(cols,
                                        sps)) +
    theme_classic()  +
    labs(y = "Landbird Species", x = "Umbrella Index",
         fill = 'Landbird Species Group') + 
    geom_vline(xintercept = 0, linetype = "dashed") +
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12),
          axis.text = element_text(size = 12))
  
  if (all(!is.null(yearsSelected),
          !is.null(areaChosen))){
    pB <- pB + facet_grid(. ~ pl)
  }

  nm <- file.path(Paths$outputPath, 
                  "individualSp_boxplot_2scen.png")
  ggsave(device = "png", filename = nm, 
         width = 8, height = 16)
  if (!is.null(uploadTo))
    drive_upload(nm, as_id(uploadTo))
  
  return(pB)
}
