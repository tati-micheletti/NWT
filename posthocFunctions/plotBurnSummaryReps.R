plotBurnSummaryReps <- function(dataPath,
                             typeSim,
                             lastYear,
                             theObject = NULL,
                             overwrite = FALSE){
  
  # Updated from plotBurnSummary2
  fileName <- file.path(dataPath, paste0("burnSummary", typeSim, ".png"))
  if (all(file.exists(fileName), !isTRUE(overwrite))){
    message("Plot exist and overwrite is FALSE. Returning plot path")
    return(fileName)
  }

  # 1) Add all reps together
  # 2) Add a third pannel with average fire size per year
  # 3) If possible, change lm to gamm
  
  parSetup <- par()
  invisible(on.exit(par(parSetup)))
  par(mfrow=c(2, 1))
  
  # FIRE
  if (!is.null(theObject)){
    burnSumm <- theObject
  } else {
    allFl <- list.files(file.path(dataPath, typeSim), 
                        recursive = TRUE, 
                        pattern = paste0("burnSummary_year",
                                         lastYear,
                                         ".rds"),
                        full.names = TRUE)
    burnSumm <- rbindlist(lapply(seq_along(allFl), 
                                     FUN = function(fl){
        burnTab <- readRDS(allFl[fl])
        burnTab[, repetition := fl]
        return(burnTab)
        }))
  }
  
  # Area burned #######################
  areaB <- burnSumm[, sumABrep := sum(areaBurned), by = c("year", "repetition")]
  
  areaB <- burnSumm[, c("sumAB", "devAB") := list(mean(sumABrep),
                                                 sd(sumABrep)), 
                    by = year]
  areaB <- data.table(year = areaB$year, val = areaB$sumAB, 
                      var = "area_burned", dev = areaB$devAB)
  areaB <- unique(areaB)
  areaB <- areaB[, val := val/1000] # Doing this so I can plot the axis with mostly the same limits. 
  areaB <- areaB[, dev := dev/1000] # Doing this so I can plot the axis with mostly the same limits. 
  #Needs to be informed in the captions!!
  # Could eventually implement something as: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
  
  tend <-lm(val ~ year, data = areaB)
  require(stats)
  coeff <- coefficients(tend)
  Fstats <- summary(tend)$fstatistic
  names(Fstats) <- NULL
  pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
  
  # N fires #######################
  nFires <- burnSumm[, NfiresRep := length(N), by = c("year", "repetition")]
  nFires <- burnSumm[, c("Nfires", "devNfires") := list(mean(NfiresRep),
                                                   sd(NfiresRep)), 
                     by = year]
  nFires <- data.table(year = nFires$year, val = nFires$Nfires, 
                       var = "number_fires", dev = nFires$devNfires)
  nFires <- unique(nFires)
  tendF <-lm(val ~ year, data = nFires)
  require(stats)
  coeffF <- coefficients(tendF)
  
  # 1. See if the trend in fires is statistically significant
  Fstats <- summary(tendF)$fstatistic
  names(Fstats) <- NULL
  pValueF <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
  
  # Fire size #######################
  fireSize <- burnSumm[areaBurned > 6.25, fireSize := mean(areaBurned, na.rm = TRUE), 
                       by = c("year", "repetition")]
  fireSize <- burnSumm[, c("meanFireSize", "devFireSize") := list(mean(fireSize, na.rm = TRUE),
                                                                  sd(fireSize, na.rm = TRUE)), 
                     by = year]
  fireSize <- data.table(year = fireSize$year, val = fireSize$meanFireSize, 
                       var = "fire_size", dev = fireSize$devFireSize)
  fireSize <- unique(fireSize)
  fireSize <- fireSize[, val := val/10] # Doing this so I can plot the axis with mostly the same limits. 
  fireSize <- fireSize[, dev := dev/10] # Doing this so I can plot the axis with mostly the same limits. 
  
  tendS <-lm(val ~ year, data = fireSize)
  require(stats)
  coeffS <- coefficients(tendS)
  
  # 1. See if the trend in fires is statistically significant
  Fstats <- summary(tendS)$fstatistic
  names(Fstats) <- NULL
  pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
  
  coefXA <- round(coeff[2],1)
  coefYA <- round(coeff[1],1)
  coefXF <- round(coeffF[2],1)
  coefYF <- round(coeffF[1],1)
  coefXS <- round(coeffS[2],1)
  coefYS <- round(coeffS[1],1)
  
  # New facet label names for dose variable
  replacementNames <- c(paste0("Area burned: ",
                               "y = ", ifelse(coefXA < 10000, coefXA, formatC(coefXA, format = "e", digits = 2)),
                               "x + ", ifelse(coefYA < 10000, coefYA, formatC(coefYA, format = "e", digits = 2)), pValueA),
                        paste0("No fires: ",
                               "y = ", ifelse(coefXF < 10000, coefXF, formatC(coefXF, format = "e", digits = 2)),
                               "x + ", ifelse(coefYF < 10000, coefYF, formatC(coefYF, format = "e", digits = 2)), pValueF),
                        paste0("Mean fire size: ",
                               "y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
                               "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)), pValueS))
  names(replacementNames) <- c("area_burned", "number_fires", "fire_size")
  
  dt <- rbind(areaB, nFires, fireSize)
  
  p1 <- ggplot2::ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
    geom_point() +
    stat_smooth(method = "lm", color = "darkred", fill = "red") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
    coord_cartesian(ylim = c(100, 1500)) +
    labs(y = "ha x 10^3")
  p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
    geom_point(colour = "black") +
    stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(0, 500)) +
    ylab(label = "no. of fires")
  p3 <- ggplot2::ggplot(data = dt[var == "fire_size",], aes(x = year, y = val)) +
    geom_point(colour = "black") +
    stat_smooth(method = "lm", color = "orange", fill = "orange") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
    coord_cartesian(ylim = c(100, 1500)) +
    labs(y = "ha x 10")
  
  p <- gridExtra::grid.arrange(p1, p2, p3, ncol=1,
                               top = grid::textGrob(typeSim, gp = grid::gpar(fontsize = 12)))
  
  ggsave(fileName, plot = p, dpi = 600)
  
  return(list(fileLocation = fileName, model = list(areaBurned = tend, 
                                                    noFires = tendF, 
                                                    fireSize = tendS)))
}
