plotBurnSummaryRepsCaribou <- function(dataPath,
                             typeSim,
                             lastYear,
                             reps = paste0("run", 1:5),
                             theObject = NULL,
                             overwrite = FALSE,
                             doStatsManually = FALSE){
  
  # Updated from plotBurnSummary2
    fileName <- file.path(dataPath, paste0("burnSummaryReps_", paste(typeSim, collapse = "_"), ".png"))
  if (all(file.exists(fileName), !isTRUE(overwrite))){
    message("Plot exist and overwrite is FALSE. Returning plot path")
    return(fileName)
  }
  
  parSetup <- par()
  invisible(on.exit(par(parSetup)))
  par(mfrow=c(2, 1))
  
  # FIRE
  if (!is.null(theObject)){
    burnSumm <- theObject
  } else {
      # grep all folders that are in the type
      allDirs <- list.dirs(dataPath, recursive = FALSE)
      whichPaths <- allDirs[grepl(x = allDirs, pattern = paste(typeSim, collapse = "|"))]
      allFl <- list.files(whichPaths, 
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
  
  ####################### Area burned #######################

  burnSumm[, sumAB := sum(areaBurned), by = c("year", "repetition")]
  areaB <- unique(burnSumm[, c("year", "repetition", "sumAB")])
  
  # See if the trend in area burned is statistically significant
  tend <-lm(sumAB ~ year, data = areaB)
  require(stats)
  coeff <- coefficients(tend)
  Fstats <- summary(tend)$fstatistic
  names(Fstats) <- NULL
  pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
  
  areaB[, var := "area_burned"]
  areaB[, val := sumAB/1000] # Doing this so I can plot the axis with mostly the same limits. 
  # Could eventually implement something as: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
  
  ####################### N fires #######################

  burnSumm[, Nfires := length(N), by = c("year", "repetition")]
  nFires <- unique(burnSumm[, c("year", "repetition", "Nfires")])
  
  # See if the trend in fires is statistically significant
  tendF <-lm(Nfires ~ year, data = nFires)
  require(stats)
  coeffF <- coefficients(tendF)
  Fstats <- summary(tendF)$fstatistic
  names(Fstats) <- NULL
  pValueF <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
  nFires[, var := "number_fires"]
  nFires[, val := Nfires] # Doing this so I can plot the axis with mostly the same limits. 
  
  ####################### Fire size #######################

  burnSumm[areaBurned > 6.25, fireSize := mean(areaBurned, na.rm = TRUE), 
                       by = c("year", "repetition")]
  fireSize <- na.omit(unique(burnSumm[, c("year", "repetition", "fireSize")]))
  
  # See if the trend in fires is statistically significant
  tendS <-lm(fireSize ~ year, data = fireSize)
  require(stats)
  coeffS <- coefficients(tendS)
  Fstats <- summary(tendS)$fstatistic
  names(Fstats) <- NULL
  pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
  
  fireSize[, var := "fire_size"]
  fireSize[, val := fireSize/10] # Doing this so I can plot the axis with mostly the same limits. 

  #######################   P L O T   #######################

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
  
  dt <- rbind(areaB, nFires, fireSize, use.names = FALSE)
  # Now remove original variable. It uses the first item's nameL sumAB
  dt[, sumAB := NULL]
  
  if (doStatsManually){ # Code to fit the model to the raw data, not just average
    browser()
    burnSummOriginal <- copy(burnSumm)
    burnSumm$centeredYear <- scale(burnSumm$year, scale = FALSE)
    burnSumm$centeredYear2 <- scale(burnSumm$year ^2, scale = FALSE)
    burnSummSimple <- unique(burnSumm[, c("centeredYear", "sumAB", "centeredYear2", "year")])
    
    p0 <- ggplot2::ggplot(data = burnSummSimple, aes(x = year, y = sumAB)) +
      geom_point() +
      stat_smooth(method = "lm", color = "darkred", fill = "red")
    
    tend1 <-lm(sumAB ~ centeredYear, data = burnSummSimple)
    tend2 <-lm(sumAB ~ centeredYear + centeredYear2, data = burnSummSimple)
    tend1AIC <- AIC(tend1)
    tend2AIC <- AIC(tend2)
    coef(tend1)
    coef(tend2)
    # HERE
    require(stats)
    coeff <- coefficients(tend1)
    Fstats <- summary(tend)$fstatistic
    names(Fstats) <- NULL
    pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
    
    require(stats)
    coeff <- coefficients(tend2)
    Fstats <- summary(tend)$fstatistic
    names(Fstats) <- NULL
    pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, " \n(significant)", " \n(non-significant)")
  }
  
  p1 <- ggplot2::ggplot(data = dt[var == "area_burned",], aes(x = year, y = val)) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", color = "darkred", fill = "red") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm")) +
    coord_cartesian(ylim = c(100, 1500)) +
    labs(y = expression(paste("ha x ", 10^{3}))) 
  p2 <- ggplot(data = dt[var == "number_fires",], aes(x = year, y = val, colour = "blue")) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", fill = "blue", color = "darkblue") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    coord_cartesian(ylim = c(200, 700)) +
    ylab(label = "no. of fires")
  p3 <- ggplot2::ggplot(data = dt[var == "fire_size",], aes(x = year, y = val)) +
    geom_point(colour = "grey70") +
    stat_smooth(method = "lm", color = "orange", fill = "orange") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(-0.01, 0.2, 0.2, 0.2), "cm")) +
    coord_cartesian(ylim = c(0, 300)) +
    labs(y = "ha x 10")
  
  p <- gridExtra::grid.arrange(p1, p2, p3, ncol=1)#,
                               # top = grid::textGrob(typeSim, gp = grid::gpar(fontsize = 12)))
  
  ggsave(fileName, plot = p, width = 11, height = 8)
  qs::qsave(dt, file.path(dataPath, paste0("burnSummaryTable", paste(typeSim, collapse = "_"),".qs")))
  return(list(fileLocation = fileName, model = list(areaBurned = tend, 
                                                    noFires = tendF, 
                                                    fireSize = tendS)))
}
