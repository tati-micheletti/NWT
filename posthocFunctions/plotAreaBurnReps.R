plotAreaBurnReps <- function(dataPath,
                                typeSim,
                                lastYear,
                                theObject = NULL,
                             yCrossingPlot = FALSE,
                                overwrite = FALSE){
  
  # Make the difference between the two scenarios passed in typeSim
  fileName <- file.path(dataPath, paste0("areaBurnedReps_", paste(typeSim, collapse = "_"), ".png"))
  if (all(file.exists(fileName), !isTRUE(overwrite))){
    message("Plot exist and overwrite is FALSE. Returning plot path")
    return(fileName)
  }
  
  if (!is.null(theObject)){
    burnSumm <- theObject
  } else {
    allFl <- list.files(file.path(dataPath, typeSim), 
                        recursive = TRUE, 
                        pattern = paste0("burnSummary_year",
                                         lastYear,
                                         ".rds"),
                        full.names = TRUE)
    
    burnSumm <- rbindlist(lapply(typeSim, function(typeSimulation){
      fls <- allFl[grep(allFl, pattern = typeSimulation)]
      oneTypeSim <- rbindlist(lapply(seq_along(fls), 
                                     FUN = function(fl){
                                       burnTab <- readRDS(fls[fl])
                                       burnTab[, repetition := fl]
                                       burnTab[, scenario := typeSimulation]
                                       return(burnTab)
                                     }))
      return(oneTypeSim)
    }))
  }
  
  ####################### Area burned #######################
  
  burnSumm[, sumAB := sum(areaBurned), by = c("year", "repetition", "scenario")]
  areaB <- unique(burnSumm[, c("year", "repetition", "sumAB", "scenario")])
  areaB[, var := "Annual Area Burned"]
  areaB[, val := sumAB/1000] # Doing this so I can plot the axis with mostly the same limits. 
  # Could eventually implement something as: https://fishandwhistle.net/post/2018/modifying-facet-scales-in-ggplot2/
  
  # See if the trend in area burned is statistically significant
  trends <- lapply(typeSim, function(typeSimulation){
    areaBsubset <- areaB[scenario == typeSimulation, ]
    tend <-lm(sumAB ~ year, data = areaBsubset)
    require(stats)
    coeff <- coefficients(tend)
    Fstats <- summary(tend)$fstatistic
    names(Fstats) <- NULL
    pValueA <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.01, " \n(significant)", " \n(non-significant)")
    return(list(coefficients = coeff, mod = tend, pVal = pValueA))
  })
  names(trends) <- typeSim
  
  #######################   P L O T   #######################
  dt <- areaB
  # Now remove original variable. It uses the first item's nameL sumAB
  dt[, sumAB := NULL]
  
  # Melt the DT to separate val from both scenarios in diff columns
  # Do the difference at a repetition+year level (LandR.CS_fS-LandR_SCFM)
  # plot only the difference
  DT <- dcast(dt, year + repetition + var ~ scenario, value.var = "val")
  DT[, diffPoints := LandR.CS_fS-LandR_SCFM]
  diffInPredictlm <- lm(formula = diffPoints ~ year, data = DT)
  
  DT[, Year := year - mean(year)]
  DT[, Year2 := year^2]
  diffInPredictlm2 <- lm(formula = diffPoints ~ Year * Year^2, data = DT)
  
  coeff <- coefficients(diffInPredictlm)
  Fstats <- summary(diffInPredictlm)$fstatistic
  names(Fstats) <- NULL
  pValueA <- ifelse(pf(Fstats[1], Fstats[2], 
                       Fstats[3], lower.tail = F) < 0.01, 
                    " \n(significant)", 
                    " \n(non-significant)")
  coefXA1 <- round(coeff[2],1)
  coefYA1 <- round(coeff[1],1)
  
  # New facet label names for dose variable
  replacementNames <- paste("Difference in area burned: ",
                               "y = ", ifelse(coefXA1 < 10000, coefXA1, formatC(coefXA1, format = "e", digits = 2)),
                               "x + ", ifelse(coefYA1 < 10000, coefYA1, formatC(coefYA1, format = "e", digits = 2)), 
                               trends[[typeSim[1]]][["pVal"]])
  
  names(replacementNames) <- "Annual Area Burned"

  # PLOT
    p1 <- ggplot2::ggplot(data = DT) +
    geom_point(aes(x = year, y = diffPoints), alpha = 0.4) +
    scale_colour_manual(values = "darkblue") +
    facet_grid(var ~ ., labeller = labeller(var = replacementNames)) +
    theme(legend.position = "none",
          strip.text.y = element_text(size = 9, face = "bold"),
          plot.margin = unit(c(0.2, 0.2, -0.01, 0.2), "cm"),
          panel.background = element_rect(fill = "white"),
          panel.grid.major.y = element_line(size = 0.15, linetype = 'solid',
                                          colour = "grey"), 
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank()
          ) +
      ylim(min(DT[["diffPoints"]]), 1500) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 30)) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(y = expression(paste("ha x ", 10^{3}))) +
    stat_smooth(aes(x = year, y = diffPoints), 
                method = "lm", se = TRUE, 
                fill="red", fullrange = TRUE,
                colour="darkred", linetype = 6) +
      geom_hline(yintercept = 0, colour="grey50", 
                 linetype = 4, size = 1)
    # geom_line(mapping = aes(x = year, y = difference), 
    #           color = "black", linetype = 6, 
    #           size = 0.8)
  if (yCrossingPlot)
    p1 <- p1 + xlim(1950, 2100)
    
  ggsave(fileName, plot = p1, width = 11, height = 8)
  return(list(fileName = fileName, mod1 = diffInPredictlm, mod2 = diffInPredictlm2))
}
