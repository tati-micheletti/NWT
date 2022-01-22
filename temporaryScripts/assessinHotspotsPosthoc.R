Require("usefulFuns")
patt <- paste(paste0("_", as.character(as.roman(42:51))), collapse = "|")
fls <- grepMulti(list.files(path = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1", 
                            full.names = TRUE), patterns = patt, unwanted = "mean")
for (i in 1:length(fls)){
  file.copy(from = fls[i], to = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1/randomBootstrapped")
  message(paste0("File ", i, " of ", length(fls), " copied."))
}

for (i in 1:length(fls)){
  unlink(fls[i], recursive = FALSE)
  message(paste0("File ", i, " of ", length(fls), " deleted."))
}


allFls <- list.files("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1")
length(allFls)
allFls2 <- list.files("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1")
length(allFls2)
length(allFls)-length(allFls2)


function(ras, percentageArea, reps = 100){
  totalArea <- sum(ras[], na.rm = TRUE)
  totPix <- round(percentageArea*totalArea, 0)
  ras[] <- ras[]
  allPix <- na.omit(data.table(pixelID = 1:ncell(ras),
                               vals = getValues(ras)))
  if (reps > 1){
    bootSelected <- raster::calc(raster::stack(lapply(1:reps, function(Rep){
      selectedPix <- sample(allPix[["pixelID"]], totPix)
      selected <- ras
      selected[!is.na(ras[])] <- 0
      selected[selectedPix] <- 1
      return(selected)
    })), fun = mean, na.rm = TRUE)
  } else {
    selected <- sample(allPix[["pixelID"]], totPix)
    bootSelected <- ras
    bootSelected[!is.na(ras[])] <- 0
    bootSelected[selected] <- 1
  }
  return(bootSelected)
}

################################
# FINAL PLOT AND TABLE TRIAL 2 #
################################

CI <- function (x, ci = 0.95, toReturn = "mean"){
  a <- mean(x)
  s <- sd(x)
  n <- length(x)
  error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  if (toReturn == "mean")
    return(a) else
      if (toReturn == "L")
        return(a-error) else
          if (toReturn == "U")
            return(a+error)
}

umbrellaTable <- rbindlist(lapply(unique(DT2$Species), function(sp){
  DT <- dcast(DT2, 
              Species + Run + Year + ClimateScenario + ProportionAreaChosen ~ PrioritizedFor,
              value.var = "ProportionIndividualsConserved")
  toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
              "caribou", sp)
  DT <- DT[Species == sp, ..toKeep]
  setkey(DT, "Species", "Year", "ProportionAreaChosen", "ClimateScenario", "Run")
  DT[, higherValueUmbrella := fifelse(caribou > get(sp),
                                      TRUE, FALSE)]
  DT[higherValueUmbrella == TRUE, ]
  DTr <- data.table(Species = sp,
                    umbrellaHigher = NROW(DT[higherValueUmbrella == TRUE]),
                    umbrellaLower = NROW(DT[higherValueUmbrella == FALSE]))
  return(DTr)
}))

DTx <- dcast(DT2[Species != "caribou",], 
             Species + Run + Year + ClimateScenario + ProportionAreaChosen ~ PrioritizedFor,
             value.var = "ProportionIndividualsConserved")

toRemove <- FALSE

comparisonTableExtra <- lapply(unique(DTx$Species), function(sp){
  allYears <- lapply(unique(DTx$Year), function(Y){
    allAreas <- lapply(unique(DTx$ProportionAreaChosen), function(A){
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "caribou", "random", sp)
      DT <- DTx[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      if (toRemove){
        # ID and Remove rows where random is bigger than the reference --> cause for artifacts
        rowsToRemove <- which(DT[["random"]] > DT[[sp]])
        toExcl <- data.table()
        if (length(rowsToRemove) != 0){
          toExcl <- DT[rowsToRemove]
          # Warning about which Species, Year, climate scenario, and Run we have this problem
          message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                        crayon::red(NROW(toExcl))," points for ", crayon::red(sp), 
                                        " year ", crayon::red(Y), " area ", crayon::red(A), 
                                        " and ", 
                                        crayon::red(paste(unique(toExcl[["ClimateScenario"]]), 
                                                          collapse = "; ")))))
          DT <- DT[-rowsToRemove]
          if (NROW(DT) == 0){
            return(list(tb = NULL, pointsExcluded = NROW(toExcl)))
          }
        }
      } else {
        browser()
      }
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      
      # keepIterating <- TRUE
      # while (keepIterating) {
      #   bef <- length(umbrellaIndex)
      #   umbrellaIndexOK <- remove_outliers(umbrellaIndex)
      #   if (length(umbrellaIndexOK) != length(umbrellaIndex)){
      #     warning(paste0("Outliers removed for ", sp, " year ", Y,
      #                    " area ", A, ": ", paste(round(umbrellaIndex[attr(umbrellaIndexOK, 
      #                                                                      "na.action")], 1),
      #                                             collapse = "; ")), 
      #             immediate. = TRUE)
      #   }
      #   umbrellaIndex <- umbrellaIndexOK
      #   aft <- length(umbrellaIndex)
      #   if (bef == aft){
      #     keepIterating <- FALSE
      #   }
      # }
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L"),
                                    CI(umbrellaIndex, toReturn = "U"))]
      DT <- unique(DT[, c("Species", "Year", "ProportionAreaChosen", 
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = NROW(toExcl)))
    })
    allA <- rbindlist(lapply(allAreas, `[[`, "tb"))
    allPex <- sum(c(sapply(allAreas, `[[`, "pointsExcluded")))
    return(list(tb = allA, pointsExcluded = allPex))
  })
  allY <- rbindlist(lapply(allYears, `[[`, "tb"))
  allPex <- sum(c(sapply(allYears, `[[`, "pointsExcluded")))
  return(list(tb = allY, pointsExcluded = allPex))
})
pointsExcluded <- sum(c(sapply(comparisonTableExtra, `[[`, "pointsExcluded")))/NROW(DTx)
comparisonTableExtra <- rbindlist(lapply(comparisonTableExtra, `[[`, "tb"))

finalPlot <- ggplot(data = comparisonTableExtra, aes(x = Year)) +
  geom_line(aes(y = umbrellaMean, 
                group = Species,
                color = Species), 
            size = 1.5, show.legend = F) +
  geom_ribbon(aes(ymin = umbrellaLCI,
                  ymax = umbrellaUCI,
                  group = Species,
                  fill = Species), alpha = 0.5, colour = NA) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species), scales = "free_y") +
  xlab("Time") + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1) +
  ylab(paste0("Umbrella Index")) +
  # scale_y_continuous(breaks = seq(-1, 1, by = 0.2),
  #                    limits = c(-1, 1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
finalPlot

finalPlot2 <- ggplot(data = comparisonTableExtra, aes(x = Year)) +
  geom_line(aes(y = umbrellaMean, 
                group = Species,
                color = Species), 
            size = 1.1, show.legend = F) +
  geom_ribbon(aes(ymin = umbrellaLCI,
                  ymax = umbrellaUCI,
                  group = Species,
                  fill = Species), alpha = 0.5, colour = NA) +
  geom_line(aes(y = randomMean),
            color = "black", 
            size = 1, show.legend = F) +
  geom_ribbon(aes(ymin = randomLCI,
                  ymax = randomUCI), alpha = 0.4, colour = "grey") +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species)) +
  xlab("Time") + 
  geom_hline(yintercept = 1) +
  ylab(paste0("Index")) +
  scale_y_continuous(breaks = seq(0.4, 1.1, by = 0.1),
                     limits = c(0.3, 1.1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
finalPlot2


# TABLE

DTt <- DT2[Species != "caribou" & 
             ProportionAreaChosen %in% seq(0.25, 0.55, by = 0.1), ]

DTd <- dcast(DTt,
             Species + Run + Year + ClimateScenario + ProportionAreaChosen ~ PrioritizedFor,
             value.var = "ProportionIndividualsConserved")

finalTable <- rbindlist(lapply(unique(DTd$Species), function(sp){
  allYears <- rbindlist(lapply(unique(DTd$Year), function(Y){
    allAreas <- rbindlist(lapply(unique(DTd$ProportionAreaChosen), function(A){
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "caribou", sp, "random")
      DT <- DTd[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      # ID and Remove rows where random is bigger than the reference --> cause for artifacts
      rowsToRemove <- which(DT[["random"]] > DT[[sp]])
      toExcl <- data.table()
      if (length(rowsToRemove) != 0){
        toExcl <- DT[rowsToRemove]
        # Warning about which Species, Year, climate scenario, and Run we have this problem
        message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                      crayon::red(NROW(toExcl))," points for ", crayon::red(sp), 
                                      " year ", crayon::red(Y), " area ", crayon::red(A), 
                                      " and ", 
                                      crayon::red(paste(unique(toExcl[["ClimateScenario"]]), 
                                                        collapse = "; ")))))
        DT <- DT[-rowsToRemove]
        if (NROW(DT) == 0){
          return(list(tb = NULL, pointsExcluded = NROW(toExcl)))
        }
      }
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      DT[, umbrellaIndex := umbrellaIndex]
      return(DT[, c("Species", "umbrellaIndex")])
    }))
  }))
}))

finalTableDT <- data.table(finalTable)
finalTableM <- melt(finalTableDT, id.vars = "Species", 
                    measure.vars = "umbrellaIndex")
# finalTableM <- melt(finalTableDT, id.vars = "Species", 
#                     measure.vars = c("umbrellaIndex", "randomIndex"))

finalTableF <- finalTableM[, c("meanIndex",
                               "LCIIndex",
                               "UCIIndex") := list(mean(value),
                                                   CI(value, toReturn = "L"),
                                                   CI(value, toReturn = "U")), by = c("Species",
                                                                                      "variable")]
finalTableF <- unique(finalTableF[, c("Species", #"variable", 
                                      "meanIndex", "LCIIndex", "UCIIndex")])
# General Average
finalTableA <- finalTableDT$umbrellaIndex
finalTableA <- data.table(Species = "average", 
                          meanIndex = mean(finalTableA),
                          LCIIndex = CI(finalTableA, toReturn = "L"),
                          UCIIndex = CI(finalTableA, toReturn = "U"))

finalTablePlotDT <- rbind(finalTableA, finalTableF)

finalTablePlotDT[, Species := factor(Species, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                         "shrub", "wetland", "average"))]
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

pal <- gg_color_hue(length(unique(finalTablePlotDT[["Species"]]))-1)

cols <- c(pal, "grey")

finalTablePlotDT[, plotPlace := fifelse(Species != "average", "All Groups", "Average")]

p <- ggplot(finalTablePlotDT, aes(x = Species, fill = Species)) + 
  geom_bar(aes(y = meanIndex), stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values = setNames(cols,
                                      vars),
                    breaks = c("meanIndex"),
                    labels = c("Umbrella Index")) +
  scale_color_manual(values = rep("black", times = 7)) +
  geom_errorbar(aes(ymin = LCIIndex, 
                    ymax = UCIIndex,
                    color = Species), 
                position = position_dodge(0.9), size = 0.8, width = 0.25)+
  labs(x = "Landbird Species Group", y = "Index") + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.05),
                     limits = c(0, 1), expand = c(0,0)) +
  geom_hline(yintercept = 0.65, linetype = "dashed") + #finalTablePlotDT[Species == "average", meanIndex]
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(), 
        axis.text = element_text(size = 12))  #+
# guides(fill = guide_legend(override.aes = 
#                              list(color = "black",
#                                   fill = c("gray", "blueviolet"))),
#        color = FALSE)
p

### THROUGH TIME

DTtime <- DT2[Species != "caribou", ]

throughTime <- rbindlist(lapply(unique(DTtime[["Species"]]), function(sp){
  allProportions <- rbindlist(lapply(unique(DTtime[["ProportionAreaChosen"]]), function(P){
    DT <- DTtime[Species == sp & ProportionAreaChosen == P & PrioritizedFor %in% c("caribou", 
                                                                                   "random", sp),]
    DTdcast <- dcast(DT,
                     Year + Run + ClimateScenario ~ PrioritizedFor, 
                     value.var = c("ProportionIndividualsConserved"))
    # ID and Remove rows where random is bigger than the reference --> cause for artifacts
    rowsToRemove <- which(DTdcast[["random"]] > DTdcast[[sp]])
    toExcl <- data.table()
    if (length(rowsToRemove) != 0){
      toExcl <- DTdcast[rowsToRemove]
      # Warning about which Species, Year, climate scenario, and Run we have this problem
      message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                    crayon::red(NROW(toExcl))," points for ", crayon::red(sp),
                                    " area ", crayon::red(P), 
                                    " and ", crayon::red(paste(unique(toExcl[["ClimateScenario"]]), 
                                                               collapse = "; ")))))
      DTdcast <- DTdcast[-rowsToRemove]
      if (NROW(DTdcast) == 0){
        return(data.table(Species = sp,
                          ProportionAreaChosen = P,
                          umbrellaIndex = NA))
      }
    }
    boo <- DTdcast[["caribou"]]
    low <- DTdcast[["random"]]
    upp <- DTdcast[[sp]]
    umbrellaIndex <- numeric(NROW(DTdcast))
    for (i in 1:length(umbrellaIndex)){
      umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
    }
    DTdcast[, umbrellaIndex := umbrellaIndex]
    lmU <- summary(lm(DTdcast, formula = umbrellaIndex ~ Year))
    lmUdir <- ifelse(lmU$coefficients[2] < 0, -1, 1)
    lmUsig <- ifelse(lmU$coefficients[length(lmU$coefficients)] < 0.05, 1, 0)
    # lmR <- summary(lm(DTdcast, formula = randomIndex ~ Year))
    # lmRdir <- ifelse(lmR$coefficients[2] < 0, -1, 1)
    # lmRsig <- ifelse(lmR$coefficients[length(lmR$coefficients)] < 0.05, 1, 0)
    return(data.table(Species = sp,
                      ProportionAreaChosen = P,
                      umbrellaIndex = lmUdir*lmUsig))
  }))
}))

(table(throughTime$umbrellaIndex)/60)*100

