# Hotspots older code.. Not sure I will have use 
# to it anymore...

# 2. Make a contingency table for each year and treatment
whichComparison <- c(paste0("caribouBirds", 1:3),
                     paste0("coarseFilter", 1:2),
                     paste0("weightning", 1:6))
whichComparison <- paste0("caribouVSBirds", 1:6)
overwriteRas <- FALSE

compsAllYs <- lapply(yearsWanted, function(Y){
  allComps <- lapply(whichComparison, function(whichComp){
    message("Making contingency tables for year ", Y, " for comparison ",
            whichComp)
    # Pairing of rasters
    # CARIBOU vs BIRDS
    comp <- switch(whichComp,
                   "caribouBirds1" = c("V", "VI"), # # 70% with other as NA + CoarseFilter
                   "caribouBirds2" = c("VII", "VIII"), # 30% with other as NA + CoarseFilter
                   "caribouBirds3" = c("X", "XI"), # 30% with other as NA - CoarseFilter
                   # COARSE FILTER vs NO-FILTER
                   "coarseFilter1" = c("IV", "XII"), # 30% each + CoarseFilter with 30% each - CoarseFilter
                   "coarseFilter2" = c("IX", "XII"), # NA each + CoarseFilter with 30% each - CoarseFilter 
                   # WEIGHTNING
                   "weightning1" = c("I", "II"), # 7070 vs 7030
                   "weightning2" = c("I", "III"), # 7070 vs 3070
                   "weightning3" = c("I", "IV"), # 7070 vs 3030
                   "weightning4" = c("II", "III"), # 7030 vs 3070
                   "weightning5" = c("II", "IV"), # 7030 vs 7030
                   "weightning6" = c("III", "IV"), # 3070 vs 3030
                   # RETHOUGHT CARIBOU VS BIRDS --> Focus on caribou
                   "caribouVSBirds1" = c("I", "V"),
                   "caribouVSBirds2" = c("I", "II"),
                   "caribouVSBirds3" = c("II", "V"),
                   "caribouVSBirds4" = c("III", "VIII"),
                   "caribouVSBirds5" = c("III", "IV"),
                   "caribouVSBirds6" = c("IV", "VIII")
    )
    descr <- switch(whichComp,
                    "caribouBirds1" = "70% with the other group as NA with CoarseFilter",
                    "caribouBirds2" = "30% with the other group as NA with CoarseFilter",
                    "caribouBirds3" = "30% with the other group as NA without CoarseFilter",
                    # COARSE FILTER vs NO-FILTER
                    "coarseFilter1" = "30% both groups with and without coarse filter",
                    "coarseFilter2" = "NA both groups plus coarse filter with 30% both groups without coarse filter",
                    # WEIGHTNING
                    "weightning1" = "70% caribou + 70% birds vs 70% caribou + 30% birds",
                    "weightning2" = "70% caribou + 70% birds vs 30% caribou + 70% birds",
                    "weightning3" = "70% caribou + 70% birds vs 30% caribou + 30% birds",
                    "weightning4" = "70% caribou + 30% birds vs 30% caribou + 70% birds",
                    "weightning5" = "70% caribou + 30% birds vs 70% caribou + 30% birds",
                    "weightning6" = "30% caribou + 70% birds vs 30% caribou + 30% birds",
                    # RETHOUGHT CARIBOU VS BIRDS --> Focus on caribou
                    "caribouVSBirds1" = "Caribou: 70%; Birds 70% vs NA",
                    "caribouVSBirds2" = "Caribou: 70%; Birds 70% vs 30%",
                    "caribouVSBirds3" = "Caribou: 70%; Birds 30% vs NA",
                    "caribouVSBirds4" = "Caribou: 30%; Birds 70% vs NA",
                    "caribouVSBirds5" = "Caribou: 30%; Birds 70% vs 30%",
                    "caribouVSBirds6" = "Caribou: 30%; Birds 30% vs NA" 
    )
    firstRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[1],"_meanSolutions_Year",Y)]]
    secondRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[2],"_meanSolutions_Year",Y)]]
    firstRas[waterRaster[] == 1] <- NA
    secondRas[waterRaster[] == 1] <- NA
    stk <- raster::stack(firstRas, secondRas)
    DT <- na.omit(data.table::data.table(getValues(stk)))
    # 1. Spearman Correlation Test
    corrSp <- ccaPP::corSpearman(x = DT[[names(firstRas)]], 
                                 y = DT[[names(secondRas)]])
    # 2. Contingency table
    DT[, c("zz", "oz", "zo", "oo") := list(fifelse(get(names(firstRas)) == 0 & get(names(secondRas)) == 0, 1, 0),
                                           fifelse(get(names(firstRas)) == 1 & get(names(secondRas)) == 0, 1, 0),
                                           fifelse(get(names(firstRas)) == 0 & get(names(secondRas)) == 1, 1, 0),
                                           fifelse(get(names(firstRas)) == 1 & get(names(secondRas)) == 1, 1, 0))]
    # Clean all zeros but keep the % of the dataset was excluded
    totRows <- NROW(DT)
    DT[, c(names(firstRas), names(secondRas)) := NULL]
    DT[, allZ := zz+oz+zo+oo]
    DT <- DT[allZ > 0, ]
    DT[, allZ := NULL]
    totRemoved <- totRows - NROW(DT)
    contTb <- matrix(c(sum(DT[["zz"]]),
                       sum(DT[["zo"]]),
                       sum(DT[["oz"]]),
                       sum(DT[["oo"]])), nrow = 2)
    colnames(contTb) <- rownames(contTb) <- c("z", "o")
    x2test <- chisq.test(contTb)
    
    # 3. Synergy
    fileNAME <-  file.path(Paths$outputPath, paste0("comparison_", paste(comp, collapse = "_"), 
                                                    "_Year", Y))
    if (any(overwriteRas, !file.exists(fileNAME))){
      synergy <- raster::calc(stk, fun = sum, filename = fileNAME, format = "GTiff", 
                              overwrite = TRUE)
    } else {
      synergy <- raster::raster(fileNAME)
    }
    names(synergy) <- paste0("comparison_", paste(comp, collapse = "_"))
    tb <- table(synergy[])
    totalAreaHa <- 6.25*sum(!is.na(synergy[]))
    synergyAreaTotalHa <- 6.25*as.numeric(tb["2"])
    synDT <- data.table(comparison = whichComp,
                        scenarios = paste(comp, collapse = "_"),
                        description = descr,
                        totalAreaHa = totalAreaHa,
                        synergyAreaTotalHa = synergyAreaTotalHa,
                        synergyPerc = synergyAreaTotalHa/totalAreaHa,
                        spearman_rho = corrSp,
                        pX2 = x2test$p.value,
                        totalNonNApix = totRows,
                        percNon0or1 = totRemoved/totRows,
                        Year = Y
    )
    
    return(list(synergyRasters = raster::stack(stk, synergy),
                synergyTable = synDT))
  })
  names(allComps) <- whichComparison
  return(allComps)
})
names(compsAllYs) <- paste0("Year", yearsWanted)

# Extract rasters --> Make a stack, save
allRasters <- raster::stack(lapply(names(compsAllYs), function(YEAR){
  allComps <- raster::stack(lapply(names(compsAllYs[[YEAR]]), function(comp){
    synRas <- compsAllYs[[YEAR]][[comp]][["synergyRasters"]][[raster::nlayers(compsAllYs[[YEAR]]
                                                                              [[comp]]
                                                                              [["synergyRasters"]])]]
    names(synRas) <- paste0(names(synRas), "_", YEAR)
    return(synRas)
  }))
  return(allComps)
}))

# Make pretty maps
allMaps <- lapply(whichComparison, function(wc){
  comp <- switch(wc,
                 "caribouBirds1" = c("V", "VI"), # # 70% with other as NA + CoarseFilter
                 "caribouBirds2" = c("VII", "VIII"), # 30% with other as NA + CoarseFilter
                 "caribouBirds3" = c("X", "XI"), # 30% with other as NA - CoarseFilter
                 # COARSE FILTER vs NO-FILTER
                 "coarseFilter1" = c("IV", "XII"), # 30% each + CoarseFilter with 30% each - CoarseFilter
                 "coarseFilter2" = c("IX", "XII"), # NA each + CoarseFilter with 30% each - CoarseFilter 
                 # WEIGHTNING
                 "weightning1" = c("I", "II"), # 7070 vs 7030
                 "weightning2" = c("I", "III"), # 7070 vs 3070
                 "weightning3" = c("I", "IV"), # 7070 vs 3030
                 "weightning4" = c("II", "III"), # 7030 vs 3070
                 "weightning5" = c("II", "IV"), # 7030 vs 3030
                 "weightning6" = c("III", "IV"), # 3070 vs 3030
                 # RETHOUGHT CARIBOU VS BIRDS --> Focus on caribou
                 "caribouVSBirds1" = c("I", "V"),
                 "caribouVSBirds2" = c("I", "II"),
                 "caribouVSBirds3" = c("II", "V"),
                 "caribouVSBirds4" = c("III", "VIII"),
                 "caribouVSBirds5" = c("III", "IV"),
                 "caribouVSBirds6" = c("IV", "VIII")
  )
  descr <- switch(wc,
                  "caribouBirds1" = "70% with the other group as NA with CoarseFilter",
                  "caribouBirds2" = "30% with the other group as NA with CoarseFilter",
                  "caribouBirds3" = "30% with the other group as NA without CoarseFilter",
                  # COARSE FILTER vs NO-FILTER
                  "coarseFilter1" = "30% both groups with and without coarse filter",
                  "coarseFilter2" = "NA both groups plus coarse filter with 30% both groups without coarse filter",
                  # WEIGHTNING
                  "weightning1" = "70% caribou + 70% birds vs 70% caribou + 30% birds",
                  "weightning2" = "70% caribou + 70% birds vs 30% caribou + 70% birds",
                  "weightning3" = "70% caribou + 70% birds vs 30% caribou + 30% birds",
                  "weightning4" = "70% caribou + 30% birds vs 30% caribou + 70% birds",
                  "weightning5" = "70% caribou + 30% birds vs 30% caribou + 30% birds",
                  "weightning6" = "30% caribou + 70% birds vs 30% caribou + 30% birds",
                  # RETHOUGHT CARIBOU VS BIRDS --> Focus on caribou
                  "caribouVSBirds1" = "Caribou: 70%; Birds 70% vs NA",
                  "caribouVSBirds2" = "Caribou: 70%; Birds 70% vs 30%",
                  "caribouVSBirds3" = "Caribou: 70%; Birds 30% vs NA",
                  "caribouVSBirds4" = "Caribou: 30%; Birds 70% vs NA",
                  "caribouVSBirds5" = "Caribou: 30%; Birds 70% vs 30%",
                  "caribouVSBirds6" = "Caribou: 30%; Birds 30% vs NA"
  )
  pat <- paste0("comparison_", paste(comp, collapse = "_"))           
  stk <- allRasters[[grep(x = names(allRasters), pattern = pat)]]
  allPs <- lapply(names(stk), function(YEAR){
    lay <- stk[[YEAR]]/2
    Y <- usefulFuns::substrBoth(strng = YEAR, howManyCharacters = 4, fromEnd = TRUE)
    breaks <- seq(minValue(lay), maxValue(lay), length.out = 9)
    Colors <- c('#ffffd9','#edf8b1','#c7e9b4','#7fcdbb',
                '#41b6c4','#1d91c0','#225ea8','#0c2c84')
    layFilename <- file.path(Paths$outputPath, paste0(YEAR, ".png"))
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    library("colorspace")
    png(filename = layFilename,
        width = 21, height = 29,
        units = "cm", res = 300)
    p <- levelplot(lay,
                   sub = paste0("Year ", Y),
                   margin = FALSE,
                   maxpixels = 7e6,
                   at = breaks,
                   colorkey = list(
                     space = 'bottom',
                     axis.line = list(col = 'black'),
                     width = 0.75
                   ),
                   par.settings = list(
                     strip.border = list(col = 'transparent'),
                     strip.background = list(col = 'transparent'),
                     axis.line = list(col = 'transparent')),
                   scales = list(draw = FALSE),
                   col.regions = Colors,
                   par.strip.text = list(cex = 0.8,
                                         lines = 1,
                                         col = "black"))
    print(p)
    dev.off()
    return(p)
  })
  library("gridExtra")
  clearPlot()
  layFilename <- file.path(Paths$outputPath, paste0(wc, ".png"))
  png(filename = layFilename,
      width = 29, height = 21,
      units = "cm", res = 300)
  grid.arrange(allPs[[1]], allPs[[2]], 
               allPs[[3]], 
               ncol=length(allPs), top = grid::textGrob(descr, gp = gpar(fontsize = 20, 
                                                                         font = 2)))
  dev.off()
  return(layFilename)
})

# Make summary plots with the table
# Extract synergy tables --> make a big table, save
synergyTabs <- rbindlist(lapply(names(compsAllYs), function(YEAR){
  allComps <- rbindlist(lapply(names(compsAllYs[[YEAR]]), function(comp){
    return(compsAllYs[[YEAR]][[comp]][["synergyTable"]])
  }))
  return(allComps)
}))

# Make the plots:
synergyTabs[, diffB := rep(c(0.7, 0.4, 0.3), length.out = NROW(synergyTabs))]
synergyTabs[, caribouT := rep(c(0.7, 0.3), each = NROW(synergyTabs)/2)]
synergyTabs[, caribouT := as.character(caribouT)]

Require("ggplot2")
p1 <- ggplot(data = synergyTabs, mapping = aes(x = Year, y = spearman_rho, 
                                               group = scenarios,
                                               color = scenarios)) +
  geom_point() +
  stat_smooth(aes(linetype = scenarios), method = "lm", 
              formula = y ~ x) +
  scale_color_manual(values = c(I_II = "yellow", 
                                I_V = "orange",
                                II_V = "red", 
                                III_IV = "forestgreen",
                                III_VIII = "darkgreen",
                                IV_VIII = "darkblue")) +
  ylab(expression(paste("Spearman Correlation (", rho, ")")))
p1

p2 <- ggplot(data = synergyTabs, mapping = aes(x = spearman_rho, 
                                               y = diffB)) +
  geom_point(aes(group = scenarios, 
                 color = scenarios), 
             size = 2) +
  # geom_smooth(aes(group = caribouT, linetype = caribouT), 
  #             method="nls", se=FALSE, formula = y~a*log(x)+k,
  #             method.args = list(start=c(a=1, k=1))) #+
  geom_smooth(aes(group = caribouT, linetype = caribouT), 
              method="lm", se=TRUE, formula = y~x) +
  xlab(expression(paste("Spearman Correlation (", rho, ")"))) +
  ylab("Difference in landbird targets")+
  labs(group = "Scenarios", linetype = "Caribou Target")

p2

eachIndMap <- lapply(allScenarios, function(scen){
  allYears <- lapply(yearsWanted, function(Y){
    message("Making maps and tables for year ", Y, " for scenario ",
            scen)
    layFilename <- file.path(Paths$outputPath, paste0(scen, "_Year", Y, ".png"))
    # 1. Make each individual maps (not comparing = 6 * 3 point in time)
    lay <- scenPerYear[[paste0("Year",Y)]][[paste0(scen,"_meanSolutions_Year",Y)]]
    lay[waterRaster[] == 1] <- NA
    breaks <- seq(minValue(lay), maxValue(lay), length.out = 11)
    Colors <- c("#ffffd9",
                "#edf9b1",
                "#c5ebb4",
                "#aae9c9",
                "#7fcebb",
                "#42b6c4",
                "#1d91c0",
                "#225daa",
                "#253394",
                "#081d57")
    library("lattice")
    library("rasterVis")
    library("viridis")
    library("maptools")
    library("colorspace")
    png(filename = layFilename,
        width = 21, height = 29,
        units = "cm", res = 300)
    p <- levelplot(lay,
                   sub = paste0("Scenario ", scen," Year ", Y),
                   margin = FALSE,
                   maxpixels = 7e6,
                   at = breaks,
                   colorkey = list(
                     at = breaks,
                     labels = list(at = breaks,
                                   labels = breaks),
                     space = 'bottom',
                     axis.line = list(col = 'black'),
                     width = 0.75
                   ),
                   par.settings = list(
                     strip.border = list(col = 'transparent'),
                     strip.background = list(col = 'transparent'),
                     axis.line = list(col = 'transparent')),
                   scales = list(draw = FALSE),
                   col.regions = Colors,
                   par.strip.text = list(cex = 0.8,
                                         lines = 1,
                                         col = "black"))
    print(p) # <~~~~~~ RETURN TO MAKE 3 YEAR PLOTS
    dev.off()
    tbFilename <- file.path(Paths$outputPath, paste0("DT_", scen, "_Year", Y, ".qs"))
    if (!file.exists(tbFilename)){
      message(crayon::red(paste0("Caribou and birds table not found for ", scen,
                                 " for year ", Y, ", making it. This will table about 8-10m.")))
      # 2. Make the table with: scenario, year, totalAreaSelected, 
      #                         CaribouTarget, 
      #                         Bird Target, 
      #                         %RSF actually protected, 
      #                         % individual bird species protected (one column per species), 
      #                         bird diversity,
      #                         rank (summing all %'s)
      booTarget <- switch(EXPR = scen, 
                          I = 0.7,
                          II = 0.7,
                          III = 0.3,
                          IV = 0.3,
                          V = 0.7,
                          VI = 0,
                          VII = 0,
                          VIII = 0.3,
                          IX = 0,
                          X = 0.3,
                          XI = 0,
                          XII = 0.3)
      stream3Target <- switch(EXPR = scen, 
                              I = 0.7,
                              II = 0.3,
                              III = 0.7,
                              IV = 0.3,
                              V = 0,
                              VI = 0.7,
                              VII = 0.3,
                              VIII = 0,
                              IX = 0,
                              X = 0,
                              XI = 0.3,
                              XII = 0.3)
      tic("Boo and birds calculations elapsed time: ")
      # % Boo RSF
      booPerc <- rbindlist(lapply("relativeSelection", function(BOO){
        message(paste0("Extracting predictions of ", BOO, " for year ", Y))
        flName <- file.path(Paths$outputPath, paste0("mean_", BOO,
                                                     "_Year", Y, ".tif"))
        if (!file.exists(flName)){
          fls <- grepMulti(list.files(path = Paths$inputPath,
                                      recursive = TRUE, 
                                      full.names = TRUE),
                           patterns = c(climateModelType, BOO, Y, ".tif"), 
                           unwanted = "Uncertain")
          stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, 
                      na.rm = TRUE, 
                      format = "GTiff")
          # Need to bin
          stkBin <- binRSFtoDeMars2019(stk)
          writeRaster(x = stkBin, filename = flName, overwrite = TRUE)
          rm(stkBin);gc()
          stk <- raster::raster(flName)
        } else {
          stk <- raster::raster(flName)
        }
        conservSp <- stk*lay
        sumBins <- sum(conservSp[], 
                       na.rm = TRUE)
        totBins <- sum(stk[], 
                       na.rm = TRUE)
        DT <- data.table(species = "caribou",
                         percPredictedWithPP = sumBins/totBins,
                         totPredictedWithPP = sumBins,
                         Year = Y,
                         scenario = scen)
        return(DT)
      }))
      
      # % birds
      birdsPerc <- rbindlist(lapply(allb, function(BIRD){
        message(paste0("Extracting predictions of ", BIRD, " for year ", Y, 
                       ". Species ", which(allb == BIRD), " of ", length(allb)))
        flName <- file.path(Paths$outputPath, paste0("mean_", BIRD,
                                                     "_Year", Y, ".tif"))
        if (!file.exists(flName)){
          fls <- grepMulti(list.files(path = Paths$inputPath, 
                                      pattern = climateModelType,
                                      recursive = TRUE, 
                                      full.names = TRUE),
                           patterns = c("predicted", BIRD, Y, ".tif"))
          stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, na.rm = TRUE, 
                      filename = flName, 
                      format = "GTiff")
        } else {
          stk <- raster::raster(flName)
        }
        totBirds <- sum(stk[], na.rm = TRUE)
        conservSp <- stk*lay
        predBirds <- sum(conservSp[], na.rm = TRUE)
        DT <- data.table(species = BIRD,
                         percPredictedWithPP = predBirds/totBirds,
                         totPredictedWithPP = predBirds,
                         Year = Y,
                         scenario = scen)
        return(DT)
      }))
      
      bnc <- merge(booPerc, birdsPerc, all = TRUE)
      
      scenDT <- data.table(scenario = scen,
                           Year = Y,
                           totalAreaSelected = 6.25*sum(na.omit(getValues(lay))),
                           caribouTarget = booTarget,
                           stream3Target = stream3Target,
                           stream4n5Target = 0.1)
      toc()
      fullTB <- merge(scenDT, bnc, all = TRUE, by = c("Year", "scenario"))
      qs::qsave(fullTB, file = tbFilename)
    } else {
      fullTB <- qs::qread(tbFilename)
    }
    return(list(individualLay = lay,
                individualPlot = p,
                totTable = fullTB))
  })
  names(allYears) <- yearsWanted
  
  # 1. Get all individual plots for each scenario
  allPs <- lapply(allYears, `[[`, "individualPlot")
  
  library("gridExtra")
  clearPlot()
  descr <- paste0("Mean solutions for scenario ", scen)
  layFilename <- file.path(Paths$outputPath, 
                           paste0(scen, "_meanSolutions.png"))
  png(filename = layFilename,
      width = 29, height = 21,
      units = "cm", res = 300)
  grid.arrange(allPs[[1]], allPs[[2]],
               allPs[[3]],
               ncol=length(allPs), top = grid::textGrob(descr, gp = gpar(fontsize = 20,
                                                                         font = 2)))
  dev.off()
  
  # 2. Get all individual layers to make the best areas for conservation now and in the 
  # future
  allLays <- raster::stack(lapply(allYears, `[[`, "individualLay"))
  names(allLays) <- paste0(scen, "_Year", yearsWanted)
  
  # 3. Get the tables together
  allTabs <- rbindlist(lapply(allYears, `[[`, "totTable"))      
  
  return(list(rasterStack = allLays,
              predictedTable = allTabs))
})
names(eachIndMap) <- paste0("Scenario_", allScenarios)

# 1. Make the map summing all scenarios 
stk <- raster::stack(lapply(eachIndMap, `[[`, "rasterStack"))
fileName <- file.path(Paths$outputPath, 
                      paste0("bestPriorityAreas.tif"))
bestAreas <- calc(stk, fun = sum, overwrite = TRUE,
                  filename = fileName, format = "GTiff")

#1.b. Make a pretty map!
fileNamePNG <- file.path(Paths$outputPath, 
                         paste0("bestPriorityAreas.png"))
breaks <- seq(minValue(bestAreas), maxValue(bestAreas), 
              length.out = 11)
Colors <- c("#ffffd9",
            "#edf9b1",
            "#c5ebb4",
            "#aae9c9",
            "#7fcebb",
            "#42b6c4",
            "#1d91c0",
            "#225daa",
            "#253394",
            "#081d57")
library("lattice")
library("rasterVis")
library("viridis")
library("maptools")
library("colorspace")
png(filename = fileNamePNG,
    width = 21, height = 29,
    units = "cm", res = 300)
p <- levelplot(bestAreas,
               main = paste0("Areas for conservation priority \nacross time and scenarios"),
               sub = paste0("The higher the value, the higher the priority"),
               margin = FALSE,
               maxpixels = 7e6,
               at = breaks,
               colorkey = list(
                 at = breaks,
                 labels = list(at = breaks,
                               labels = breaks),
                 space = 'bottom',
                 axis.line = list(col = 'black'),
                 width = 0.75
               ),
               par.settings = list(
                 strip.border = list(col = 'transparent'),
                 strip.background = list(col = 'transparent'),
                 axis.line = list(col = 'transparent')),
               scales = list(draw = FALSE),
               col.regions = Colors,
               par.strip.text = list(cex = 0.8,
                                     lines = 1,
                                     col = "black"))
print(p)
dev.off()

# 2. Make the table summing all points in time
DT <- rbindlist(lapply(eachIndMap, `[[`, "predictedTable"))
