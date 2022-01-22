#########################################################
##       P O S T H O C         H O T S P O T S         ##
#########################################################

# Run !sourceScript.R until script 2

################################

# 1. For each year, get all runs and calculate the mean of each scenario
stepInterval <- 40 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)
allScenarios <- as.character(utils::as.roman(22:31))
internalFolder <- "ms1" # Here to set in which folder are the scenarios I am looking for (i.e. ms1)

################################

hotspotsCache <- checkPath(file.path(generalCacheFolder, "hotspots", runName), create = TRUE)
SpaDES.core::setPaths(cachePath = hotspotsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))
if (all(runLandR == FALSE)){
  if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
  newInputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                       replacement = originalDateAnalysis)
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  setPaths(inputPath = dirname(newInputPath),
           outputPath = checkPath(file.path(dirname(newOutputPath), 
                                            "hotspots", internalFolder), create = TRUE))
} else {
  setPaths(inputPath = dirname(Paths$outputPath),
           outputPath = checkPath(file.path(dirname(Paths$outputPath), 
                                            "hotspots", internalFolder)), create = TRUE)
}

allb <- usefulFuns::substrBoth(list.files("~/projects/NWT/modules/birdsNWT/data/models/",
                                          pattern = "brt8.R"),
                               howManyCharacters = 4,
                               fromEnd = FALSE)

# I should adapt the allScenarios based on what is already available...
# Only run the ones that are not yet done, and then combine the original
# that is done with the missing ones. 
# I had until 12, now I have scenarios XIII and XIV
meanSolutionsFilepath <- file.path(Paths$outputPath, 
                                   paste0("meanSolutionsAllYears", 
                                          paste(allScenarios[1], 
                                                allScenarios[length(allScenarios)], 
                                                sep = "_"),
                                          ".qs"))

if (!file.exists(meanSolutionsFilepath)){
  # Check if the file exists "retrospectively"
  maxScen <- allScenarios
  thisExists <- FALSE
while (!thisExists){
  maxScen <- maxScen[1:length(maxScen)-1]
  if (length(maxScen) == 0) break # If no scenarios exist, break
  meanSolPath <- file.path(Paths$outputPath, 
                           paste0("meanSolutionsAllYears",
                                  paste(maxScen[1],
                                        maxScen[length(maxScen)],
                                        sep = "_"),
                                  ".qs"))
  thisExists <- file.exists(meanSolPath)
}
  if (length(maxScen) == 0){
    message(crayon::red("No scenarios have posthoc analysis performed.  Performing analysis for the following 
                      scenarios:", paste(allScenarios, collapse = ", ")))
  } else {
    message(crayon::red("The last scenario found for which the posthoc has been performed was ",
                        maxScen[length(maxScen)], ". Performing analysis for the following 
                      scenarios: ", paste(setdiff(allScenarios, maxScen), collapse = ", ")))
    allScenarios <- setdiff(allScenarios, maxScen)
  }
  scenPerYear <- lapply(yearsWanted, function(Y){
    scenariosMean <- lapply(allScenarios, function(scen){
      message("Calculating mean solutions for scenario ", scen, 
              " for year ", Y)
      # List all folders that belong to the climate scenario we are looking at
      # get all files
      climMod <- strsplit(climateModel, split = "_")[[1]][1]
      allFolds <- grepMulti(list.dirs(Paths$inputPath, 
                                      full.names = TRUE,
                                      recursive = FALSE), pattern = climMod)
      allFiles <- raster::stack(lapply(allFolds, function(fd){
        fl <- file.path(fd, "hotspots", internalFolder, paste0(scen, "_solutions_Year", Y, ".tif"))
        if (!file.exists(fl)) stop(paste0("The file ", fl," doesn't exist. Are you sure you ",
                                          "set the right paths?"))
        ras <- raster::raster(fl)
        ras[] <- ras[]
        return(ras)
      }))
      meanSolutions <- raster::calc(allFiles, fun = mean, na.rm = TRUE)
      names(meanSolutions) <- paste0(scen, "_meanSolutions_Year", Y)
      return(meanSolutions)
    })
    names(scenariosMean) <- unlist(lapply(scenariosMean, names))
    if (length(maxScen) != 0){
      # Now I have to get meanSolPath file and merge it with 
      # the scenariosMean! Then return it! But that only if I had something
      # calculated previously
      allScenAvailable <- qs::qread(meanSolPath)
      # Filter for the specific year
      scenAvailableThisY <- allScenAvailable[[paste0("Year", Y)]]
      scenariosMean <- c(scenAvailableThisY, scenariosMean)
    }
    return(scenariosMean)
  })
  names(scenPerYear) <- paste0("Year", yearsWanted)
  qs::qsave(x = scenPerYear, file = meanSolutionsFilepath)
} else {
  scenPerYear <- qs::qread(meanSolutionsFilepath)
}

################
#              #
#   FIGURE 1   #
#              #
################

library("lattice")
library("rasterVis")
library("viridis")
library("maptools")
library("colorspace")
library("ggplot2")
library("googledrive")

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

allb <- usefulFuns::substrBoth(list.files("~/projects/NWT/modules/birdsNWT/data/models/",
                                          pattern = "brt8.R"),
                               howManyCharacters = 4,
                               fromEnd = FALSE)
yearsWanted <- c(2031, 2051, 2071)
whichComparison <- c("synergy30","synergy70")
# whichComparison <- c("synergy30noCF","synergy70noCF")
wantedScenarios <-  allScenarios#as.character(as.roman(1:14))

allNumbersPath <- file.path(Paths$outputPath, "allNumbersTable.qs") # Make and save the darn full table!

############## ATTENTION ###############
overwriteFig1Numbers <- FALSE
############## ATTENTION ###############
if(any(overwriteFig1Numbers,
       !file.exists(allNumbersPath))){
  Fig1_numbers <- rbindlist(lapply(X = yearsWanted, 
                                   FUN = function(Y){
                                     allScen <- rbindlist(lapply(wantedScenarios, function(scen){
                                       message("Calculating % caribou RSF and bird abundance for scenario ", 
                                               scen,
                                               " and year ",
                                               Y)
                                       allScenThisYear <- scenPerYear[[paste0("Year", Y)]]
                                       allNms <- names(allScenThisYear)
                                       scNms <- unlist(lapply(strsplit(allNms, split = "_"), function(S) return(S[1])))
                                       whichToKeep <- which(scNms %in% scen)
                                       solRas <- allScenThisYear[[whichToKeep]]
                                       # 1. Cleanup water
                                       solRas[waterRaster[] == 1] <- NA
                                       # 2. Get only areas that are selected more than 80% of the time
                                       solRas[solRas >= 0.8] <- 1
                                       solRas[solRas < 0.8] <- 0
                                       # 3. Calculate total area of that
                                       totAreaHa <- 6.25*sum(solRas[], na.rm = TRUE)
                                       # 4. Get all birds %
                                       tic("Boo and birds calculations elapsed time: ")
                                       birdsPerc <- rbindlist(lapply(allb, function(BIRD){
                                         
                                         # NEED TO EXTRACT THE TOTAL BIRDS AT THE MOMENT -- 2011 AS A GUIDELINE
                                         message(paste0("Extracting predictions of ", BIRD, " for year 2011 (to calculate total birds today). 
                     Species ", which(allb == BIRD), " of ", length(allb)))
                                         flName <- file.path(Paths$outputPath, paste0("mean_", BIRD,
                                                                                      "_Year2011.tif"))
                                         if (!file.exists(flName)){
                                           message(crayon::yellow(flName, " doesn't exist. Creating..."))
                                           fls <- grepMulti(list.files(path = Paths$inputPath, 
                                                                       pattern = climateModelType,
                                                                       recursive = TRUE, 
                                                                       full.names = TRUE),
                                                            patterns = c("predicted", BIRD, 2011, ".tif"))
                                           stkOriginal <- calc(raster::stack(lapply(fls, raster)), fun = mean, na.rm = TRUE, 
                                                               filename = flName, 
                                                               format = "GTiff")
                                         } else {
                                           message(crayon::green(flName, " exists. Loading..."))
                                           stkOriginal <- raster::raster(flName)
                                         }
                                         # NEED TO EXTRACT THE TOTAL BIRDS AT EACH YEAR IN TIME -- TO CALCULATE HOW MUCH WE ARE PROTECTING
                                         message(paste0("Extracting predictions of ", BIRD, " for year ", Y, 
                                                        " (to calculate birds kept in year ", Y, "). 
                     Species ", which(allb == BIRD), " of ", length(allb)))
                                         flName <- file.path(Paths$outputPath, paste0("mean_", BIRD,
                                                                                      "_Year", Y,".tif"))
                                         if (!file.exists(flName)){
                                           message(crayon::yellow(flName, " doesn't exist. Creating..."))
                                           fls <- grepMulti(list.files(path = Paths$inputPath, 
                                                                       pattern = climateModelType,
                                                                       recursive = TRUE, 
                                                                       full.names = TRUE),
                                                            patterns = c("predicted", BIRD, 2011, ".tif"))
                                           stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, na.rm = TRUE, 
                                                       filename = flName, 
                                                       format = "GTiff")
                                         } else {
                                           message(crayon::green(flName, " exists. Loading..."))
                                           stk <- raster::raster(flName)
                                         }
                                         totBirds <- sum(stkOriginal[], na.rm = TRUE)
                                         conservSp <- stk*solRas
                                         predBirds <- sum(conservSp[], na.rm = TRUE)
                                         DT <- data.table(species = BIRD,
                                                          totPredictedWithPP = predBirds,
                                                          totIndvs = totBirds,
                                                          percPredictedWithPP = predBirds/totBirds,
                                                          Year = Y,
                                                          scenario = scen)
                                         return(DT)
                                       }))
                                       # 5. Get caribou %
                                       booPerc <- rbindlist(lapply("relativeSelection", function(BOO){
                                         message(paste0("Extracting predictions of ", BOO, " for year 2011 (to calculate total RSF today)"))
                                         flName <- file.path(Paths$outputPath, paste0("mean_", BOO,
                                                                                      "_Year2011.tif"))
                                         if (!file.exists(flName)){
                                           fls <- grepMulti(list.files(path = Paths$inputPath,
                                                                       recursive = TRUE, 
                                                                       full.names = TRUE),
                                                            patterns = c(climateModelType, BOO, 2011, ".tif"), 
                                                            unwanted = "Uncertain")
                                           stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, 
                                                       na.rm = TRUE, 
                                                       format = "GTiff")
                                           # Need to bin
                                           stkBin <- binRSFtoDeMars2019(stk)
                                           writeRaster(x = stkBin, filename = flName, overwrite = TRUE)
                                           rm(stkBin);gc()
                                           stkOriginal <- raster::raster(flName)
                                         } else {
                                           stkOriginal <- raster::raster(flName)
                                         }
                                         
                                         message(paste0("Extracting predictions of ", BOO, " for year ", Y,
                                                        " (to calculate RSF kept in year ", Y, ")."))
                                         flName <- file.path(Paths$outputPath, paste0("mean_", BOO,
                                                                                      "_Year", Y, ".tif"))
                                         if (!file.exists(flName)){
                                           fls <- grepMulti(list.files(path = Paths$inputPath,
                                                                       recursive = TRUE, 
                                                                       full.names = TRUE),
                                                            patterns = c(climateModelType, BOO, 2011, ".tif"), 
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
                                         conservSp <- stk*solRas
                                         sumBins <- sum(conservSp[], 
                                                        na.rm = TRUE)
                                         totBins <- sum(stkOriginal[], 
                                                        na.rm = TRUE)
                                         DT <- data.table(species = "caribou",
                                                          totIndvs = totBins,
                                                          totPredictedWithPP = sumBins,
                                                          percPredictedWithPP = sumBins/totBins,
                                                          Year = Y,
                                                          scenario = scen)
                                         return(DT)
                                       }))
                                       toc()
                                       bnc <- merge(booPerc, birdsPerc, all = TRUE)
                                       bnc[, totalArea := totAreaHa]
                                       return(bnc)
                                     }))
                                     return(allScen)
                                   }))  
  
  # Calculate the amount kept of total landbirds (caribou is calculated already)
  message("Calculating bird and caribou protection for scenarios ", 
          paste(wantedScenarios, collapse = ", "))
  
  DTb <- Fig1_numbers[species != "caribou", ]
  DTb[, totIndvs := sum(totIndvs), by = c("scenario", "Year")]
  DTb[, totPredictedWithPP := sum(totPredictedWithPP), by = c("scenario", "Year")]
  DTb[, percPredictedWithPP := totPredictedWithPP/totIndvs]
  DTb[, species := NULL]
  DTb[, species := "Landbirds"]
  DTb <- unique(DTb)
  Fig1_numbers <- rbind(Fig1_numbers, DTb, use.names = TRUE)
  qs::qsave(Fig1_numbers, file = allNumbersPath)
} else {
  Fig1_numbers <- qs::qread(allNumbersPath)
}
uploadMaps <- FALSE
createMaps <- FALSE
overlapTablePath <- file.path(Paths$outputPath, paste0("overlapTable_", 
                              paste(whichComparison, collapse = "_"),
                              ".qs"))
if (!file.exists(overlapTablePath)){
  Fig1_maps <- lapply(yearsWanted, function(Y){
    allComps <- lapply(whichComparison, function(whichComp){
      message("Making synergy maps for year ", Y, 
              " for comparison ", whichComp)
      # Pairing of rasters
      # CARIBOU vs BIRDS
      # ALWAYS "BIRD FRIENDLY" SCENARIO FIRST!!! --> Important for plotting correctly
      comp <- switch(whichComp,
                     "synergy70" = c("VI", "V"),
                     "synergy30" = c("VII", "VIII")#,
                     # "synergy70noCF" = c("XIV", "XIII"),
                     # "synergy30noCF" = c("XI", "X")
      )
      
      firstRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[1],"_meanSolutions_Year",Y)]]
      secondRas <- scenPerYear[[paste0("Year",Y)]][[paste0(comp[2],"_meanSolutions_Year",Y)]]
      # Remove water
      firstRas[waterRaster[] == 1] <- NA
      secondRas[waterRaster[] == 1] <- NA
      
      # 2. Get only areas that are selected more than 80% of the time
      firstRas[firstRas[] >= 0.8] <- 1
      firstRas[firstRas[] < 0.8] <- 0
      secondRas[secondRas[] >= 0.8] <- 1
      secondRas[secondRas[] < 0.8] <- 0
      
      # 3. Calculate total area of that
      message("Calculating total area for year ", Y, " scenarios ", 
              paste(comp, collapse = " vs "))
      areaSelectedHaFirstRas <- 6.25*sum(firstRas[], na.rm = TRUE)
      areaSelectedHaSecondRas <- 6.25*sum(secondRas[], na.rm = TRUE)
      totalAreaFirst <- 6.25*sum(!is.na(firstRas[]))
      totalAreaSecond <- 6.25*sum(!is.na(secondRas[]))
      
      # test that the NA's are the same
      testthat::expect_true(totalAreaFirst == totalAreaSecond)
      
      stk <- raster::stack(firstRas, secondRas)
      DT <- na.omit(data.table::data.table(getValues(stk)))
      # 2. Spearman Correlation Test
      message("Calculating spearman correlation for year ", Y, " scenarios ", 
              paste(comp, collapse = " vs "))
      
      corrSp <- ccaPP::corSpearman(x = DT[[names(firstRas)]], 
                                   y = DT[[names(secondRas)]])
      # 3. Contingency table
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
      
      # 3. Synergy -- perc overlapping
      message("Calculating percentage overlap for year ", Y, " scenarios ",
              paste(comp, collapse = " vs "))
      
      percOverFirst <- sum(firstRas[firstRas[] == 1 & secondRas[] == 1])/sum(firstRas[firstRas[] == 1])
      percOverSecond <- sum(secondRas[firstRas[] == 1 & secondRas[] == 1])/sum(secondRas[secondRas[] == 1])
      maxOverlap <- max(percOverFirst, percOverSecond)
      
      # 4. Contiguity
      message("Calculating contiguity for year ", Y, " scenarios ", 
              paste(comp, collapse = " vs "))
      
      firstClumped <- raster::clump(firstRas, directions = 8, gaps = TRUE)
      secondClumped <- raster::clump(secondRas, directions = 8, gaps = TRUE)
      clumpStk <- raster::stack(firstClumped, secondClumped)
      names(clumpStk) <- comp
      
      DTclump <- na.omit(data.table(getValues(clumpStk)))
      
      ppDTclump <- rbindlist(lapply(names(DTclump), function(nm){
        currDT <- DTclump[, ..nm]
        nPix <- currDT[, .N, by = nm]
        TB <- table(nPix[["N"]])
        DT <- data.table(scenario = nm,
                         numberAreas = as.numeric(TB),
                         sizeAreasHa = 6.25*as.numeric(names(TB)))
        return(DT)
      }))
      ppDTclump[, "sizeAreasKm2" := sizeAreasHa*0.01]
      
      # 5. Make the pretty Map      
      secondRas[secondRas[] == 1] <- 2
      finalRas <- firstRas + secondRas
      finalRas <- ratify(finalRas)
      
      rat <- levels(finalRas)[[1]]
      rat$species <- c("NotSelected", "landbirds", "caribou", "both")
      levels(finalRas) <- rat
      ColorsTB <- data.table(levs = c("NotSelected", "landbirds", "caribou", "both"),
                             Colors = c("grey85", "darkmagenta", "forestgreen", "blue4"))
      ColorsTB <- ColorsTB[match(rat[["species"]], levs),]
      message("Making Figure 1 map for year ", Y, " scenarios ", 
              paste(comp, collapse = " vs "))
      
      fileNamePNG <- file.path(Paths$outputPath, 
                               paste0(paste(comp, collapse = "_"),"_priorityAreas_Year", 
                                      Y, ".png"))
      png(filename = fileNamePNG,
          width = 21, height = 29,
          units = "cm", res = 300)
      p <- levelplot(finalRas,
                     main = paste0("Spearman Correlation: ", round(corrSp, 2)),
                     sub = paste0("Intersection: ",  round(maxOverlap, 2),
                                  "\nCaribou: ", round(as.numeric(Fig1_numbers[scenario == comp[1] & 
                                                                                 species == "caribou" &
                                                                                 Year == Y, 
                                                                               "percPredictedWithPP"]), 2), 
                                  "\nLandbirds: ", round(as.numeric(Fig1_numbers[scenario == comp[2] &
                                                                                   species == "Landbirds" &
                                                                                   Year == Y, 
                                                                                 "percPredictedWithPP"]), 2)),
                     margin = FALSE,
                     maxpixels = 7e6,
                     colorkey = FALSE,
                     par.settings = list(
                       strip.border = list(col = 'transparent'),
                       strip.background = list(col = 'transparent'),
                       axis.line = list(col = 'transparent')),
                     scales = list(draw = FALSE),
                     col.regions = ColorsTB[["Colors"]],
                     par.strip.text = list(cex = 0.8,
                                           lines = 1,
                                           col = "black"))
      print(p)
      dev.off()
      return(list(overlapTable = data.table(comparison = paste(comp, collapse = "_"),
                                            sizeAreas = ppDTclump,
                                            Year = Y,
                                            rho = corrSp,
                                            x2 = x2test$p.value,
                                            maxOverlap = maxOverlap),
                  prettyPlots = p))
    })
    names(allComps) <- whichComparison
    return(allComps)
  })
  names(Fig1_maps) <- paste0("Year", yearsWanted)
  fl <- grepMulti(list.files(Paths$outputPath,
                             pattern = "png",
                             full.names = TRUE), 
                  patterns = c("priorityAreas_", "VI_V|VII_VIII"))
  if (uploadMaps)
    lapply(fl, drive_upload, path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))
  overlapTable30 <- lapply(Fig1_maps, `[[`, "synergy30")
  overlapTable70 <- lapply(Fig1_maps, `[[`, "synergy70")
  overlapTable3070 <- c(overlapTable30, overlapTable70)
  overlapTable <- rbindlist(lapply(overlapTable3070, `[[`, "overlapTable"))
  names(overlapTable) <- c("comparison", "scenario", "numberAreas", "sizeAreasHa", "sizeAreasKm2", 
                           "Year", "rho", "x2", "maxOverlap")
  qs::qsave(overlapTable, overlapTablePath)
} else {
  overlapTable <- qs::qread(overlapTablePath)
}

# Table 1: species, focus, target
Require("data.table")
DT1 <- Fig1_numbers[species %in% c("Landbirds", "caribou") & 
                      scenario %in% c("VII", "VIII", "VI", "V")]
DT1[, CaribouTarget := fifelse(scenario %in% c("VI", "VII"),
                               NaN, 
                               fifelse(scenario %in% "V",
                                       70, 
                                       30))]
DT1[, LandbirdsTarget := fifelse(scenario %in% c("V", "VIII"),
                                 NaN, 
                                 fifelse(scenario %in% "VI",
                                         70, 
                                         30))]
### TRYING SOMETHING NEW --> Using area to devide the values of % 
DT1[, percPredictedWithPP := (percPredictedWithPP*100)/(totalArea/10^4)]

###### END OF SOMETHING NEW

mDT1 <- dcast(data = DT1, formula = Year + CaribouTarget + LandbirdsTarget ~ species, value.var = "percPredictedWithPP")

# Add focus species
mDT1[, focusSpecies := fifelse(is.na(CaribouTarget), "landbirds", "caribou")]
# Add target column
mDT1[, target := fifelse(is.na(CaribouTarget), LandbirdsTarget, CaribouTarget)]
# Remove Caribou and LandbirdsTarget
mDT1[, c("LandbirdsTarget", "CaribouTarget") := NULL]
# Melt Landbirds and caribou into one: variable = species, value = perc
mDT1 <- melt(mDT1, id.vars = c("Year", "focusSpecies", "target"),
             measure.vars = c("Landbirds", "caribou"))
# Just rename the vars
names(mDT1)[names(mDT1) == "value"] <- "perc"
names(mDT1)[names(mDT1) == "variable"] <- "species"

# Table 2: rho and intersection
DT2 <- unique(overlapTable[, c("scenario", "Year", "rho", "maxOverlap")])
# Devise from scenario: Target and focusSpecies
DT2[, target := fifelse(scenario %in% c("VIII", "VII"), 
                        30, 70)]
DT2[, focusSpecies := fifelse(scenario %in% c("VIII", "V"), 
                        "caribou", 
                        "landbirds")]
# Make rho and maxOverlap become one variable named metric, and value names perc
mDT2 <- melt(DT2, id.vars = c("Year", "scenario", "target", "focusSpecies"),
             measure.vars = c("rho", "maxOverlap"))
# Just rename the vars
names(mDT2)[names(mDT2) == "value"] <- "percMetric"
names(mDT2)[names(mDT2) == "variable"] <- "metric"

DT <- merge(mDT1, mDT2, by = c("Year", "target", "focusSpecies"))
# Fix capital Landbirds from species
DT[species == "Landbirds", species := "landbirds"]
# Need to make one color code for species + focused Species
DT[, sp := paste0(species, " when targeting ", focusSpecies)]
DT[, sp := factor(sp, levels = c("landbirds when targeting landbirds",
                                 "landbirds when targeting caribou",
                                 "caribou when targeting caribou",
                                 "caribou when targeting landbirds"))]

Table2 <- unique(mDT2[, c("Year", "target", "metric", "percMetric")])
Table2 <- dcast(data = Table2, formula = Year + target ~ metric, value.var = "percMetric")
write.csv(Table2, file.path(Paths$outputPath, "Table2.csv"))
drive_upload(file.path(Paths$outputPath, "Table2.csv"), path = as_id("1qOOQrdZYEVbY98xx5x2mP6wDYQ6cpAe1"))
# x = Year
# y = percentage
# groups = intersection, rho, caribou, birds
# 'scenarios' --> 
#     birds c("VI", "VII")
#     caribou c("V", "VIII")

targetLab <- c("Target = 30%", "Target = 70%")
names(targetLab) <- c(30, 70)

library("ggplot2")
Fig1_plot <- ggplot(data = DT[metric != "rho",], aes(x = Year)) +
  geom_col(aes(y = perc, 
               fill = stringr::str_wrap(sp, 20)), position = position_dodge()) + 
  scale_fill_manual(name = "Species protected",
                     values = c("forestgreen",
                                "chartreuse3",
                                "darkmagenta",
                                "orchid"
                                )) +
  # geom_point(aes(y = percMetric, 
  #                shape = metric)) + 
  # geom_line(aes(y = percMetric, 
  #                linetype = metric)) +
  scale_linetype(guide = 'none') +
  scale_shape_manual(name = "Metric",
                     values = c(8, 2),
                     labels = c(expression(Spearman~rho), "Area overlap")) +
  facet_grid(target ~ ., 
             labeller = labeller(target = targetLab)) +
  ylab(expression(paste("% per ", Km^2))) +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        legend.box = "vertical", 
        legend.margin = margin()) +
  scale_x_continuous(breaks = sort(unique(DT$Year)))

Fig1_plot

fileNamePNG <- file.path(Paths$outputPath, 
                         paste0("Figure1_Plots.png"))
png(filename = fileNamePNG,
    width = 20, height = 14,
    units = "cm", res = 300)
Fig1_plot
dev.off()
drive_upload(media = fileNamePNG, 
             path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))

# Checking total area size:
scenarios <- as.character(as.roman(5:8))
Fig1_totArea <- rbindlist(lapply(yearsWanted, function(Y){
  allScen <- rbindlist(lapply(scenarios, function(scen){
    message("Getting total area for year ", Y, 
            " for scenario ", scen)
    r <- scenPerYear[[paste0("Year",Y)]][[paste0(scen,"_meanSolutions_Year",Y)]]
    # Remove water
    r[waterRaster[] == 1] <- NA
    
    # 2. Get only areas that are selected more than 80% of the time
    r[r[] >= 0.8] <- 1
    r[r[] < 0.8] <- 0

    # 3. Calculate total area of that
    message("Calculating total area for year ", Y, " scenario ", 
            scen)
    areaSelectedKm2 <- 0.01*6.25*sum(r[], na.rm = TRUE)
    DT <- data.table(scenario = scen,
                     areaSelectedKm2 = areaSelectedKm2,
                     Year = Y)
    return(DT)
  }))
  return(allScen)
}))

tb <- data.table(description = c("when targeting landbirds",
                                 "when targeting caribou",
                                 "when targeting landbirds",
                                 "when targeting caribou"),
                 targets = c("30", 
                             "30", 
                             "70", 
                             "70"),
                 scenario = c("VII",
                              "VIII",
                              "VI",
                              "V"))
totalAreaTable <- merge(Fig1_totArea, tb, by = "scenario")
setkey(totalAreaTable, "Year", "targets")

# NOTE: I didn't figure out a good way of dealing with 
# R to put the real figure together with arrange.grid().
# Ended up doing in PowerPoint: 
# https://drive.google.com/file/d/1xxzymsOovglSWhACWy_RS2PT-I-ymtI-/view?usp=sharing

################
#              #
#   FIGURE 2   #
#              #
################

# I will do it based on 3 groups: umbrella birds, umbrella caribou, both
# From These 3 maps, I will extract the below,
# but for 10, 20, 30, 40, 50, 60, 70, 80, 90 and 100% of the selected areas of
# each group
# These 3 groups are my new "shape"
flatScenPerYear <- unlist(scenPerYear)
umbrellaBirds <- raster::subset(flatScenPerYear, subset = grepl(x = names(flatScenPerYear), 
                                               pattern = "1.VI_|1.VII_"))
umbrellaCaribou <- raster::subset(flatScenPerYear, subset = grepl(x = names(flatScenPerYear), 
                                               pattern = "1.V_|1.VIII_"))
optimized <- raster::subset(flatScenPerYear, subset = grepl(x = names(flatScenPerYear), 
                                               pattern = "1.I_|1.IV_"))

# HERE NEED TO ADD THE CALCULATION OF BEST PRIORITY AREAS

############## ATTENTION ###############
overwriteBestPPMap <- FALSE
############## ATTENTION ###############

umbrellaBirdsPath <- file.path(Paths$outputPath, "umbrellaLandbirdsPA.tif")
umbrellaBooPath <- file.path(Paths$outputPath, "umbrellaCaribouPA.tif")
optimizedPath <- file.path(Paths$outputPath, "optimizedPA.tif")

if (!exists("waterRaster"))
  

if (any(overwriteBestPPMap, 
        !file.exists(umbrellaBirdsPath),
        !file.exists(umbrellaBooPath),
        !file.exists(optimizedPath))){
  
  Landbirds <- calc(stack(umbrellaBirds), fun = sum, 
                           na.rm = TRUE, overwrite = TRUE,
                           filename = umbrellaBirdsPath)
  Caribou <- calc(stack(umbrellaCaribou), fun = sum, 
                           na.rm = TRUE, overwrite = TRUE,
                           filename = umbrellaBooPath)
  Optmized <- calc(stack(optimized), fun = sum, 
                           na.rm = TRUE, overwrite = TRUE,
                           filename = optimizedPath)
  # 1. Cleanup water
  Landbirds[waterRaster[] == 1] <- NA
  Caribou[waterRaster[] == 1] <- NA
  Optmized[waterRaster[] == 1] <- NA
} else {
  # Get the full maps
  Landbirds <- raster::raster(umbrellaBirdsPath)
  Caribou <- raster::raster(umbrellaBooPath)
  Optmized <- raster::raster(optimizedPath)
  # 1. Cleanup water and outside of the SA
  Landbirds[waterRaster[] == 1] <- NA
  Landbirds[is.na(rasterToMatch[])] <- NA
  Caribou[waterRaster[] == 1] <- NA
  Caribou[is.na(rasterToMatch[])] <- NA
  Optmized[waterRaster[] == 1] <- NA
  Optmized[is.na(rasterToMatch[])] <- NA
}

Landbirds <- Landbirds/length(umbrellaBirds)
Caribou <- Caribou/length(umbrellaCaribou)
Optmized <- Optmized/length(optimized)

wantedMaps <- c("Landbirds", "Caribou", "Optmized")
wantedLevels <- seq(0.1, 0.9, by = 0.2) 
# Here we go with the percentage (as area!) 
# we want to see the values for!
tic("Calculate Numbers for Figure 2 elapsed time: ") # 6hs
Fig2_numbers <- rbindlist(lapply(wantedMaps, function(scen){
  allYs <- rbindlist(lapply(X = yearsWanted,
                              FUN = function(Y){
    allLevels <- rbindlist(lapply(wantedLevels, function(lvl){
    message("Calculating % caribou RSF and bird abundance for ", 
            scen, ", for level ", lvl," and year ",
            Y)
    solRas <- get(scen)
    topX <- maxValue(solRas)-(maxValue(solRas)*lvl)
    solRas[solRas[] < topX] <- 0
    solRas[solRas[] >= topX] <- 1
    totAreaHa <- 6.25*sum(solRas[], na.rm = TRUE)
    # 4. Get all birds %
    tic("Boo and birds calculations elapsed time: ")
    birdsPerc <- rbindlist(lapply(allb, function(BIRD){
      
      # NEED TO EXTRACT THE TOTAL BIRDS AT THE MOMENT -- 2011 AS A GUIDELINE
      message(paste0("Extracting predictions of ", BIRD, " for year 2011 (to calculate total birds today). 
                     Species ", which(allb == BIRD), " of ", length(allb)))
      flName <- file.path(Paths$outputPath, paste0("mean_", BIRD,
                                                   "_Year2011.tif"))
      if (!file.exists(flName)){
        message(crayon::yellow(flName, " doesn't exist. Creating..."))
        fls <- grepMulti(list.files(path = Paths$inputPath, 
                                    pattern = climateModelType,
                                    recursive = TRUE, 
                                    full.names = TRUE),
                         patterns = c("predicted", BIRD, 2011, ".tif"))
        stkOriginal <- calc(raster::stack(lapply(fls, raster)), fun = mean, na.rm = TRUE, 
                    filename = flName, 
                    format = "GTiff")
      } else {
        message(crayon::green(flName, " exists. Loading..."))
        stkOriginal <- raster::raster(flName)
      }
      # NEED TO EXTRACT THE TOTAL BIRDS AT EACH YEAR IN TIME -- TO CALCULATE HOW MUCH WE ARE PROTECTING
      message(paste0("Extracting predictions of ", BIRD, " for year ", Y, 
                     " (to calculate birds kept in year ", Y, "). 
                     Species ", which(allb == BIRD), " of ", length(allb)))
      flName <- file.path(Paths$outputPath, paste0("mean_", BIRD,
                                                   "_Year", Y,".tif"))
      if (!file.exists(flName)){
        message(crayon::yellow(flName, " doesn't exist. Creating..."))
        fls <- grepMulti(list.files(path = Paths$inputPath, 
                                    pattern = climateModelType,
                                    recursive = TRUE, 
                                    full.names = TRUE),
                         patterns = c("predicted", BIRD, 2011, ".tif"))
        stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, na.rm = TRUE, 
                    filename = flName, 
                    format = "GTiff")
      } else {
        message(crayon::green(flName, " exists. Loading..."))
        stk <- raster::raster(flName)
      }
      totBirds <- sum(stkOriginal[], na.rm = TRUE)
      conservSp <- stk*solRas
      predBirds <- sum(conservSp[], na.rm = TRUE)
      DT <- data.table(species = BIRD,
                       totPredictedWithPP = predBirds,
                       totIndvs = totBirds,
                       percPredictedWithPP = predBirds/totBirds,
                       Year = Y,
                       scenario = scen)
      return(DT)
    }))
    # 5. Get caribou %
    booPerc <- rbindlist(lapply("relativeSelection", function(BOO){
      message(paste0("Extracting predictions of ", BOO, " for year 2011 (to calculate total RSF today)"))
      flName <- file.path(Paths$outputPath, paste0("mean_", BOO,
                                                   "_Year2011.tif"))
      if (!file.exists(flName)){
        fls <- grepMulti(list.files(path = Paths$inputPath,
                                    recursive = TRUE, 
                                    full.names = TRUE),
                         patterns = c(climateModelType, BOO, 2011, ".tif"), 
                         unwanted = "Uncertain")
        stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, 
                    na.rm = TRUE, 
                    format = "GTiff")
        # Need to bin
        stkBin <- binRSFtoDeMars2019(stk)
        writeRaster(x = stkBin, filename = flName, overwrite = TRUE)
        rm(stkBin);gc()
        stkOriginal <- raster::raster(flName)
      } else {
        stkOriginal <- raster::raster(flName)
      }
      
      message(paste0("Extracting predictions of ", BOO, " for year ", Y,
                     " (to calculate RSF kept in year ", Y, ")."))
      flName <- file.path(Paths$outputPath, paste0("mean_", BOO,
                                                   "_Year", Y, ".tif"))
      if (!file.exists(flName)){
        fls <- grepMulti(list.files(path = Paths$inputPath,
                                    recursive = TRUE, 
                                    full.names = TRUE),
                         patterns = c(climateModelType, BOO, 2011, ".tif"), 
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
      conservSp <- stk*solRas
      sumBins <- sum(conservSp[], 
                     na.rm = TRUE)
      totBins <- sum(stkOriginal[], 
                     na.rm = TRUE)
      DT <- data.table(species = "caribou",
                       totIndvs = totBins,
                       totPredictedWithPP = sumBins,
                       percPredictedWithPP = sumBins/totBins,
                       Year = Y,
                       scenario = scen)
      return(DT)
    }))
    toc()
    bnc <- merge(booPerc, birdsPerc, all = TRUE)
    DTb <- bnc[species != "caribou", ]
    DTb[, totIndvs := sum(totIndvs), by = c("scenario", "Year")]
    DTb[, totPredictedWithPP := sum(totPredictedWithPP), by = c("scenario", "Year")]
    DTb[, percPredictedWithPP := totPredictedWithPP/totIndvs]
    DTb[, species := NULL]
    DTb[, species := "landbirds"]
    DTb <- unique(DTb)
    bnc <- rbind(bnc, DTb, use.names = TRUE)
    bnc[, totalAreaHa := totAreaHa]
    bnc[, totalAreaKm2 := totAreaHa*0.01]
    return(bnc)
  }))
  return(allLevels)
  }))
  return(allYs)
}))
toc() 

# MAPS
allMaps <- lapply(wantedMaps, function(scen){
  solRas <- get(scen)
  fileNamePNG <- file.path(Paths$outputPath, 
                           paste0("Figure2_", scen,".png"))
  
  breaks <- seq(minValue(solRas), maxValue(solRas), 
                length.out = 11)
  
  subTxt <- if (scen %in% c("Caribou", "Landbirds"))
    paste0(scen, " as umbrella species") else 
      paste0(scen, " for both caribou and landbirds")

  png(filename = fileNamePNG,
      width = 21, height = 29,
      units = "cm", res = 300)
  p <- levelplot(solRas,
                 main = paste0("Areas for conservation priority \nacross time and targets"),
                 sub = subTxt, 
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
  
  drive_upload(media = fileNamePNG, path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))
  
  return(fileNamePNG)
})

###############
# I believe everything below is outdated
# # Calculate bird and caribou kept for each year by each scenario
# DT <- Fig2_numbers[species != "caribou", ]
# DT[, totTot := sum(totIndvs), by = c("Year", "scenario")]
# DT[, totPred := sum(totPredictedWithPP), by = c("Year", "scenario")]
# DT[, percKept := totPred/totTot]
# DTred <- unique(DT[, c("scenario", "percKept", "Year")])
# DTred[, species := "Landbirds"] 
# # Now caribou
# DT2 <- Fig2_numbers[species == "caribou", ] 
# DT2[, "percKept" := totPredictedWithPP/totIndvs]
# DT2[, names(DT2)[!names(DT2) %in% c("species", "percKept", "scenario", "Year")] := NULL]
# DT <- rbind(DTred, DT2, use.names = TRUE)
# # Fix percKept to real percKept (I was doing percKept!)
# DT[, percKept := round(1-percKept, 2)]
# 
# areaDT <- unique(Fig2_numbers[, c("scenario", "totalArea")])
# DT <- merge(DT, areaDT)
# 
# scenDT <- data.table(scenario = c(paste0("best", 1:3)),
#                      newScen = c("top 60%", "top 40%", "top 20%"))
# 
# DT <- merge(DT, scenDT)
# DT[, scenario := NULL]
# names(DT)[names(DT) == "newScen"] <- "scenario"
# DT[, scenario := as.factor(scenario)]
# DT[, totalArea := round(totalArea/1000000, digits = 2)]
# DT[, totalAreaKm2 := totalArea*0.01*1000000]
###############

DT_2 <- Fig2_numbers[species %in% c("caribou", "landbirds"), ]

# Currently, with ALL conservation areas planned executed, we have 
# - 29,800 km in Established Protected Areas + 
# - 12,800 in Candidate Protected Areas + 
# - 21,845 in Conservation Zones + 
# - 59,404 in Interin and Land withdraws. 
# This makes 123,800 km2

# Divide the percent into bins and use 
DT_2[, percPredictedWithPP := round(percPredictedWithPP, 2)]
DT_2[, bins := cut_width(DT_2$percPredictedWithPP, width = 0.1)]
v <- data.table(bins = sort(unique(DT_2[["bins"]])),
                newPerc = seq(0.15, 0.95, by = 0.1))
DT_2 <- merge(DT_2, v)
DT_2[, newPerc := as.character(newPerc)]

# pal <- RColorBrewer::brewer.pal(name = "YlGnBu", n = 9)
pal <- viridis::magma(9, direction = -1)
checkColorPalette(pal)

sort(round(unique(DT_2$totalAreaKm2), 0))
# Manually derived break
totKmBreaks <- sort(c(seq(57000, 357000, by = 50000), 123000))
DT_2[, totalAreaKm2_fac := as.factor(totalAreaKm2)]
DT_2[, colorScheme := fifelse(species == "caribou" & totalAreaKm2 < 123000, paste0("caribou below 123,000 Km^2"),
                              fifelse(species == "caribou" & totalAreaKm2 >= 123000, paste0("caribou above 123,000 Km^2"),
                                      fifelse(species == "landbirds" & totalAreaKm2 < 123000, paste0("landbirds below 123,000 Km^2"),
                                              paste0("landbirds above 123,000 Km^2"))))]
p3 <- ggplot(data = DT_2, mapping = aes(x = Year,
                                        y = percPredictedWithPP,
                                        color = colorScheme,
                                        size = totalAreaKm2,
                                        shape = scenario)) +
  geom_jitter(width = 1) +
  ylab("Percentage protected with selected areas") +
  scale_x_continuous(breaks = yearsWanted) +
  scale_color_manual(name = "Species",
                       values = c("forestgreen",
                                  "chartreuse3",
                                  "darkmagenta",
                                  "orchid")) +
  scale_size_continuous(name = expression(Total~Area~(Km^2)),
                        breaks = totKmBreaks) +
  scale_shape_manual(name = "Focused protection",
                     values = c(15, 17, 19)) +
  theme(legend.key = element_blank(),
        legend.background = element_blank()) +
  guides(shape = guide_legend(override.aes = list(size = 4)),
         color = guide_legend(override.aes = list(size = 4)))

fileNamePNG <- file.path(Paths$outputPath,
                         paste0("Figure2_plot.png"))
png(filename = fileNamePNG,
    width = 29, height = 21,
    units = "cm", res = 300)
p3
dev.off()
drive_upload(media = fileNamePNG, 
             path = as_id("1yzfYnVqAANyfPRQeTu-l8YUl2hW6FbBk"))

 
# p2 <- ggplot(data = DT_2, mapping = aes(x = Year,
#                                       y = totalAreaKm2/1000,
#                                       color = newPerc,
#                                       shape = species)) +
#   facet_grid(scenario ~ .) +
#   geom_jitter(size = 5, width = 1) +
#   ylab(expression("Total area selected"*" (x 10"^3*" Km"^2*")")) +
#   scale_x_continuous(breaks = yearsWanted) +
#   scale_color_manual(name = "Percent Protected",
#                      values = pal) +
#   scale_shape_manual(name = "Species",
#                      values = c(15, 17)) +
#   theme(legend.key = element_blank(),
#         legend.background = element_blank(),
#         legend.position = "bottom",
#         text = element_text(size = 12)) +
#   guides(shape = guide_legend(override.aes = list(size = 5)),
#          color = guide_legend(override.aes = list(size = 5))) +
#   geom_hline(yintercept = 123, linetype = 3)
# p2

# p2 <- ggplot(data = DT, mapping = aes(x = Year,
#                                       y = totalAreaKm2/1000,
#                                       size = percPredictedWithPP,
#                                       color = species,
#                                       shape = scenario)) +
#   geom_point() +
#   # geom_line() + 10^{3}), Km^{2}), ")"
#   ylab(expression("Total area selected"*" (x 10"^3*" Km"^2*")")) +
#   scale_x_continuous(breaks = yearsWanted) +
#   scale_color_manual(name = "Species",
#                      values = c("forestgreen", "darkmagenta")) +
#   scale_size_continuous(name = "Percent Protected") +
#   scale_shape_manual(name = "Scenario",
#                      values = c(15, 17, 19)) +
#   theme(legend.key = element_blank(),
#         legend.background = element_blank(),
#         legend.position = "bottom",
#         text = element_text(size = 12)) +
#   guides(shape = guide_legend(override.aes = list(size = 4)),
#          color = guide_legend(override.aes = list(size = 4))) +
#   geom_hline(yintercept = 123, linetype = 3) +
#   geom_jitter()
# p2


# p3 <- ggplot(data = DT_2, mapping = aes(x = Year,
#                                       y = percPredictedWithPP,
#                                       size = totalAreaKm2,
#                                       color = species,
#                                       shape = scenario)) +
#   geom_jitter(width = 1) +
#   ylab("Percentage protected with selected areas") +
#   scale_x_continuous(breaks = yearsWanted) +
#   scale_color_manual(name="Species",
#                      values = c("forestgreen", "darkmagenta")) +
#   scale_size_continuous(name = expression(Total~Area~(Km^2)),
#                         breaks = totKmBreaks) +
#   scale_shape_manual(name = "Focused protection",
#                      values = c(15, 17, 19)) +
#   theme(legend.key = element_blank(),
#         legend.background = element_blank()) +
#   guides(shape = guide_legend(override.aes = list(size = 4)),
#          color = guide_legend(override.aes = list(size = 4)))
# p3


# library("gridExtra")
# fileNamePNG <- file.path(Paths$outputPath,
#                          paste0("Figure2.png"))
# png(filename = fileNamePNG,
#     width = 21, height = 29,
#     units = "cm", res = 300)
# grid.arrange(p, p2, ncol = 2)
# dev.off()
