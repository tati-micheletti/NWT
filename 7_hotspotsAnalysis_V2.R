#########################################################
##                 H O T S P O T S                     ##
#########################################################

# This version of the hotspot analysis does not use the spatial optimization

# Run !sourceScript.R until script 2

################################

# 1. For each year, get all runs and calculate the mean of each scenario
stepInterval <- 20 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)
# wantedScenarios <- paste0("area", seq(5, 95, by = 10))
wantedScenarios <- seq(0.05, 0.95, by = 0.1)
internalFolder <- "ms1" # Here to set in which folder are the scenarios I am looking for (i.e. ms1)
runAUTO <- TRUE

SpaDES.core::setPaths(cachePath = hotspotsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))
generalOutputs <- dirname(file.path(getwd(), "outputs",
                                    "landscapeRuns",
                                    definedRun$whichRUN,
                                    replicateNumber))
if (all(runLandR == FALSE)){
  if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
  newInputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                       replacement = originalDateAnalysis)
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  setPaths(inputPath = newInputPath,
           outputPath = newOutputPath)
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots/ms"), create = TRUE)

  setPaths(outputPath = hotOutPath)
} else {
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots_ms"), create = TRUE)
  setPaths(inputPath = newOutputPath,
           outputPath = hotOutPath)
}


if (runAUTO){
  climateScenarios <- strsplit(x = climateModel, split = "_")[[1]][1]
  # runs <- paste0("run", RUN)
  runs <- paste0("run", 1:5) # Trying all replicates in one bash
  # message(crayon::red(paste0("PROCESSING Climate Scenarios ", climateScenarios, " run ", RUN)))
  message(crayon::red(paste0("PROCESSING Climate Scenarios ", climateScenarios, " all runs")))
} else {
  climateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
  runs <- paste0("run", 1:5)
}

# Here I need to randomly select areas for each and save them as new scenario names (42:51)
# in Paths$outputPath, as paste0("solRas_", scen, "_Year2011.tif")
source("~/projects/NWT/posthocFunctions/randomlySelectAreas.R")

source('functions/makePlanningUnit.R')
planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)

################################
overwriteFig1Numbers <- FALSE ### <~~~~ IMPORTANT!!! ATTENTION!!!
overwriteFinalTables <- FALSE
################################

allb <- usefulFuns::substrBoth(list.files("~/projects/NWT/modules/birdsNWT/data/models/",
                                          pattern = "brt8.R"),
                               howManyCharacters = 4,
                               fromEnd = FALSE)

fullFinalTablePath <- file.path(Paths$outputPath, "fullFinalTable.qs") # Make and save the darn full table!

if(any(overwriteFig1Numbers,
       !file.exists(fullFinalTablePath))){
  tic("Total elapsed time for creating table: ")
  finalTable <- rbindlist(lapply(X = climateScenarios, function(ClimateScenario){
    allRuns <- rbindlist(lapply(X = runs, function(Run){
      allYears <- rbindlist(lapply(X = yearsWanted, function(y){
        message(crayon::white(paste0("Starting ", y, " for ",
                                     Run, " for ", ClimateScenario)))
        tic(paste0("Post process for ", y, " for ", Run, " for ", ClimateScenario,
                   " finished. TIME ELAPSED: "))
        # Create water template
        water <- rasterToMatch
        water[waterRaster == 1] <- NA
        # [1] 15532882 15561327 These are NA in birds but not in the RTM or Boo?
        # Legacy from resampling? I will manually exclude them. 2 pixels are not
        # relavant for the results
        water[c(15532882, 15561327)] <- NA
        
        # 1. Make the solution for random
        # 1.1. If you have the solution, load it
        randomSolution <- file.path(Paths$outputPath,
                                    paste0("Solution_random_", ClimateScenario, "_",
                                           Run, "_", y,".qs"))
        message(crayon::yellow(paste0("Creating solutions for ",
                                      Run, " for ", ClimateScenario, 
                                      " for random")))
        
        if (!file.exists(randomSolution)){
          randomPrediction <- water
          
          # For some reason, we have 2 pixels in the birds
          randomTable <- na.omit(data.table(pixelID = 1:ncell(randomPrediction),
                                            vals = getValues(randomPrediction)))
          maxPix <- NROW(randomTable)
          
          for (perc in wantedScenarios){
            selectedPix <- sample(randomTable[["pixelID"]], round(maxPix*perc, 0))
            randomTable[pixelID %in% selectedPix, paste0("area", perc) := 1]
            randomTable[!pixelID %in% selectedPix, paste0("area", perc) := 0]
          }
          
          randomTable <- merge(randomTable, data.table(pixelID = 1:ncell(randomPrediction)), 
                               all.y = TRUE, by = "pixelID")
          setkey(randomTable, pixelID)
          randomSolutionStk <- raster::stack(lapply(paste0("area", wantedScenarios), function(A){
            randomRas <- setValues(randomPrediction, randomTable[[A]])
            return(randomRas)
          }))
          names(randomSolutionStk) <- paste0("Solution_random_", 
                                             ClimateScenario, "_",Run,
                                             "_", y, "_", wantedScenarios)
          
          randomSolutionMap <- file.path(Paths$outputPath,
                                         paste0("Solution_random_", ClimateScenario, "_",
                                                Run,"_", y,".tif"))
          if (!file.exists(randomSolutionMap))
            writeRaster(randomSolutionStk, filename = randomSolutionMap, format = "GTiff")
          qs::qsave(x = randomTable, file = randomSolution)
          rm(randomSolutionStk)
        }
        randomTable <- qs::qread(randomSolution)
        
        
        # 2. Make the solution for caribou
        # 2.1. If you have the solution, load it
        booSolution <- file.path(Paths$outputPath,
                                 paste0("Solution_caribou_", ClimateScenario, "_",
                                        Run, "_", y,".qs"))
        message(crayon::yellow(paste0("Creating solutions for ",
                                      Run, " for ", ClimateScenario, 
                                      " for caribou")))
        if (!file.exists(booSolution)){
          # 1.2. Find the prediction for the given CS, run, year
        allBooFiles <- grepMulti(list.files(file.path(generalOutputs, paste(ClimateScenario, Run, 
                                                                            sep = "_")), 
                                  recursive = TRUE, full.names = TRUE),
                                  patterns = c("relativeSelectioncaribou", y, ".tif"))
        if (length(allBooFiles) > 1) stop("more than one caribou raster being selected. please debug")
        # Add catch for more than 1 file?
        booPrediction <- raster(allBooFiles)
        
        # Clean up water!
        booPrediction[is.na(water)] <- NA
        
        booTable <- data.table(pixelID = 1:ncell(booPrediction),
                               vals = getValues(booPrediction)) |>
          na.omit() |>
          setkey(vals)
        
        maxPix <- NROW(booTable)
        
        for (perc in wantedScenarios){

          booTable[, paste0("area", perc) := c(rep(0, times = round(maxPix*(1-perc), 0)),
                                               rep(1, times = round(maxPix*perc, 0)))]
        }
        booTable <- merge(booTable, data.table(pixelID = 1:ncell(booPrediction)), 
                           all.y = TRUE, by = "pixelID")
        setkey(booTable, pixelID)
        booSolutionStk <- raster::stack(lapply(paste0("area", wantedScenarios), function(A){
          booRas <- setValues(booPrediction, booTable[[A]])
          return(booRas)
        }))
        names(booSolutionStk) <- paste0("Solution_caribou_", 
                                        ClimateScenario, "_",Run,
                                        "_", y,"_", wantedScenarios)
        booSolutionMap <- file.path(Paths$outputPath,
                                    paste0("Solution_caribou_", ClimateScenario, "_",
                                           Run, "_", y, ".tif"))
        if (!file.exists(booSolutionMap))
          writeRaster(booSolutionStk, filename = booSolutionMap, format = "GTiff")
        qs::qsave(x = booTable, file = booSolution)
        rm(booSolutionStk)
        }
        booTable <- qs::qread(booSolution)
    
          # 3. Make the solution for  each species 
          # 3.1. If you have the solution, load it

        allSp <- rbindlist(lapply(X = allb, function(sp){
        
          birdFinalTable <- file.path(Paths$outputPath,
                                    paste0("finalTable_", sp,"_", 
                                           ClimateScenario, "_",
                                           Run, "_", y,".qs"))
          if (any(!file.exists(birdFinalTable), 
                  overwriteFinalTables)){
            
          birdSolution <- file.path(Paths$outputPath,
                                    paste0("Solution_", sp,"_", 
                                           ClimateScenario, "_",
                                           Run, "_", y,".qs"))
          message(crayon::yellow(paste0("Creating solutions for ", sp, 
                                        " ", Run, " for ", ClimateScenario, 
                                        " for ", y, " for ", sp, "(", which(sp == allb), " of ", 
                                        length(allb), ")")))
          if (!file.exists(birdSolution)){
            
            # 1.2. Find the prediction for the given CS, run, year
            allBirdFiles <- grepMulti(list.files(file.path(generalOutputs, paste(ClimateScenario, Run, 
                                                                                sep = "_")), 
                                                recursive = TRUE, full.names = TRUE),
                                     patterns = c(sp, y, ".tif"))
            if (length(allBirdFiles) > 1) stop("more than one bird raster being selected. please debug")
            # Add catch for more than 1 file?
            birdPrediction <- raster(allBirdFiles)
            
            # Clean up water!
            birdPrediction[is.na(water)] <- NA
            
            birdTable <- data.table(pixelID = 1:ncell(birdPrediction),
                                   vals = getValues(birdPrediction)) |>
              na.omit() |>
              setkey(vals)
            
            maxPix <- NROW(birdTable)
            
            for (perc in wantedScenarios){
              birdTable[, paste0("area", perc) := c(rep(0, times = round(maxPix*(1-perc), 0)),
                                                   rep(1, times = round(maxPix*perc, 0)))]
            }
            birdTable <- merge(birdTable, data.table(pixelID = 1:ncell(birdPrediction)), 
                              all.y = TRUE, by = "pixelID")
            setkey(birdTable, pixelID)
            birdSolutionStk <- raster::stack(lapply(paste0("area", wantedScenarios), function(A){
              birdRas <- setValues(birdPrediction, birdTable[[A]])
              return(birdRas)
            }))
            names(birdSolutionStk) <- paste0("Solution_", sp,"_", 
                                            ClimateScenario, "_",Run,
                                            "_", y,"_", wantedScenarios)
            birdSolutionMap <- file.path(Paths$outputPath,
                                      paste0("Solution_", sp,"_", 
                                             ClimateScenario, "_",
                                             Run, "_", y, ".tif"))
            if (!file.exists(birdSolutionMap))
              writeRaster(birdSolutionStk, filename = birdSolutionMap, format = "GTiff")
            qs::qsave(x = birdTable, file = birdSolution)
            rm(birdSolutionStk)
          }
            birdTable <- qs::qread(birdSolution)
            
            # 4. With all solutions created, we need to then calculate the abundances 
            # under each scenario. The important here are only the birds.
            
            # 4.1. test all pixelID match!
            # Check all NA's match
            bV <- birdTable$vals
            cV <- booTable$vals
            rV <- randomTable$vals
            test1 <- which(is.na(bV)&!is.na(rV))
            test2 <- which(is.na(bV)&!is.na(cV))
            test3 <- which(is.na(cV)&!is.na(rV))
            test4 <- which(is.na(cV)&!is.na(bV))
            test5 <- which(is.na(rV)&!is.na(bV))
            test6 <- which(is.na(rV)&!is.na(cV))
            if (sum(length(test1),
                    length(test2),
                    length(test3),
                    length(test4),
                    length(test5),
                    length(test6)) != 0) 
              stop("There are non-expected NA's in a raster! Please debug")

            birdTable <- na.omit(birdTable)
            booTable <- na.omit(booTable)
            randomTable <- na.omit(randomTable)

            # Below refers to Junior's comments:
            # 
            # OPTION A: Converting to probability of occurrence:
            # We shouldn't use this because we are interested in the
            # abundance, with this we can't work with abundance
            # birdTable[, vals := 1-dpois(x = 0, lambda = vals)]
            
            # OPTION B: use 99% quantiles
            # We shouldn't use this because the rasters looked fine (acc. Diana/Ana/Steve C.)
            # maxVal <- quantile(birdTable[["vals"]], 0.99)
            # minVal <- quantile(birdTable[["vals"]], 0.01)

            # OPTION C: Median and Sum
            # Go ahead with the tables as are with sum and median.
            # See if we have results that are too different
            
            # Calculate the best protection
            DT <- rbindlist(lapply(wantedScenarios, function(Area){
              area <- paste0("area", Area)
              
              birdIndex <- birdTable[[area]]*birdTable[["vals"]]
              # Calculate the caribou protection
              booIndex <- booTable[[area]]*birdTable[["vals"]]
              # Calculate the worse protection
              randomIndex <- randomTable[[area]]*birdTable[["vals"]]
              
              # MEDIAN: Individuos calculation: bird maps are in density --> d*6.25 to abundance
              # TotalIndividualsInitialM <- 6.25*median(birdTable[["vals"]])
              # TotalIndividualsCurrentM <- 6.25*c(median(birdIndex[birdIndex != 0]), 
              #                                    median(booIndex[booIndex != 0]), 
              #                                    median(randomIndex[randomIndex != 0]))
              
              # SUM: Individuos calculation: bird maps are in density --> d*6.25 to abundance
              TotalIndividualsInitial <- 6.25*sum(birdTable[["vals"]])
              TotalIndividualsCurrent <- 6.25*c(sum(birdIndex), 
                                                 sum(booIndex), 
                                                 sum(randomIndex))
               Dt <- data.table(
                      Species = sp,
                      ClimateScenario = ClimateScenario,
                      Run  = Run,
                      Year = y,
                      PrioritizedFor  = c(sp, "caribou", "random"),
                      ProportionAreaChosen  = Area,
                      TotalIndividualsInitial = TotalIndividualsInitial,
                      TotalIndividualsCurrent = TotalIndividualsCurrent,
                      ProportionIndividualsConserved = TotalIndividualsCurrent/TotalIndividualsInitial
                    )

               return(Dt)
            }))
            qs::qsave(DT, file = birdFinalTable)
            message(crayon::green(paste0("Postprocess for ", sp, 
                                          " ", Run, " for ", ClimateScenario, 
                                          " finished for ", sp, " (", which(sp == allb), " of ", 
                                          length(allb), ")")))
          } else {
            DT <- qs::qread(birdFinalTable)
          }
          return(DT)
        }))
        toc()
        return(allSp)
      }))
      return(allYears)
    }))
    return(allRuns)
  }))
  # qs::qsave(finalTable, file = fullFinalTablePath) # ONCE FINISHED RUNNING, UNCOMMENT HERE AND RUN TO GET THE FULL TABLE CREATED and change AUTO to FALSE
  toc()
} else {
  finalTable <- qs::qread(fullFinalTablePath)
}

print("Process finished!")
