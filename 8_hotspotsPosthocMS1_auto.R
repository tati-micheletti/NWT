#########################################################
##       P O S T H O C         H O T S P O T S    2    ##
#########################################################

# Run !sourceScript.R until script 2

################################

# 1. For each year, get all runs and calculate the mean of each scenario
stepInterval <- 20 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)
allScenarios <- as.character(utils::as.roman(22:51))
internalFolder <- "ms1" # Here to set in which folder are the scenarios I am looking for (i.e. ms1)
wantedScenarios <- allScenarios

################################
overwriteFig1Numbers <- TRUE ### <~~~~ IMPORTANT!!! ATTENTION!!!
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

allNumbersPath <- file.path(Paths$outputPath, "allNumbersTable.qs") # Make and save the darn full table!

climateScenarios <- strsplit(x = climateModel, split = "_")[[1]][1]#c("CCSM4", "CanESM2", "INM-CM4")
runs <- paste0("run", RUN)
message(crayon::red(paste0("PROCESSING Climate Scenarios ", climateScenarios, " run ", RUN)))
specificScenarios <- data.table(expand.grid(as.character(as.roman(32:41)), 
                                            c("shrub", "generalist", "deciduous",
                                              "conifer", "wetland", "grassland", 
                                              "mixedwood")))
specificScenarios[, scenarios := paste(Var1, Var2, sep = "_")]
wantedScenarios <- c(as.character(as.roman(22:31)), # Caribou Conservation Scenarios
                     specificScenarios[["scenarios"]], # Reference Scenarios
                     as.character(as.roman(42:51))) # Random Scenarios
yearsWanted <- seq(2011, 2091, by = 20)

# Here I need to randomly select areas for each and save them as new scenario names (42:51)
# in Paths$outputPath, as paste0("solRas_", scen, "_Year2011.tif")
source("~/projects/NWT/posthocFunctions/randomlySelectAreas.R")

# Prepare rasters and calculate the following:

# Climate Scenarios (x3):
#       CCSM4, CanESM2, INM-CM4

# Runs (x5):
#       Run1:Run5

# Scenarios (x 80):
#       22:31 = prioritize for caribou (n = 10);
#       32:41 = prioritize for bird group (groups = 6, which means '32:41_group') (n = 60);
#       42:51 = Null scenario (random pixels) (n = 10)

# Years wanted (x5): 
#       2011, 2031, 2051, 2071, 2091

# TOTAL ROWS PER SPECIES (5*80*3*5) = 6,000
# TOTAL ROWS (75 birds + 1 caribou) (76*6,000) = 456,000

# In the table:
#   A. Species;
#   B. Scenario (separate the group here for scenarios 32:41);
#   C. ClimateScenario;
#   D. Run;
#   E. Year;
#   F. PrioritizedFor;
#   G. TotalAreaNWT;
#   H. TotalAreaChosen;
#   I. PercentageAreaChosen;
#   I. TotalIndividualsInitial;
#   J. TotalIndividualsCurrent;
#   K. PercentageIndividualsConserved;

# 1. FOR 2011:
#   1.1. Total area of the raster (with water as we are allowing water to be selected) 
#        (multiplied by 6.5 to convert to ha);
#   1.2. Total area of the raster selected as solution (multiplied by 6.5 to convert to ha);
#   1.4. Extract for each bird species and caribou the predicted raster (OP) for 2011 without 
#        solution --> "Original Predictions";
#   1.5. Save OP;
#   1.5. Sum the total caribou and total for each bird for the OP (Predicted in 2011)

# 2. FOR 2011 - 2091:
#   2.1. For each year wanted, run, scenario, and climate extract for each bird species and 
#        caribou the predicted raster multiplied by the solution raster (YP)
#   2.2. Save YP;
#   2.3. Sum the total caribou and total for each bird for the YP

# 3. Once all rasters have been processed, create the proportion

# ATTENTION: wantedScenarios needs to also include the combination of 32:41_group!


source('functions/makePlanningUnit.R')
planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)

tic("Total elapsed time for creating all scenarios: ")
Fig1_numbers <- rbindlist(lapply(X = climateScenarios, function(ClimateScenario){
    allRuns <- rbindlist(lapply(X = runs, function(Run){
      allScenarios <- rbindlist(lapply(X = wantedScenarios, function(scen){
        message(crayon::white(paste0("Starting scenario ", scen, " for ", 
                                     Run, " for ", ClimateScenario)))
        tic(paste0("Time elapsed for scenario ", scen, " ", ClimateScenario, " ", Run))
        
        booBirdTable <- file.path("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1/",
                                  paste0("FigNumbers_", ClimateScenario, "_",
                                         Run,"_",
                                         scen,".qs"))
        
        if (!file.exists(booBirdTable)){
          message(crayon::red(paste0(booBirdTable, " does not exist. Creating...")))
          # Separate scenario from PrioritizedFor
          Scenario <- strsplit(x = scen, split = "_")[[1]][1]
          PrioritizedFor <- strsplit(x = scen, split = "_")[[1]][2]
          if (is.na(PrioritizedFor)){
            PrioritizedFor <- ifelse(as.numeric(as.roman(Scenario)) > 41, 
                                     "random", 
                                     "caribou")
          }
          # To recover the solutions, we need to look for the specific 
          # scenarios, runs and climate scenarios
          # in the Paths$outputPath folder (for optimized ones) or create 
          # the random ones
          
          fold <- file.path(dirname(dirname(Paths$outputPath)),
                            paste0(ClimateScenario, "_", Run),
                            "hotspots/ms1")
          
          if (PrioritizedFor == "random"){
            solfileName <- file.path(fold, 
                                     paste0(Scenario,
                                            "_solutions_Year2011.tif"))
            if (file.exists(solfileName)){
              print(paste0("Solution raster for scenario ",  Scenario, 
                           " already exists."))
            } else {
              print(paste0("Solution raster for scenario ",  Scenario, 
                           " does not exist; creating..."))
              areaToConserve <- switch(EXPR = Scenario, 
                                       XLII = 0.05,
                                       XLIII = 0.15,
                                       XLIV = 0.25,
                                       XLV = 0.35,
                                       XLVI = 0.45,
                                       XLVII = 0.55,
                                       XLVIII = 0.65,
                                       XLIX = 0.75,
                                       L = 0.85,
                                       LI = 0.95)
              ras <- planningUnit[[1]]
              ras[!is.na(planningUnit[[1]][])] <- 1
              solRas <- randomlySelectAreas(ras = ras, 
                                            percentageArea = areaToConserve, 
                                            reps = 5)
              writeRaster(solRas, filename = solfileName)
            }
            solRas <- raster::raster(solfileName)
          } else {
            solRasName <- grepMulti(x = list.files(path = fold,
                                                   full.names = TRUE), 
                                    patterns = c(paste0(scen, "_solutions"), "Year2011"))
            if (length(solRasName)>1){
              solRasName <- file.path(fold, paste0(scen, "_solutions_Year2011.tif"))
            }
            if (!file.exists(solRasName)){
              stop(paste0("The file ", solRasName, " doesn't exist in the folder ", fold))
            } else {
              solRas <- raster::raster(solRasName)
            }
          }
          solRas[] <- solRas[]
          # Calculate the area related metrics
          TotalAreaChosen <- 6.25*sum(solRas[], na.rm = TRUE)
          totRas <- solRas
          totRas[!is.na(solRas)] <- 1
          TotalAreaHaNWT <- 6.25*sum(totRas[], na.rm = TRUE)
          PercentageAreaChosen <- TotalAreaChosen/TotalAreaHaNWT
          
          message(crayon::yellow("Calculating % caribou RSF and bird abundance for ", 
                                 paste(scen, Run, ClimateScenario, sep = " ")))
          tic(paste0("Boo and birds calculations elapsed time for ", 
                     paste(scen, Run, ClimateScenario, sep = " "),": "))
          
          # Get all birds %
          birdsPerc <- rbindlist(lapply(allb, function(BIRD){
            
            # NEED TO EXTRACT THE TOTAL BIRDS AT THE MOMENT -- 2011 AS A BASELINE
            
            message(crayon::green(paste0("Extracting predictions of ", BIRD, 
                                         ". Species ", which(allb == BIRD), " of ", length(allb))))
            
            flName <- grepMulti(x = list.files(file.path(dirname(dirname(fold)), "birdPredictionsV8"), 
                                               full.names = TRUE),
                                patterns = c(BIRD, "Year2011.tif"))
            if (!file.exists(flName)){
              stop(flName, " doesn't exist. Please check file location.")
            }
            stkOriginal <- raster::raster(flName)
            
            # NEED TO EXTRACT THE TOTAL BIRDS AT EACH YEAR IN TIME -- 
            # TO CALCULATE HOW MUCH WE ARE PROTECTING
            
            allYears <- rbindlist(lapply(yearsWanted, function(Y){
              
              message(paste0("Extracting predictions of ", BIRD, 
                             " for year ", Y,
                             "."))
              
              flName <- grepMulti(x = list.files(file.path(dirname(dirname(fold)), "birdPredictionsV8"), 
                                                 full.names = TRUE),
                                  patterns = c(BIRD, "Year", Y,".tif"))
              if (!file.exists(flName)){
                stop(flName, " doesn't exist. Please check file location.")
              }
              stkCurrent <- raster::raster(flName)
              
              TotalIndividualsInitial <- 6.25*sum(stkOriginal[], na.rm = TRUE)
              # Convert density to abundance (per pixel we have 6.25 ha)
              conservSp <- stkCurrent*solRas
              TotalIndividualsCurrent <- 6.25*sum(conservSp[], na.rm = TRUE)
              
              DT <- data.table(
                Species = BIRD,
                Scenario = Scenario,
                ClimateScenario  = ClimateScenario,
                Run  = Run,
                Year = Y,
                PrioritizedFor  = PrioritizedFor,
                TotalAreaHaNWT  = TotalAreaHaNWT,
                TotalAreaChosen  = TotalAreaChosen,
                ProportionAreaChosen  = PercentageAreaChosen,
                TotalIndividualsInitial = TotalIndividualsInitial,
                TotalIndividualsCurrent = TotalIndividualsCurrent,
                ProportionIndividualsConserved = TotalIndividualsCurrent/TotalIndividualsInitial
              )
              return(DT)
            }))
          }))
          # Get caribou %
          booPerc <- rbindlist(lapply("caribou", function(BOO){
            
            # NEED TO EXTRACT THE TOTAL BOO AT THE MOMENT -- 2011 AS A BASELINE
            
            message(crayon::green(paste0("Extracting predictions of ", BOO, 
                                         ".")))
            # Need to bin
            stkOriginal <- raster::raster(grepMulti(x = list.files(file.path(dirname(dirname(fold)), "caribouPredictions"), 
                                                                   full.names = TRUE),
                                                    patterns = c("relativeSelectioncaribouRSF", "Year2011.tif"),
                                                    unwanted = "Uncertain"))
            stkOriginal <- binRSFtoDeMars2019(stkOriginal)
            
            # NEED TO EXTRACT THE TOTAL BOO AT EACH YEAR IN TIME -- 
            # TO CALCULATE HOW MUCH WE ARE PROTECTING
            
            allYears <- rbindlist(lapply(yearsWanted, function(Y){
              
              message(paste0("Extracting predictions of ", BOO, 
                             " for year ", Y,
                             "."))
              # Need to bin
              stkCurrent <- raster::raster(grepMulti(x = list.files(file.path(dirname(dirname(fold)), "caribouPredictions"), 
                                                                    full.names = TRUE),
                                                     patterns = c("relativeSelectioncaribouRSF", "Year", Y,".tif"),
                                                     unwanted = "Uncertain"))
              stkCurrent <- binRSFtoDeMars2019(stkCurrent)
              
              TotalIndividualsInitial <- 6.25*sum(stkOriginal[], na.rm = TRUE)
              # Convert density to abundance (per pixel we have 6.25 ha)
              conservSp <- stkCurrent*solRas
              TotalIndividualsCurrent <- 6.25*sum(conservSp[], na.rm = TRUE)
              
              DT <- data.table(
                Species = BOO,
                Scenario = Scenario,
                ClimateScenario  = ClimateScenario,
                Run  = Run,
                Year = Y,
                PrioritizedFor  = PrioritizedFor,
                TotalAreaHaNWT  = TotalAreaHaNWT,
                TotalAreaChosen  = TotalAreaChosen,
                ProportionAreaChosen  = PercentageAreaChosen,
                TotalIndividualsInitial = TotalIndividualsInitial,
                TotalIndividualsCurrent = TotalIndividualsCurrent,
                ProportionIndividualsConserved = TotalIndividualsCurrent/TotalIndividualsInitial
              )
              return(DT)
            }))
            
            
          }))
          toc()
          bnc <- merge(booPerc, birdsPerc, all = TRUE)
          qs::qsave(bnc, file = booBirdTable)
        } else {
          message(crayon::green(paste0(booBirdTable, " already exists. Returning...")))
          bnc <- qs::qread(booBirdTable)
        }
        toc()
        return(bnc)
      }))
    }))
  }))
toc()
message(crayon::green(paste0("Climate Scenario ", climateScenarios, " run ", RUN, " done!!")))
