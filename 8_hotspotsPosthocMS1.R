#########################################################
##       P O S T H O C         H O T S P O T S    2    ##
#########################################################

# Run !sourceScript.R until script 2

################################

library("Require")
Require("data.table")
Require("SpaDES")

# 1. For each year, get all runs and calculate the mean of each scenario
stepInterval <- 20 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)
allScenarios <- as.character(utils::as.roman(22:51))
internalFolder <- "ms1" # Here to set in which folder are the scenarios I am looking for (i.e. ms1)
wantedScenarios <- allScenarios
excludeWaterFromArea <- FALSE
excludeLastYear <- FALSE

################################
overwriteFig1Numbers <- FALSE ### <~~~~ IMPORTANT!!! ATTENTION!!!
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

climateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
runs <- paste0("run", 1:5)
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

source('functions/makePlanningUnit.R')
planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)

if(any(overwriteFig1Numbers,
       !file.exists(allNumbersPath))){
  tic("Total elapsed time for creating table: ")
  Fig1_numbers <- rbindlist(lapply(X = climateScenarios, function(ClimateScenario){
    allRuns <- rbindlist(lapply(X = runs, function(Run){
      allScenarios <- rbindlist(lapply(X = wantedScenarios, function(scen){
        message(crayon::white(paste0("Starting scenario ", scen, " for ", 
                                   Run, " for ", ClimateScenario)))
        
        booBirdTable <- file.path(Paths$outputPath,
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
                                          reps = 1)
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
        return(bnc)
      }))
    }))
  }))
  toc()
  tic("Table saving time: ")
  qs::qsave(Fig1_numbers, file = allNumbersPath)
  toc()
} else {
  Fig1_numbers <- qs::qread(allNumbersPath)
}
Fig1_numbers <- Fig1_numbers[Species != "caribou", ]

################
#              #
#   FIGURES    #
#              #
################

library("lattice")
library("rasterVis")
library("viridis")
library("maptools")
library("colorspace")
library("ggplot2")
library("googledrive")

# Because we actually allow water to be counted as total area (because we do have water coefficient 
# for both birds and caribou models), we end up 
# getting a percentage of area chosen that is actually higher than the intended (19% of the land is
# water). Because birds are NOT distributed in water (predictions coming from the original predictions)
# we can re-calculate the total area chosen and, consequently, proportion of area chosen. This
# ensures we properly build the ProportionIndividualsConserved in function of proportion area
# chosen. If we were looking at caribou, this would be different (as water is in fact an area we have 
# interest for setting as priority).
if (excludeWaterFromArea){
  totalAreaWater <- sum(waterRaster[], na.rm = TRUE)*6.25 # Area in ha to match the table
  Fig1_numbers[, TotalAreaHaNWT := TotalAreaHaNWT-totalAreaWater]
  Fig1_numbers[, ProportionAreaChosen := TotalAreaChosen/TotalAreaHaNWT]
  Fig1_numbers[, ProportionAreaChosen  := round(ProportionAreaChosen , 1)]
  # Now we should also remove any proportions of area that are above 1
  Fig1_numbers <- Fig1_numbers[ProportionAreaChosen  < 1, ]
  } 

# We also have some weird results for the last year of simulations (2091).
# This is especially true for grassland (proportionAreaChosen == 0.8) and shrub species 
# (proportionAreaChosen == 0.19). Not sure exaclty why this is, but maybe it is due to 
# an abrupt change in landscape? 
if (excludeLastYear){
  Fig1_numbers <- Fig1_numbers[Year < 2090, ]
}

# First thing to do, is to group the birds into their original groups. We then recalculate the
# ProportionIndividualsConserved by summing for all species the TotalIndividualsInitial and the 
# TotalIndividualsCurrent
generalOutputs <- Paths$outputPath
birdsGroupingTable <- prepInputs(url = "https://drive.google.com/file/d/1SGJ5ABhafT97wm2wIUyK6bIyWeTZ4e7S/pub?output=csv", 
                                 targetFile = "Bird Classification - birdHabitatRevised.csv",
                                 destinationPath = generalOutputs, 
                                 fun = "data.table::fread", 
                                 header = TRUE)

# Simplyfying and putting the correct names
birdsGroupingTable <- birdsGroupingTable[, c("Species Code", "Habitat")]
names(birdsGroupingTable) <- c("Species", "Habitat")

# Merge the tables
Fig1_numbers2 <- merge(Fig1_numbers, 
                       birdsGroupingTable, by = "Species", all.x = TRUE)

DT2 <- Copy(Fig1_numbers2)
DT2[, c("TotalAreaHaNWT", "TotalAreaChosen", "Scenario") := NULL]

DT2[, ProportionIndividualsConserved := sum(TotalIndividualsCurrent)/sum(TotalIndividualsInitial), 
   by = c("Year", "ClimateScenario", "Run", "PrioritizedFor", 
          "ProportionAreaChosen", "Habitat")]

DT2[, c("Species", "TotalIndividualsInitial", 
       "TotalIndividualsCurrent") := NULL]

DT2 <- unique(DT2)

DT2[is.na(Habitat), Habitat := "caribou"]

names(DT2)[names(DT2) == "Habitat"] <- "Species"

########################
# FINAL PLOT AND TABLE #
########################

CI <- function (x, ci = 0.95, toReturn = "mean", type = "deviation"){
  a <- mean(x)
  s <- sd(x)
  n <- length(x)
  if (type == "error"){
    error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n) 
  } else {
    if (type == "deviation"){
      error <- qt(ci + (1 - ci)/2, df = n - 1) * s
    } else 
      stop("Type mus be either 'error' or 'deviation'")
  }
  if (toReturn == "mean")
    return(a) else
      if (toReturn == "L")
        return(a-error) else
          if (toReturn == "U")
            return(a+error)
}

###################################
# FIGURE 1: Individual birds plot #
###################################

DT3 <- Copy(Fig1_numbers2)
DT3[, c("TotalAreaHaNWT", "TotalAreaChosen", "Scenario") := NULL]
DT3 <- DT3[Species != "caribou",]

# umbrellaTable1 <- rbindlist(lapply(unique(DT3$Species), function(sp){
#   DT <- dcast(DT3[Species == sp],
#               Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
#               value.var = "ProportionIndividualsConserved")
#   habitat <- unique(DT[["Habitat"]])
#   toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
#               "caribou", habitat, "random")
#   DT <- DT[Species == sp, ..toKeep]
#   setkey(DT, "Species", "Year", "ProportionAreaChosen", "ClimateScenario", "Run")
#   DT[, higherValueUmbrella := fifelse(caribou > get(habitat),
#                                       TRUE, FALSE)]
#   DT[higherValueUmbrella == TRUE, ]
#   DTr <- data.table(Species = sp,
#                     umbrellaHigher = NROW(DT[higherValueUmbrella == TRUE]),
#                     umbrellaLower = NROW(DT[higherValueUmbrella == FALSE]))
#   return(DTr)
# }))


DT3[Species == "YRWA" & Run == "run5" & ClimateScenario == "INM-CM4" &
      ProportionAreaChosen == "0.95" & Habitat == "conifer" & Year == 2091, ]

DT3x1[Species == "YRWA" & Run == "run5" & #ClimateScenario == "INM-CM4" &
        ProportionAreaChosen == "0.95" & Habitat == "conifer" & Year == 2091, ]

DTx1 <- dcast(DT3x1, 
             Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
             value.var = "ProportionIndividualsConserved")

toRemove <- T
toInvert <- F

##############################################################################################
# Getting excluded points and detailed table (Species by Year and Proportion Area Conserved) #
##############################################################################################

comparisonTableExtra1 <- lapply(unique(DTx1$Species), function(sp){
  message(paste0("Processing ", sp, " (", which(unique(DTx1$Species) == sp),
                 " of ", length(unique(DTx1$Species)), ")"))
  allYears <- lapply(unique(DTx1$Year), function(Y){
    allAreas <- lapply(unique(DTx1$ProportionAreaChosen), function(A){
      habitat <- unique(DTx1[Species == sp, Habitat])
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "Habitat", "caribou", "random", habitat)
      DT <- DTx1[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      rowsToRemove <- which(DT[["random"]] > DT[[habitat]])
      toExcl <- data.table()
      if (toRemove){
        # ID and Remove rows where random is bigger than the reference --> cause for artifacts
        if (length(rowsToRemove) != 0){
          toExcl <- DT[rowsToRemove]
          names(toExcl)[names(toExcl) == habitat] <- "reference"
          # Warning about which Species, Year, climate scenario, and Run we have this problem
          message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                        crayon::red(NROW(toExcl))," points for ", crayon::red(sp), 
                                        " year ", crayon::red(Y), " area ", crayon::red(A), 
                                        " and ", 
                                        crayon::red(paste(unique(toExcl[["ClimateScenario"]]), 
                                                          collapse = "; ")))))
          DT <- DT[-rowsToRemove]
          if (NROW(DT) == 0){
            return(list(tb = NULL, pointsExcluded = toExcl))
          }
        }
      }
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[habitat]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        if (toInvert){
          if (low[i] > upp[i]){
            umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(upp[i], low[i]))
          } else {
            umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
          }
        } else {
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
      }
      
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L"),
                                    CI(umbrellaIndex, toReturn = "U"))]
      DT <- unique(DT[, c("Species", "Year", "ProportionAreaChosen", "Habitat",
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(list(tb = DT, pointsExcluded = toExcl))
    })
    allA <- rbindlist(lapply(allAreas, `[[`, "tb"))
    allPex <- rbindlist(lapply(allAreas, `[[`, "pointsExcluded"))
    # allPex <- sum(c(sapply(allAreas, `[[`, "pointsExcluded")))
    return(list(tb = allA, pointsExcluded = allPex))
  })
  allY <- rbindlist(lapply(allYears, `[[`, "tb"))
  allPex <- rbindlist(lapply(allYears, `[[`, "pointsExcluded"))
  # allPex <- sum(c(sapply(allYears, `[[`, "pointsExcluded")))
  return(list(tb = allY, pointsExcluded = allPex))
})

pointsExcluded1 <- rbindlist(lapply(comparisonTableExtra1, `[[`, "pointsExcluded"))
comparisonTableExtraTB <- rbindlist(lapply(comparisonTableExtra1, `[[`, "tb"))

tableExcluded <- pointsExcluded1[, c("Species", "Year", "ProportionAreaChosen", "ClimateScenario", "Run")]
tableExcluded2 <- dcast(tableExcluded, Species + Year + ProportionAreaChosen + ClimateScenario ~ ., 
                        fun.agg = length)
names(tableExcluded2)[names(tableExcluded2) == "."] <- "ExcludedPoints"

write.csv(tableExcluded2, file = file.path(Paths$outputPath, "excludedPointsSpecies.csv"))
Require("googledrive")
drive_upload(file.path(Paths$outputPath, "excludedPointsSpecies.csv"), 
                          path = as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))

totalPointsPerSpecies <- 5*3*4*8
ptsPerSp <- tableExcluded2[, sum(ExcludedPoints), by = "Species"]
names(ptsPerSp)[names(ptsPerSp) == "V1"] <- "ExcludedPoints"
ptsPerSp[, "proportionPointsExcluded" := round(ExcludedPoints/totalPointsPerSpecies, 2)]

write.csv(ptsPerSp, file = file.path(Paths$outputPath, "summExcludedPointsSpecies.csv"))

Require("googledrive")
drive_upload(file.path(Paths$outputPath, "summExcludedPointsSpecies.csv"), 
             path = as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))
##############################################################################################

plot1TableReady <- lapply(unique(DTx1$Species), function(sp){
  message(paste0("Processing ", sp, " (", which(unique(DTx1$Species) == sp),
                 " of ", length(unique(DTx1$Species)), ")"))
  
  habitat <- unique(DTx1[Species == sp, Habitat])
  toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
              "Habitat", "caribou", "random", habitat)
  DT <- DTx1[Species == sp, ..toKeep]
  rowsToRemove <- which(DT[["random"]] > DT[[habitat]])
  toExcl <- data.table()
  if (toRemove){
    # ID and Remove rows where random is bigger than the reference --> cause for artifacts
    if (length(rowsToRemove) != 0){
      toExcl <- DT[rowsToRemove]
      names(toExcl)[names(toExcl) == habitat] <- "reference"
      # Warning about which Species, Year, climate scenario, and Run we have this problem
      message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                    crayon::red(NROW(toExcl))," points for ", crayon::red(sp), 
                                    " and ", crayon::red(paste(unique(toExcl[["ClimateScenario"]]),
                                                               collapse = "; ")))))
      DT <- DT[-rowsToRemove]
      if (NROW(DT) == 0){
        return(list(tb = NULL, pointsExcluded = toExcl))
      }
    }
  }
  boo <- DT[["caribou"]]
  low <- DT[["random"]]
  upp <- DT[[habitat]]
  umbrellaIndex <- numeric(NROW(DT))
  for (i in 1:length(umbrellaIndex)){
    if (toInvert){
      if (low[i] > upp[i]){
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(upp[i], low[i]))
      } else {
        umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
    } else {
      umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
    }
  }
  
  DT[, c("umbrellaMean",
         "umbrellaLCI",
         "umbrellaUCI") := list(mean(umbrellaIndex),
                                CI(umbrellaIndex, toReturn = "L"),
                                CI(umbrellaIndex, toReturn = "U"))]
  DT <- unique(DT[, c("Species", "Habitat",
                      "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
  return(list(tb = DT, pointsExcluded = toExcl))
})
plot1TableReady <- rbindlist(lapply(plot1TableReady, `[[`, "tb"))

plot1TableReady[, Habitat := factor(Habitat, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                                        "mixedwood", "shrub", "wetland"))]
cols <- gg_color_hue(length(unique(plot1TableReady[["Habitat"]])))

sps <- c("conifer", "deciduous", "generalist", "grassland", 
         "mixedwood", "shrub", "wetland")
Require("ggplot2")

plot1TableReady[, Species := as.character(Species)]
setkey(plot1TableReady, "Habitat")

plot1TableReady[, revorder := frankv(umbrellaMean, order=-1, ties.method = "first"), by = "Habitat"]
setkey(plot1TableReady, Habitat, revorder)

levs <- plot1TableReady[1:75, Species]
plot1TableReady[, Species := factor(Species, levels = rev(levs))]

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
  coord_cartesian(xlim = c(-0.2, 3)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

plot1
ggsave(device = "png", filename = file.path(Paths$outputPath, 
                                            "individualSpecies.png"), 
       width = 8, height = 16)
drive_upload(file.path(Paths$outputPath, "individualSpecies.png"), 
             as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))


# Check still INM-CM4 2
# Make sure to put the posthoc table to run, but for ms2!

###################################
#  FIGURE 2: General birds plot   #
###################################

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

toRemove <- T
toInvert <- F

checkingTable <- rbindlist(lapply(unique(DTx$Species), function(sp){
  allYears <- rbindlist(lapply(unique(DTx$Year), function(Y){
    allAreas <- rbindlist(lapply(unique(DTx$ProportionAreaChosen), function(A){
      message(paste0("Processing ", A, " for ", sp))
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "caribou", "random", sp)
      DT <- DTx[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      whichIsNA <- unlist(lapply(names(DT), function(x){
        allNA <- all(is.na(DT[[x]]))
        if (allNA) return(x) else return(NULL)
      }))
      checked <- data.table(Species = sp,
                            Year = Y,
                            ProportionAreaChosen = A,
                            caribou = ifelse("caribou" %in% whichIsNA, TRUE, FALSE),
                            random = ifelse("random" %in% whichIsNA, TRUE, FALSE),
                            reference = ifelse(sp %in% whichIsNA, TRUE, FALSE)) 
    }))
  }))
}))

comparisonTableExtra <- lapply(unique(DTx$Species), function(sp){
  allYears <- lapply(unique(DTx$Year), function(Y){
    allAreas <- lapply(unique(DTx$ProportionAreaChosen), function(A){
      message(paste0("Processing ", A, " for ", sp))
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "caribou", "random", sp)
      DT <- DTx[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      rowsToRemove <- which(DT[["random"]] > DT[[sp]])
      toExcl <- data.table()
      if (toRemove){
        # ID and Remove rows where random is bigger than the reference --> cause for artifacts
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
      }
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[[sp]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
        if (toInvert){
          if (low[i] > upp[i]){
            umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(upp[i], low[i]))
          } else {
            umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
          }
        } else {
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
      }
      
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
             rows = vars(Species), scales = "free_y") + #, scales = "free_y"
  xlab("Time") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = 1, fill = Species), 
              stat = "identity", alpha = 0.1) +
  ylab(paste0("Umbrella Index")) +
  coord_cartesian(ylim = c(-0.3, 1)) +
  # scale_y_continuous(breaks = seq(-2, 1, by = 0.2),
  #                    limits = c(-2, 1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 12),
        axis.text = element_text(size = 12))

finalPlot
ggsave(device = "png", filename = file.path(Paths$outputPath, 
                                            "speciesGroups.png"), 
       width = 11, height = 8)
drive_upload(file.path(Paths$outputPath, "speciesGroups.png"), 
             as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))

###################################
# FIGURE 3: Averaged across time  #
###################################

DTt <- DT2[Species != "caribou", ]

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
      return(DT[, c("Species", "Run", "ClimateScenario", 
                    "Year", "ProportionAreaChosen", 
                    "umbrellaIndex")])
    }))
  }))
}))

finalTable2 <- Copy(finalTable)
finalTable <- finalTable[, c("Species", "umbrellaIndex")]

finalTableDT <- data.table(finalTable)
finalTableM <- melt(finalTableDT, id.vars = "Species", 
                    measure.vars = "umbrellaIndex")

finalTableF <- finalTableM[, c("meanIndex",
                                  "LCIIndex",
                                  "UCIIndex") := list(mean(value),
                                                      CI(value, toReturn = "L"),
                                                      CI(value, toReturn = "U")), by = c("Species",
                                                                                         "variable")]
finalTableF <- unique(finalTableF[, c("Species", 
                                      "meanIndex", "LCIIndex", "UCIIndex")])

# General Average
# First we need to weight the average to the number of species
finalTableSpecies <- Copy(finalTableDT)
names(finalTableSpecies) <- c("Habitat", "umbrellaIndex")

numberSpecies <- birdsGroupingTable[, .N, by = "Habitat"]
finalTableA <- merge(finalTableSpecies, numberSpecies, by = "Habitat")

weightedCI <- function(x, weights, 
                       conf.level = 0.95,
                       toReturn = "L") {
  Require("Hmisc")
  nx <- length(x)
  df <- nx - 1
  vx <- wtd.var(x, weights, normwt = TRUE) ## From Hmisc
  mx <- weighted.mean(x, weights)
  stderr <- sqrt(vx/nx)
  tstat <- mx/stderr ## not mx - mu
  alpha <- 1 - conf.level
  cint <- qt(1 - alpha/2, df)
  cint <- tstat + c(-cint, cint)
  LCI <- min(cint * stderr)
  UCI <- max(cint * stderr)
  if (toReturn == "L")
    return(LCI) else return(UCI)
}

finalTableA <- data.table(Species = "average", 
                          meanIndex = weighted.mean(x = finalTableA$umbrellaIndex, 
                                                    w = finalTableA$N),
                          LCIIndex = weightedCI(finalTableA$umbrellaIndex, 
                                                weights = finalTableA$N,
                                                toReturn = "L"),
                          UCIIndex = weightedCI(finalTableA$umbrellaIndex, 
                                                weights = finalTableA$N, 
                                                toReturn = "U"))

finalTablePlotDT <- rbind(finalTableA, finalTableF)

finalTablePlotDT[, Species := factor(Species, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                         "mixedwood", "shrub", "wetland", "average"))]
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

pal <- gg_color_hue(length(unique(finalTablePlotDT[["Species"]]))-1)

cols <- c(pal, "grey")

finalTablePlotDT[, plotPlace := fifelse(Species != "average", "All Groups", "Average")]

sps <- c("conifer", "deciduous", "generalist", "grassland", 
         "mixedwood", "shrub", "wetland", "average")

p <- ggplot(finalTablePlotDT, aes(x = Species, fill = Species)) + 
  geom_bar(aes(y = meanIndex), stat = "identity", position = position_dodge()) + 
  scale_fill_manual(values = setNames(cols,
                                      sps),
                    breaks = c("meanIndex"),
                    labels = c("Umbrella Index")) +
  scale_color_manual(values = rep("black", times = 8)) +
  geom_errorbar(aes(ymin = LCIIndex, 
                    ymax = UCIIndex,
                    color = Species), 
                position = position_dodge(0.9), size = 0.8, width = 0.25)+
  labs(x = "Landbird Species Group", y = "Index") + 
  scale_y_continuous(breaks = seq(-0.15, 0.8, by = 0.05),
                     limits = c(-0.15, 0.55), expand = c(0,0), label = function(x) 
                       format(round(x, 2), scientific = FALSE)) +
  geom_hline(yintercept = finalTablePlotDT[Species == "average", meanIndex], linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dotdash") +
  theme_classic() +
  theme(legend.position = "none",
        legend.title = element_blank(), 
        axis.text = element_text(size = 12))
p
ggsave(device = "png", filename = file.path(Paths$outputPath, 
                                            "groupsAveraged.png"), 
       width = 9, height = 6)
drive_upload(file.path(Paths$outputPath, "groupsAveraged.png"), 
             as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))


########################################
# TABLE  2: Linear model through time  #
########################################

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
    return(data.table(Species = sp,
                      ProportionAreaChosen = P,
                      umbrellaIndex = lmUdir*lmUsig))
  }))
}))

round((table(throughTime$umbrellaIndex)/NROW(throughTime))*100, 1)

throughTimeTable <- rbindlist(lapply(unique(DTtime[["Species"]]), function(sp){
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
    lmUf <- data.table(Species = rep(sp, 2),
                       ProportionAreaChosen = rep(P, 2))
    lmUf <- cbind(lmUf, as.data.table(lmU$coefficients))
    return(lmUf)
  }))
}))
write.csv(throughTimeTable, file.path(Paths$outputPath, "throughTimeTable.csv"))
drive_upload(file.path(Paths$outputPath, "throughTimeTable.csv"), as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))

########################################
#  PLOTS  EXTRA:  Individual Species   #
########################################

# Still need to adjust the colour based on group and
# look through species
  
cols <- gg_color_hue(length(unique(plot1Table[["Habitat"]])))

sps <- c("conifer", "deciduous", "generalist", "grassland", 
         "mixedwood", "shrub", "wetland")
Require("ggplot2")

setkey(plot1Table, "Habitat", "umbrellaMean")

plot1 <- ggplot(data = plot1Table, aes(y = Species,
                                       group = Habitat,
                                       color = Habitat,
                                       fill = Habitat)) +
  geom_col(aes(x = umbrellaMean)) + 
  facet_grid(ProportionAreaChosen ~ Year) + #, scales = "free_x") + # Only if bird grouping is done!
  scale_fill_manual(values = setNames(cols,
                                      sps),
                    breaks = c("umbrellaMean"),
                    labels = c("Umbrella Index")) +
  scale_color_manual(values = setNames(cols,
                                       sps),
                     breaks = c("umbrellaMean"),
                     labels = c("Umbrella Index")) +
  geom_errorbar(aes(xmin = umbrellaLCI,
                    xmax = umbrellaUCI,
                    color = "black"), 
                position = position_dodge(0.9), size = 0.8, width = 0.25)+
  labs(x = "Landbird Species Group", y = "Index") + 
  coord_cartesian(xlim = c(-1, 2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank(), 
        axis.text = element_text(size = 12))
plot1
ggsave(device = "png", filename = file.path(folderColonization, 
                                            "affectedAreaByClimateChangePerEffect.png"), 
       width = 8, height = 11)


###########################################################
#                                                         #
# Identifying the groupping with best values per species  # 
#                                                         #
###########################################################

DT4 <- Copy(DT3)
DT4[, meanPIC := mean(ProportionIndividualsConserved), by = c("Species", "PrioritizedFor")]
DT5 <- unique(DT4[, c("Species", "Habitat", "PrioritizedFor", "meanPIC")])
DT4[, suggestedClass := max(ProportionIndividualsConserved), by = c("Species", "PrioritizedFor")]
DT5 <- dcast(DT4, 
              Species + Habitat ~ PrioritizedFor,
              value.var = "meanPIC", fun = sum)

DT5[, suggestedClass := colnames(.SD)[max.col(.SD, ties.method = "first")], 
     .SDcols = c(unique(DTx1[["Habitat"]]), "random", "caribou")]
DT5[, misclassified := Habitat != suggestedClass]
unique(DTx2[misclassified == TRUE, c("Species", "Habitat", "suggestedClass")])


