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

climateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
runs <- paste0("run", 1:5)
specificScenarios <- data.table(expand.grid(as.character(as.roman(32:41)), 
                                            c("shrub", "generalist", "deciduous",
                                              "conifer", "wetland", "grassland")))
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

Fig1_numbers[, ProportionAreaChosen  := round(ProportionAreaChosen , 2)]

# First thing to do, is to group the birds into their original groups. We then recalculate the
# ProportionIndividualsConserved by summing for all species the TotalIndividualsInitial and the 
# TotalIndividualsCurrent

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

DT <- Copy(Fig1_numbers2)
DT[, c("TotalAreaHaNWT", "TotalAreaChosen", "Scenario") := NULL]

DT[, ProportionIndividualsConserved := sum(TotalIndividualsCurrent)/sum(TotalIndividualsInitial), 
   by = c("Year", "ClimateScenario", "Run", "PrioritizedFor", 
          "ProportionAreaChosen", "Habitat")]

DT[, c("Species", "TotalIndividualsInitial", 
       "TotalIndividualsCurrent") := NULL]

DT <- unique(DT)

DT[is.na(Habitat), Habitat := "caribou"]

names(DT)[names(DT) == "Habitat"] <- "Species"

# DEALING WITH REPLICATES (Run) and CLIMATE SCENARIOS
#### Summarize for each  climate scenario by pulling the runs and CS together for each Species,
#### Year, Scenario, PrioritizedFor. We do the calculation over the ProportionIndividualsConserved 
#### (mean, max and min)

# YOU CAN'T MIX MATCH SIMULATIONS IN TERMS OF INDIVIDUALS BECAUSE THE CONDITIONS IN THE END ARE 
# HIGHLY CORRELATED WITH THE CONDITIONS IN THE BEGINNING! THEREFORE, TO ASSESS UNCERTAINTY, I NEED
# TO WORK ON THE FINAL PROPORTION AND FROM THERE DERIVE MIN, MEAN AND MAX.

DT[, ProportionIndividualsConservedMean := mean(ProportionIndividualsConserved), 
    by = c("Species", "Year", "PrioritizedFor", "ProportionAreaChosen")]
DT[, ProportionIndividualsConservedMin := min(ProportionIndividualsConserved), 
    by = c("Species", "Year", "PrioritizedFor", "ProportionAreaChosen")]
DT[, ProportionIndividualsConservedMax := max(ProportionIndividualsConserved), 
    by = c("Species", "Year", "PrioritizedFor","ProportionAreaChosen")]

# Exclude the columns that are different and simplify
DT[, c("Run", "ClimateScenario", "ProportionIndividualsConserved") := NULL]
DT <- unique(DT)

boo_birdGroups <- unique(DT[Species %in% unique(birdsGroupingTable[["Habitat"]]),])

# Delete the species that are NOT the ones we need
boo_birdGroups <- unique(boo_birdGroups[Species == PrioritizedFor | 
                                     PrioritizedFor %in% c("caribou", "random"), ])

Require("ggplot2")
landbirds_plot <- ggplot(data = boo_birdGroups, aes(x = ProportionAreaChosen, 
                                          group = PrioritizedFor,
                                          color = PrioritizedFor)) +
  geom_line(aes(y = ProportionIndividualsConservedMean), size = 1.3) + 
  geom_ribbon(aes(ymin = ProportionIndividualsConservedMin,
                  ymax = ProportionIndividualsConservedMax,
                  group = PrioritizedFor,
                  fill = PrioritizedFor
                  ), alpha = 0.1, colour = NA) +
  facet_grid(cols = vars(Year), rows = vars(Species), scales = "free_y") +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(breaks= seq(0.05, 0.95, by = 0.1),
                     limits = c(0, 1), expand = c(0,0)) +
  xlab("Proportion of total area protected") + 
  ylab("Proportion of predicted habitat suitability conserved")
  
landbirds_plot

################
#              #
#   FIGURE 2   #
#              #
################

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

Fig2_numbers <- dcast(DT2[Species != "caribou", ], 
                         Species + Year + Run + ClimateScenario + ProportionAreaChosen ~ PrioritizedFor, 
                         value.var = c("ProportionIndividualsConserved"))
Fig2_numbers[, umbrellaIndex := caribou - random]

for (sp in unique(Fig2_numbers[["Species"]])){
  Fig2_numbers[Species == sp, comparisonIndex := get(sp) - random]
}

toKeep <- c("Species", "Year", "Run", "ClimateScenario", 
            "ProportionAreaChosen", "umbrellaIndex", 
            "comparisonIndex")

Fig2_numbers2 <- Fig2_numbers[, ..toKeep]

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

if (any(length(unique(Fig2_numbers2[["Run"]])) > 1,
        length(unique(Fig2_numbers2[["ClimateScenario"]])) > 1)){
  
  Fig2_numbers2[, `:=` (comparisonIndexMean = CI(x = comparisonIndex, toReturn = "mean"), # ci = 0.95 by default
                        umbrellaIndexMean = CI(x = umbrellaIndex, toReturn = "mean"),
                        comparisonIndexLCI = CI(x = comparisonIndex, toReturn = "L"),
                        umbrellaIndexLCI = CI(x = umbrellaIndex, toReturn = "L"),
                        comparisonIndexUCI = CI(x = comparisonIndex, toReturn = "U"),
                        umbrellaIndexUCI = CI(x = umbrellaIndex, toReturn = "U")),
                by = c("Species", "Year", "ProportionAreaChosen")]
} else {
  warning(paste0("ATTENTION: Your table has no variation (possibly only",
                 " one replicate and climate scenario)"), 
          immediate. = TRUE)
  Fig2_numbers2[, `:=` (comparisonIndexMean = comparisonIndex,
                        umbrellaIndexMean = umbrellaIndex,
                        comparisonIndexLCI = comparisonIndex,
                        umbrellaIndexLCI = umbrellaIndex,
                        comparisonIndexUCI = comparisonIndex,
                        umbrellaIndexUCI = umbrellaIndex),
                by = c("Species", "Year", "ProportionAreaChosen")]
}

Fig2_numbers2 <- unique(Fig2_numbers2[, c("Run", "ClimateScenario", 
                                          "umbrellaIndex", "comparisonIndex") := NULL])

boo_birdGroupsG1 <- melt(data = Fig2_numbers2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndexMean", "comparisonIndexMean"))
names(boo_birdGroupsG1)[names(boo_birdGroupsG1) == "variable"] <- "Index"
boo_birdGroupsG1[Index == "comparisonIndexMean", Index := "comparisonIndex"]
boo_birdGroupsG1[Index == "umbrellaIndexMean", Index := "umbrellaIndex"]

boo_birdGroupsG2 <- melt(data = Fig2_numbers2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndexLCI", "comparisonIndexLCI"))
names(boo_birdGroupsG2)[names(boo_birdGroupsG2) == "variable"] <- "Index"
names(boo_birdGroupsG2)[names(boo_birdGroupsG2) == "value"] <- "valueMin"
boo_birdGroupsG2[Index == "umbrellaIndexLCI", Index := "umbrellaIndex"]
boo_birdGroupsG2[Index == "comparisonIndexLCI", Index := "comparisonIndex"]

boo_birdGroupsG3 <- melt(data = Fig2_numbers2, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"),
                         measure.vars = c("umbrellaIndexUCI", "comparisonIndexUCI"))
names(boo_birdGroupsG3)[names(boo_birdGroupsG3) == "variable"] <- "Index"
names(boo_birdGroupsG3)[names(boo_birdGroupsG3) == "value"] <- "valueMax"
boo_birdGroupsG3[Index == "umbrellaIndexUCI", Index := "umbrellaIndex"]
boo_birdGroupsG3[Index == "comparisonIndexUCI", Index := "comparisonIndex"]

boo_birds <- merge(boo_birdGroupsG1, boo_birdGroupsG2, 
                   by = c("Species", "Year", "ProportionAreaChosen", "Index"))
boo_birds <- merge(boo_birds, boo_birdGroupsG3, 
                   by = c("Species", "Year", "ProportionAreaChosen", "Index"))


index_plot <- ggplot(data = boo_birds, aes(x = ProportionAreaChosen)) +
  geom_line(aes(y = value, color = Index), size = 1.3) +
  facet_grid(cols = vars(Year), rows = vars(Species), scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = valueMin,
                  ymax = valueMax,
                  fill = Index
  ), alpha = 0.3, color = NA, show.legend = F) +
  scale_x_continuous(breaks = seq(0.05, 0.95, by = 0.1),
                     limits = c(0, 1), expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-0.2, 0.4, by = 0.1),
  #                    limits = c(-0.2, 0.4), expand = c(0,0)) +
  xlab("Proportion of total area protected") + 
  ylab("Difference in proportion of forecasted habitat suitability conserved to randomly protected areas") +
  scale_colour_discrete(name  = "Index",
                        breaks = c("umbrellaIndex", "comparisonIndex"),
                        labels = c("Umbrella", "Reference")) +
  theme_bw() +
  theme(legend.position = "bottom")

index_plot


############################

#################### WHEN WE PUT YEAR IN X AXIS
#################### 

index_plot2 <- ggplot(data = boo_birds, aes(x = Year)) +
  geom_line(aes(y = value, color = Index), size = 1.3) +
  facet_grid(cols = vars(ProportionAreaChosen), rows = vars(Species), scales = "free_y") +
  geom_hline(yintercept = 0) +
  geom_ribbon(aes(ymin = valueMin,
                  ymax = valueMax,
                  fill = Index
  ), alpha = 0.3, color = NA, show.legend = F) +
  # scale_x_continuous(breaks = seq(0.05, 0.95, by = 0.1),
  #                    limits = c(0, 1), expand = c(0,0)) +
  # scale_y_continuous(breaks = seq(-0.2, 0.4, by = 0.1),
  #                    limits = c(-0.2, 0.4), expand = c(0,0)) +
  xlab("Proportion of total area protected") + 
  ylab("Difference in proportion of forecasted habitat suitability conserved to randomly protected areas") +
  scale_colour_discrete(name  = "Index",
                        breaks = c("umbrellaIndex", "comparisonIndex"),
                        labels = c("Umbrella", "Reference")) +
  theme_bw() +
  theme(legend.position = "bottom")

index_plot2

###########################

################
#              #
#   FIGURE 3   #
#              #
################


DT3 <- Copy(Fig1_numbers2)
DT3[, c("TotalAreaHaNWT", "TotalAreaChosen", "Scenario") := NULL]

DT3[, ProportionIndividualsConserved := sum(TotalIndividualsCurrent)/sum(TotalIndividualsInitial), 
    by = c("Year", "ClimateScenario", "Run", "PrioritizedFor", 
           "ProportionAreaChosen", "Habitat")]

DT3[, c("Species", "TotalIndividualsInitial", 
        "TotalIndividualsCurrent") := NULL]

DT3 <- unique(DT3)

DT3[is.na(Habitat), Habitat := "caribou"]

names(DT3)[names(DT3) == "Habitat"] <- "Species"

Fig3_numbers <- dcast(DT3[Species != "caribou", ], 
                      Species + Year + Run + ClimateScenario + ProportionAreaChosen ~ PrioritizedFor, 
                      value.var = c("ProportionIndividualsConserved"))

# This gives me the proportion of how much better (+) or worse (-) umbrella and reference are doing in
# comparison to random, and how much umbrella is doing in comparison to reference
Fig3_numbers[, umbrellaRandom := (caribou/random)]
for (sp in unique(Fig3_numbers[["Species"]])){
  Fig3_numbers[Species == sp, referenceRandom := (get(sp)/random)]
  Fig3_numbers[Species == sp, umbrellaReference := (caribou/get(sp))]
}

# Fig3_numbers[, umbrellaRandom := (caribou/random)-1]
# for (sp in unique(Fig3_numbers[["Species"]])){
#   Fig3_numbers[Species == sp, referenceRandom := (get(sp)/random)-1]
#   Fig3_numbers[Species == sp, umbrellaReference := (caribou/get(sp))-1]
# }

# This is where I compare the groups statistically for differences
testTable <- rbindlist(lapply(unique(Fig3_numbers[, Species]), function(sp){
  testTable <- rbindlist(lapply(unique(Fig3_numbers[, Year]), function(Y){
    testTable <- rbindlist(lapply(unique(Fig3_numbers[, ProportionAreaChosen]), function(pp){
      DT <- Fig3_numbers[Species == sp
                         & Year == Y &
                           ProportionAreaChosen == pp, ]
      DT[, umbrellaReference_test := wilcox.test(DT[["caribou"]], DT[[sp]])$p.value]
      DT[, umbrellaRandom_test := wilcox.test(DT[["caribou"]], DT[["random"]])$p.value]
      DT[, referenceRandom_test := wilcox.test(DT[[sp]], DT[["random"]])$p.value]
      return(DT)
    }))
  }))
}))

testTable[, c("Run", "ClimateScenario", "caribou", "random", 
              unique(testTable[["Species"]])) := NULL]

testTable[, umbrellaReferenceMean := mean(umbrellaReference), 
              by = c("Species", "Year", "ProportionAreaChosen")]
testTable[, referenceRandomMean := mean(referenceRandom), 
              by = c("Species", "Year", "ProportionAreaChosen")]
testTable[, umbrellaRandomMean := mean(umbrellaRandom), 
              by = c("Species", "Year", "ProportionAreaChosen")]

testTable[, umbrellaReferenceSD := sd(umbrellaReference), 
          by = c("Species", "Year", "ProportionAreaChosen")]
testTable[, referenceRandomSD := sd(referenceRandom), 
          by = c("Species", "Year", "ProportionAreaChosen")]
testTable[, umbrellaRandomSD := sd(umbrellaRandom), 
          by = c("Species", "Year", "ProportionAreaChosen")]

testTable[, c("umbrellaRandom", "referenceRandom", "umbrellaReference") := NULL]

testTable <- unique(testTable)

default <- c("Species", "Year", "ProportionAreaChosen")
Mean  <- c(default, "umbrellaReferenceMean", "referenceRandomMean", "umbrellaRandomMean")
Test <- c(default, "umbrellaReference_test", "umbrellaRandom_test", "referenceRandom_test")
SD <- c(default, "umbrellaReferenceSD", "referenceRandomSD", "umbrellaRandomSD")

meanDT <- testTable[, ..Mean]
meanDT <- melt(data = meanDT, 
                         id.vars = c("Species", "Year", "ProportionAreaChosen"))
names(meanDT)[names(meanDT) == "value"] <- "Mean"
meanDT[variable == "umbrellaReferenceMean", variable := "umbrellaReference"]
meanDT[variable == "umbrellaRandomMean", variable := "umbrellaRandom"]
meanDT[variable == "referenceRandomMean", variable := "referenceRandom"]

testDT <- testTable[, ..Test]
testDT <- melt(data = testDT, 
               id.vars = c("Species", "Year", "ProportionAreaChosen"))
names(testDT)[names(testDT) == "value"] <- "statTest"
testDT[variable == "umbrellaReference_test", variable := "umbrellaReference"]
testDT[variable == "umbrellaRandom_test", variable := "umbrellaRandom"]
testDT[variable == "referenceRandom_test", variable := "referenceRandom"]

sdDT <- testTable[, ..SD]
sdDT <- melt(data = sdDT, 
               id.vars = c("Species", "Year", "ProportionAreaChosen"))
names(sdDT)[names(sdDT) == "value"] <- "SD"
sdDT[variable == "umbrellaReferenceSD", variable := "umbrellaReference"]
sdDT[variable == "umbrellaRandomSD", variable := "umbrellaRandom"]
sdDT[variable == "referenceRandomSD", variable := "referenceRandom"]

Fig3_Table <- merge(meanDT, sdDT, by = c("Species", "Year", "ProportionAreaChosen", "variable"))
Fig3_Table <- merge(Fig3_Table, testDT, by = c("Species", "Year", "ProportionAreaChosen", "variable"))


Fig3_Table[, ProportionAreaChosen := factor(ProportionAreaChosen)]
DTr <- Fig3_Table[variable == "umbrellaReference",]
DTr[, pValue := fifelse(statTest < 0.05, "< 0.05", "> 0.05")]

pal <- c("forestgreen", "darkred")

index_plot3 <- ggplot(data = DTr, aes(x = Year)) +
  geom_line(aes(y = Mean), size = 1.1, show.legend = F) +
  geom_point(aes(y = Mean, group = pValue, 
                 fill = pValue, 
                 color = pValue), shape = 18, size = 3, show.legend = F) +
  scale_color_manual(name = 'p Value',
                      values = setNames(c('forestgreen','darkred'), 
                                        c("> 0.05", "< 0.05"))) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species), scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD,
                    color = pValue), size = 1.1, width = 7) +
  xlab("Time") + 
  ylab(paste0("Umbrella Index\n'Proportion of Individuals Conserved when prioritizing for caribou'/",
              "'Proportion of Individuals Conserved when prioritizing for each landbird group'")) +
  theme_bw() +
  theme(legend.position = "bottom")

index_plot3

index_plot4 <- ggplot(data = DTr, aes(x = ProportionAreaChosen, group = pValue, 
                                      fill = pValue, 
                                      color = pValue)) +
  geom_point(aes(y = Mean), shape = 18, size = 3, show.legend = F) +
  scale_color_manual(name = 'p Value',
                     values = setNames(c('forestgreen','darkred'), 
                                       c("> 0.05", "< 0.05"))) +
  facet_grid(cols = vars(Year), 
             rows = vars(Species), scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD,
                    color = pValue), size = 1.1, width = 0.6) +
  xlab("Time") + 
  ylab(paste0("Umbrella Index\n'Proportion of Individuals Conserved when prioritizing for caribou'/",
              "'Proportion of Individuals Conserved when prioritizing for each landbird group'")) +
  theme_bw() +
  theme(legend.position = "bottom")

index_plot4

##########################
### TABLE
##########################

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

comparisonTable <- DT2[Species != "caribou", ]
comparisonTable2 <- unique(comparisonTable[Species == PrioritizedFor | 
                                             PrioritizedFor %in% c("caribou", "random"), ])
comparisonTable2[, c("meanPropIndCons", 
                     "UCIPropIndCons",
                     "LCIPropIndCons") := list(mean(ProportionIndividualsConserved),
                                               CI(ProportionIndividualsConserved, toReturn = "U"),
                                               CI(ProportionIndividualsConserved, toReturn = "L")),
                 by = c("Year", "PrioritizedFor", "ProportionAreaChosen", "Species")]
comparisonTable2[, c("ClimateScenario",  "Run", "ProportionIndividualsConserved") := NULL]
comparisonTable2 <- unique(comparisonTable2)

comparisonTable3 <- dcast(comparisonTable2, 
                          Species + Year + ProportionAreaChosen ~ PrioritizedFor, 
                          value.var = c("meanPropIndCons", "UCIPropIndCons", "LCIPropIndCons"))

# Check whether 95% distribution of one overlaps the mean of the other.

for (sp in unique(comparisonTable3[["Species"]])){
  # comparisonTable3[Species == sp, overlapMeanInv := fifelse(UCIPropIndCons_caribou >= get(paste0("meanPropIndCons_", sp)) &
  #                                                          LCIPropIndCons_caribou <= get(paste0("meanPropIndCons_", sp)),
  #                                                        TRUE, FALSE)]
  comparisonTable3[Species == sp, overlapMean := fifelse(meanPropIndCons_caribou >= get(paste0("LCIPropIndCons_", sp)) &
                                                           meanPropIndCons_caribou <= get(paste0("UCIPropIndCons_", sp)),
                                                         TRUE, FALSE)]
  comparisonTable3[Species == sp, overlapCI := fifelse({get(paste0("UCIPropIndCons_", sp)) >= LCIPropIndCons_caribou &
      get(paste0("UCIPropIndCons_", sp)) <= UCIPropIndCons_caribou} |
        {get(paste0("LCIPropIndCons_", sp)) <= UCIPropIndCons_caribou &
            get(paste0("LCIPropIndCons_", sp)) >= LCIPropIndCons_caribou},
      TRUE, FALSE)]
}

toKeep <- c("Species", "Year", "ProportionAreaChosen", "overlapMean", "overlapCI")
comparisonTable4 <- comparisonTable3[, ..toKeep]
comparisonTable4[, ProportionAreaChosen := factor(ProportionAreaChosen)]

p5_DT <- merge(DTr[, c("Species", "Year", "ProportionAreaChosen", 
                       "Mean", "SD")],
               comparisonTable4, by = c("Species", "Year", "ProportionAreaChosen"))

index_plot5 <- ggplot(data = p5_DT, aes(x = Year)) +
  geom_line(aes(y = Mean), size = 1.1, show.legend = F) +
  geom_point(aes(y = Mean, group = overlapMean, 
                 fill = overlapMean, 
                 color = overlapMean), shape = 18, 
             size = 3, show.legend = F) +
  scale_color_manual(name = 'overlapping averages',
                     values = setNames(c('forestgreen','darkred'), 
                                       c(TRUE, FALSE))) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species), scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD,
                    color = overlapMean), size = 1.1, width = 7) +
  xlab("Time") + 
  ylab(paste0("Umbrella Index")) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
index_plot5

index_plot6 <- ggplot(data = p5_DT, aes(x = Year)) +
  geom_line(aes(y = Mean), size = 1.1, show.legend = F) +
  geom_point(aes(y = Mean, group = overlapCI, 
                 fill = overlapCI, 
                 color = overlapCI), shape = 18, 
             size = 3, show.legend = F) +
  scale_color_manual(name = 'overlapping CI',
                     values = setNames(c('forestgreen','darkred'), 
                                       c(TRUE, FALSE))) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species), scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD,
                    color = overlapCI), size = 1.1, width = 7) +
  xlab("Time") + 
  ylab(paste0("Umbrella Index")) +
  # \n'Proportion of Individuals Conserved when prioritizing for caribou'/",
  #             "'Proportion of Individuals Conserved when prioritizing for each landbird group'
  theme_bw() +
  theme(legend.position = "bottom")

index_plot6



############ MADE FOR JUNIOR
p7_DT <- p5_DT[Species %in% c("conifer", "deciduous") & ProportionAreaChosen %in% c(0.25, 0.75)]

index_plot7 <- ggplot(data = p7_DT, aes(x = Year)) +
  geom_line(aes(y = Mean), size = 1.1, show.legend = F) +
  geom_point(aes(y = Mean, group = overlapMean, 
                 fill = overlapMean, 
                 color = overlapMean), shape = 18, 
             size = 3, show.legend = F) +
  scale_color_manual(name = 'overlapping averages',
                     values = setNames(c('forestgreen','darkred'), 
                                       c(TRUE, FALSE))) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Species), scales = "free_y") +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD,
                    color = overlapMean), size = 1.1, width = 7) +
  xlab("Time") + 
  ylab(paste0("Umbrella Index")) +
  scale_y_continuous(breaks = seq(0.8, 1.0, by = 0.05),
                     limits = c(0.8, 1.0), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
index_plot7

library("Require")
Require("raster")
Require("data.table")
Require("ccaPP")
Require("lattice")
Require("rasterVis")
Require("viridis")
Require("maptools")

pth <- Paths$inputPath # Need to confirm this is the correct path

yearsWanted <- 2011
whichComparison <- c("decSynergy25", "decSynergy75",
                     "conSynergy25", "conSynergy75")

Fig1_maps <- lapply(yearsWanted, function(Y){
  allComps <- lapply(whichComparison, function(whichComp){
    message("Making synergy maps for year ", Y, 
            " for comparison ", whichComp)
    # Pairing of rasters
    # CARIBOU vs BIRDS
    # ALWAYS "BIRD FRIENDLY" SCENARIO FIRST!!! --> Important for plotting correctly
    comp <- switch(whichComp,
                   "decSynergy75" = c("XXXIX_deciduous", "XXIX"),
                   "decSynergy25" = c("XXXIV_deciduous", "XXIV"),
                   "conSynergy75" = c("XXXIX_conifer", "XXIX"),
                   "conSynergy25" = c("XXXIV_conifer", "XXIV")
    )
    
    fRas <- raster::stack(list.files(path = pth, full.names = TRUE, 
                                     pattern = paste0(comp[1],"_solutions_Year", Y)))
    firstRas <- raster::calc(fRas, 
                             fun = mean, filename = file.path(pth, paste0(whichComp, "_", comp[1],"_mean")),
                             format = "GTiff", overwrite = TRUE)
    sRas <- raster::stack(list.files(path = pth, full.names = TRUE, 
                                     pattern = paste0(comp[2],"_solutions_Year", Y)))
    secondRas <- raster::calc(sRas, 
                              fun = mean, filename = file.path(pth, paste0(whichComp, "_", comp[2], "_mean")),
                              format = "GTiff", overwrite = TRUE)
    
    # 2. Get only areas that are selected more than 80% of the time
    firstRas[firstRas[] >= 0.8] <- 1
    firstRas[firstRas[] < 0.8] <- 0
    secondRas[secondRas[] >= 0.8] <- 1
    secondRas[secondRas[] < 0.8] <- 0
    
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
    
    # 5. Make the pretty Map      
    secondRas[secondRas[] == 1] <- 2
    finalRas <- firstRas + secondRas
    finalRas <- ratify(finalRas)
    
    rat <- levels(finalRas)[[1]]
    
    rat$species <- c("NotSelected", ifelse(whichComp %in% c("decSynergy75", "decSynergy25"), 
                                           "deciduous associated", 
                                           "conifer associated"), 
                     "caribou", "both")
    levels(finalRas) <- rat
    ColorsTB <- data.table(levs = c("NotSelected", ifelse(whichComp %in% c("decSynergy75", "decSynergy25"), 
                                                          "deciduous associated", 
                                                          "conifer associated"), "caribou", "both"),
                           Colors = c("grey85", "darkmagenta", "forestgreen", "blue4"))
    ColorsTB <- ColorsTB[match(rat[["species"]], levs),]
    message("Making Figure 1 map for year ", Y, " scenarios ", 
            paste(whichComp, collapse = " vs "))
    
    fileNamePNG <- file.path(pth, 
                             paste0(paste(whichComp, collapse = "_"),"_priorityAreas_Year", 
                                    Y, ".png"))
    png(filename = fileNamePNG,
        width = 21, height = 29,
        units = "cm", res = 300)
    p <- levelplot(finalRas,
                   main = paste0("Spearman Correlation: ", round(corrSp, 2)),
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


########################
# FINAL PLOT AND TABLE #
########################

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
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  na.omit(y)
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

toRemove <- F
toInvert <- T

comparisonTableExtra <- lapply(unique(DTx$Species), function(sp){
  allYears <- lapply(unique(DTx$Year), function(Y){
    allAreas <- lapply(unique(DTx$ProportionAreaChosen), function(A){
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
             rows = vars(Species)) + #, scales = "free_y"
  xlab("Time") + 
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = 1) +
  ylab(paste0("Umbrella Index")) +
  coord_cartesian(ylim = c(-3, 2)) +
  # scale_y_continuous(breaks = seq(-2, 1, by = 0.2),
  #                    limits = c(-2, 1), expand = c(0,0)) +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
#Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
finalPlot

# finalPlot2 <- ggplot(data = comparisonTableExtra, aes(x = Year)) +
#   geom_line(aes(y = umbrellaMean, 
#                 group = Species,
#                 color = Species), 
#             size = 1.1, show.legend = F) +
#   geom_ribbon(aes(ymin = umbrellaLCI,
#                   ymax = umbrellaUCI,
#                   group = Species,
#                   fill = Species), alpha = 0.5, colour = NA) +
#   geom_line(aes(y = randomMean),
#                 color = "black", 
#             size = 1, show.legend = F) +
#   geom_ribbon(aes(ymin = randomLCI,
#                   ymax = randomUCI), alpha = 0.4, colour = "grey") +
#   facet_grid(cols = vars(ProportionAreaChosen), 
#              rows = vars(Species)) +
#   xlab("Time") + 
#   geom_hline(yintercept = 1) +
#   ylab(paste0("Index")) +
#   scale_y_continuous(breaks = seq(0.4, 1.1, by = 0.1),
#                      limits = c(0.3, 1.1), expand = c(0,0)) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.text = element_text(size = 12),
#         axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
# #Umbrella Index: Proportion of Individuals Conserved when prioritizing for caribou / Proportion of Individuals Conserved when prioritizing for each landbird group
# finalPlot2


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



# finalTableD <- dcast(finalTableF, formula = Species ~ variable, 
#                      value.var = c("meanIndex", "LCIIndex", "UCIIndex"))

# finalNumber <- data.table(Index = c("random", "umbrella"),
#                           Mean = c(mean(finalTableDT[["randomIndex"]]),
#                                    mean(finalTableDT[["umbrellaIndex"]])),
#                           LCI = c(CI(finalTableDT[["randomIndex"]], toReturn = "L"),
#                                   CI(finalTableDT[["umbrellaIndex"]], toReturn = "L")),
#                           UCI = c(CI(finalTableDT[["randomIndex"]], toReturn = "U"),
#                                   CI(finalTableDT[["umbrellaIndex"]], toReturn = "U")))

# finalNumberT <- dcast(finalNumber, formula = . ~ Index, 
#                      value.var = c("Mean", "LCI", "UCI"))
# names(finalNumberT) <- c("Species", "meanIndex_randomIndex", "meanIndex_umbrellaIndex", 
#                          "LCIIndex_randomIndex", "LCIIndex_umbrellaIndex", 
#                          "UCIIndex_randomIndex", "UCIIndex_umbrellaIndex")
# finalNumberT[, Species := "average"]

# finalTablePlot <- rbind(finalNumberT, finalTableD, use.names = TRUE)

# finalTablePlotMean <- melt(finalTablePlot[, c("Species", "meanIndex_randomIndex", "meanIndex_umbrellaIndex")], 
#                            id.vars = "Species")
# names(finalTablePlotMean)[names(finalTablePlotMean) == "value"] <- "meanIndex"
# finalTablePlotMean[variable == "meanIndex_randomIndex", variable := "random"]
# finalTablePlotMean[variable == "meanIndex_umbrellaIndex", variable := "umbrella"]
# 
# finalTablePlotL <- melt(finalTablePlot[, c("Species", "LCIIndex_randomIndex", "LCIIndex_umbrellaIndex")], 
#                            id.vars = "Species")
# names(finalTablePlotL)[names(finalTablePlotL) == "value"] <- "LCIIndex"
# finalTablePlotL[variable == "LCIIndex_randomIndex", variable := "random"]
# finalTablePlotL[variable == "LCIIndex_umbrellaIndex", variable := "umbrella"]
# 
# finalTablePlotU <- melt(finalTablePlot[, c("Species", "UCIIndex_randomIndex", "UCIIndex_umbrellaIndex")], 
#                            id.vars = "Species")
# names(finalTablePlotU)[names(finalTablePlotU) == "value"] <- "UCIIndex"
# finalTablePlotU[variable == "UCIIndex_randomIndex", variable := "random"]
# finalTablePlotU[variable == "UCIIndex_umbrellaIndex", variable := "umbrella"]
# 
# finalTablePlotDT <- merge(finalTablePlotMean, finalTablePlotL, by = c("Species", "variable"))
# finalTablePlotDT <- merge(finalTablePlotDT, finalTablePlotU, by = c("Species", "variable"))

