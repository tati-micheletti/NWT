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
runAUTO <- FALSE

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
appendixFolder <- "1oFDas7tzCv3pJgXrr-f7a53N7Gb9Can_"
individualSpFolder <- "1ffWDDurVMVmK1Ezlgzg4Yoos_F_l01B2"
LMspFolder <- "1r_6YwCndBg5KTbgZM-ASwrFgEDSvUhIK"
figuresFolder <- "1D8-H4h-59vD3nea9sn3aICa4SqVRulQO"
doBoxplot <- TRUE

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

fullFinalTablePath <- file.path(generalOutputs, "fullFinalTable.qs") # Make and save the darn full table!

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
        
        outPath <- file.path(generalOutputs, paste0(ClimateScenario, "_run1"), "hotspots/ms")
        # Create water template
        water <- rasterToMatch
        water[waterRaster == 1] <- NA
        # [1] 15532882 15561327 These are NA in birds but not in the RTM or Boo?
        # Legacy from resampling? I will manually exclude them. 2 pixels are not
        # relavant for the results
        water[c(15532882, 15561327)] <- NA
        
        # 1. Make the solution for random
        # 1.1. If you have the solution, load it
        randomSolution <- file.path(outPath,
                                    paste0("Solution_random_", ClimateScenario, "_",
                                           Run, "_", y,".qs"))

        if (!file.exists(randomSolution)){
          message(crayon::yellow(paste0("Creating solutions for ",
                                        Run, " for ", ClimateScenario, 
                                        " for year ", y,
                                        " for random")))
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
          
          randomSolutionMap <- file.path(outPath,
                                         paste0("Solution_random_", ClimateScenario, "_",
                                                Run,"_", y,".tif"))
          if (!file.exists(randomSolutionMap))
            writeRaster(randomSolutionStk, filename = randomSolutionMap, format = "GTiff")
          qs::qsave(x = randomTable, file = randomSolution)
          rm(randomSolutionStk)
        } else {
          message(crayon::green(paste0("Solutions for ",
                                       Run, " for ", ClimateScenario, 
                                       " for year ", y,
                                       " for random exist. Returning.")))
        }
        randomTable <- qs::qread(randomSolution)
        
        
        # 2. Make the solution for caribou
        # 2.1. If you have the solution, load it
        booSolution <- file.path(outPath,
                                 paste0("Solution_caribou_", ClimateScenario, "_",
                                        Run, "_", y,".qs"))

        if (!file.exists(booSolution)){
          message(crayon::yellow(paste0("Creating solutions for ",
                                        Run, " for ", ClimateScenario, 
                                        " for year ", y,
                                        " for caribou")))
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
        booSolutionMap <- file.path(outPath,
                                    paste0("Solution_caribou_", ClimateScenario, "_",
                                           Run, "_", y, ".tif"))
        if (!file.exists(booSolutionMap))
          writeRaster(booSolutionStk, filename = booSolutionMap, format = "GTiff")
        qs::qsave(x = booTable, file = booSolution)
        rm(booSolutionStk)
        } else {
          message(crayon::green(paste0("Solutions for ",
                                       Run, " for ", ClimateScenario, 
                                       " for year ", y,
                                       " for caribou exist. Returning.")))
        }
        booTable <- qs::qread(booSolution)
    
          # 3. Make the solution for  each species 
          # 3.1. If you have the solution, load it

        allSp <- rbindlist(lapply(X = allb, function(sp){
        
          birdFinalTable <- file.path(outPath,
                                    paste0("finalTable_", sp,"_", 
                                           ClimateScenario, "_",
                                           Run, "_", y,".qs"))

          if (any(!file.exists(birdFinalTable), 
                  overwriteFinalTables)){
            
          birdSolution <- file.path(outPath,
                                    paste0("Solution_", sp,"_", 
                                           ClimateScenario, "_",
                                           Run, "_", y,".qs"))
          message(crayon::green(paste0("Solutions for ", sp, 
                                        " ", Run, " for ", ClimateScenario, 
                                        " for ", y, " for ", sp, "(", which(sp == allb), " of ", 
                                        length(allb), ") exist. Returning.")))
          if (!file.exists(birdSolution)){
            message(crayon::yellow(paste0("Creating solutions for ", sp, 
                                          " ", Run, " for ", ClimateScenario, 
                                          " for ", y, " for ", sp, "(", which(sp == allb), " of ", 
                                          length(allb), ")")))
            
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
            birdSolutionMap <- file.path(outPath,
                                      paste0("Solution_", sp,"_", 
                                             ClimateScenario, "_",
                                             Run, "_", y, ".tif"))
            if (!file.exists(birdSolutionMap))
              writeRaster(birdSolutionStk, filename = birdSolutionMap, format = "GTiff")
            qs::qsave(x = birdTable, file = birdSolution)
            rm(birdSolutionStk)
          } else {
            message(crayon::green(paste0("Solutions for ", sp, 
                                         " ", Run, " for ", ClimateScenario, 
                                         " for ", y, " for ", sp, "(", which(sp == allb), " of ", 
                                         length(allb), ") exist. Returning.")))
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
                                          " finished for year ", y, " for ", sp, " (", which(sp == allb), " of ", 
                                          length(allb), ")")))
          } else {
            message(crayon::green(paste0("Solutions for ", sp, 
                                         " ", Run, " for ", ClimateScenario, 
                                         " for ", y, " for all birds exist. Returning.")))
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
  qs::qsave(finalTable, file = fullFinalTablePath)
  toc()
} else {
  finalTable <- qs::qread(fullFinalTablePath)
}

# EXCLUDE SPECIES THAT WERE DEEMED TO NOT HAVE GOOD MODEL FIT
spToRemove <- c("BOWA", "BANS", "PIWO", "NOFL", "BARS", "BRBL")
finalTable <- finalTable[!Species %in% spToRemove,]
length(unique(finalTable$Species)) # 71  species left!

print("Process finished!")

################
#              #
#   FIGURES    #
#              #
################

Require("lattice")
Require("rasterVis")
Require("viridis")
Require("maptools")
Require("colorspace")
Require("ggplot2")
Require("googledrive")

# First thing to do, is to group the birds into their original groups. We then recalculate the
# ProportionIndividualsConserved by summing for all species the TotalIndividualsInitial and the 
# TotalIndividualsCurrent
generalOutputs <- Paths$outputPath
birdsGroupingTable <- prepInputs(url = "https://drive.google.com/file/d/1_JN2N0JLlM_50zTSZMdTgWXP7-hNS8ua/pub?output=csv", 
                                 targetFile = "Bird Classification - birdHabitatRevisedV2.csv",
                                 destinationPath = generalOutputs, 
                                 fun = "data.table::fread", 
                                 header = TRUE)

# Simplyfying and putting the correct names
birdsGroupingTable <- birdsGroupingTable[, c("Species Code", "Habitat")]
names(birdsGroupingTable) <- c("Species", "Habitat")

# Merge the tables
finalTable2 <- merge(finalTable, 
                       birdsGroupingTable, by = "Species", all.x = TRUE)

########################
# FINAL PLOT AND TABLE #
########################

CI <- function (x, ci = 0.95, toReturn = NULL, type = "error", silent = TRUE){
  if (all(type != "quantile", is.null(toReturn)))
    stop("You need to provide either type = 'quantile' or toReturn = 'mean', 'L', or 'U'.")
  a <- mean(x)
  SD <- sd(x)
  n <- length(x)
  if (type == "error"){
    error <- qt(ci + (1 - ci)/2, df = n - 1) * SD/sqrt(n) 
  } else {
    if (type == "deviation"){
      error <- qt(ci + (1 - ci)/2, df = n - 1) * SD
    } else {
      if (type == "quantile") {
            if (!silent) print(paste0("Returning quantile ", ci*100, "%"))
            return(as.numeric(quantile(x, prob = ci)))
      } else stop("Type must be either 'error', 'deviation', 'quantile'") 
    }
  }
  if (toReturn == "mean")
    return(a) else
      if (toReturn == "L")
        return(a-error) else
          if (toReturn == "U")
            return(a+error)
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

###################################
# FIGURE 1: Individual birds plot #
###################################

# finalTable4 <- merge(finalTable, 
#                      birdsGroupingTable, by = "Species", all.x = TRUE)
DTd <- dcast(finalTable2,
             Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
             value.var = "ProportionIndividualsConserved")

finalTable4 <- rbindlist(lapply(unique(DTd$Species), function(sp){
  allYears <- rbindlist(lapply(unique(DTd$Year), function(Y){
    allAreas <- rbindlist(lapply(unique(DTd$ProportionAreaChosen), function(A){
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "Habitat", "caribou", sp, "random")
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

DTx1 <- dcast(finalTable2[Species != "caribou",], 
              Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
              value.var = "ProportionIndividualsConserved")

toRemove <- T
toInvert <- F

source("~/projects/NWT/posthocFunctions/makeFigure3.R")

# plotYear1 <- makeFigure3(DTx1, yearsSelected = 2011)
# plotYear2 <- makeFigure3(DTx1, yearsSelected = 2091)
# 
# plotArea1 <- makeFigure3(DTx1, areaChosen = as.character(0.15)) # Current 
# plotArea2 <- makeFigure3(DTx1, areaChosen = as.character(0.45)) # Future
# 
# plotFig <- makeFigure3.1(DTx1, yearsSelected = c(2011, 2091), 
#                                   areaChosen = as.character(c(0.15, 0.45)), 
#                          grid = TRUE)
# 
# plotFig <- makeFigure3.2(DTx1, yearsSelected = c(2011, 2011), 
#                          areaChosen = as.character(c(0.15, 0.95)), 
#                          grid = TRUE)
# 
# plotFigTime <- makeFigure3.2(DTx1, 
#                         yearsSelected = c(2091, 2091, 2091), 
#                         climateModel = c("INM-CM4", "CCSM4", "CanESM2"),
#                         areaChosen = as.character(0.15),
#                         grid = TRUE)
# 

plotFig <- makeFigure3.3(finalTable4)

# plotFigPanel <- makeFigure3.4(finalTable4, 
#                               uploadTo = figuresFolder,
#                               yearsSelected = c(2011, 2031, 2091), 
#                               areaChosen = as.character(c(0.15, 0.35, 0.45)), 
#                               grid = TRUE)

# filNam <- file.path(Paths$outputPath, "individualSpecies.png")
# filNam <- file.path(Paths$outputPath, "indSp_CurFut.png")
# 
# ggsave(device = "png", filename = filNam, 
#        width = 8, height = 16)
# drive_upload(filNam, 
#              as_id("1Xl9YIqN3GeEXZj_8yewd9zvZ3Gw3Qoca"))

###################################
#  FIGURE 2: General birds plot   #
###################################

umbrellaTable <- rbindlist(lapply(unique(finalTable2$Species), function(sp){
  DT <- dcast(finalTable2, 
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
})) # Not needed anymore! Fixed

# Merge the tables
finalTable3 <- merge(finalTable, 
                     birdsGroupingTable, by = "Species", all.x = TRUE)

finalTable3[, c("Species", "ProportionIndividualsConserved") := NULL]

finalTable3[PrioritizedFor != "caribou" & 
              PrioritizedFor != "random", PrioritizedFor := "bird"]

finalTable3[, c("allIndivGroupCurrent",
                "allIndivGroupInitial") := list(sum(TotalIndividualsCurrent),
                                                sum(TotalIndividualsInitial)), by = c("ClimateScenario", "Run", 
                                                                           "Year", "PrioritizedFor",
                                                                           "ProportionAreaChosen",
                                                                           "Habitat")]

finalTable3[, c("TotalIndividualsInitial", "TotalIndividualsCurrent") := NULL]

finalTable3 <- unique(finalTable3)
finalTable3[, "ProportionIndividualsConserved" := allIndivGroupCurrent/allIndivGroupInitial]

# finalTable4 <- finalTable3[, c("ClimateScenario", "Run", "Year", "") := NULL]

DTx <- dcast(finalTable3, 
             ClimateScenario + Run + Year + ProportionAreaChosen + Habitat ~ PrioritizedFor,
             value.var = "ProportionIndividualsConserved")

comparisonTableExtra <- rbindlist(lapply(unique(DTx$Habitat), function(sp){
  allYears <- rbindlist(lapply(unique(DTx$Year), function(Y){
    allAreas <- rbindlist(lapply(unique(DTx$ProportionAreaChosen), function(A){
      message(paste0("Processing ", A, " for ", sp))
      toKeep <- c("Habitat", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "caribou", "random", "bird")
      DT <- DTx[Habitat == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
      boo <- DT[["caribou"]]
      low <- DT[["random"]]
      upp <- DT[["bird"]]
      umbrellaIndex <- numeric(NROW(DT))
      for (i in 1:length(umbrellaIndex)){
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
      }
      
      DT[, c("umbrellaMean",
             "umbrellaLCI",
             "umbrellaUCI") := list(mean(umbrellaIndex),
                                    CI(umbrellaIndex, toReturn = "L"),
                                    CI(umbrellaIndex, toReturn = "U"))]
      DT <- unique(DT[, c("Habitat", "Year", "ProportionAreaChosen", 
                          "umbrellaMean", "umbrellaLCI", "umbrellaUCI")])
      return(DT)
    }))
    return(allAreas)
  }))
  return(allYears)
}))

finalPlot <- ggplot(data = comparisonTableExtra, aes(x = Year)) +
  geom_line(aes(y = umbrellaMean, 
                group = Habitat,
                color = Habitat), 
            size = 1.5, show.legend = F) +
  geom_ribbon(aes(ymin = umbrellaLCI,
                  ymax = umbrellaUCI,
                  group = Habitat,
                  fill = Habitat), alpha = 0.5, colour = NA) +
  facet_grid(cols = vars(ProportionAreaChosen), 
             rows = vars(Habitat), scales = "free_y") + #, scales = "free_y"
  xlab("Time") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_ribbon(aes(ymin = 0, ymax = 1, fill = Habitat), 
              stat = "identity", alpha = 0.1) +
  ylab(paste0("Umbrella Index")) +
  coord_cartesian(ylim = c(-2, 1)) +
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
             as_id(figuresFolder))

###################################
# FIGURE 3: Averaged across time  #
###################################

make2scenarios <- FALSE
makeIndividualProportionArea <- FALSE # Already made

finalTable4 <- merge(finalTable, 
                     birdsGroupingTable, by = "Species", all.x = TRUE)
DTd <- dcast(finalTable4,
                     Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
                     value.var = "ProportionIndividualsConserved")

finalTable4 <- rbindlist(lapply(unique(DTd$Species), function(sp){
  allYears <- rbindlist(lapply(unique(DTd$Year), function(Y){
    allAreas <- rbindlist(lapply(unique(DTd$ProportionAreaChosen), function(A){
      toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                  "Habitat", "caribou", sp, "random")
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

finalTable5 <- Copy(finalTable4)
finalTable5 <- finalTable5[, c("Species", "umbrellaIndex")]

finalTableDT <- data.table(finalTable5)
finalTableM <- melt(finalTableDT, id.vars = "Species", 
                    measure.vars = "umbrellaIndex")

finalTableM <- merge(finalTableM,
                     birdsGroupingTable, by = "Species", all.x = TRUE)

finalTableM[, Species := NULL]
names(finalTableM)[names(finalTableM) == "Habitat"] <- "Species"
finalTableF <- finalTableM[, c("meanIndex",
                               "LCIIndex",
                               "UCIIndex") := list(mean(value),
                                                   CI(value, toReturn = "L", type = "deviation"),
                                                   CI(value, toReturn = "U", type = "deviation")), 
                           by = c("Species", "variable")]
finalTableF <- unique(finalTableF[, c("Species", 
                                      "meanIndex", "LCIIndex", "UCIIndex")])

# General Average
# First we need to weight the average to the number of species
if (doBoxplot){
finalTableSpecies <- Copy(finalTable4)
finalTableSpecies[, c("Run", "ClimateScenario", "Year") := NULL]
finalTableSpecies <- merge(finalTableSpecies,
                           birdsGroupingTable, by = "Species", all.x = TRUE)
finalTableSpecies[, Species := NULL]
finalTableSpecies2 <- copy(finalTableSpecies)
names(finalTableSpecies2) <- c("proportionOfArea", "umbrellaIndex", "Species")

finalTableSpecies3 <- copy(finalTableSpecies2)
finalTableSpecies3[, Species := "average"]

finalTableSpecies2 <- rbind(finalTableSpecies2, finalTableSpecies3)
finalTableSpecies2[, Species := factor(Species, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                         "mixedwood", "shrub", "wetland", "average"))]

pal <- gg_color_hue(length(unique(finalTableSpecies2[["Species"]]))-1)
cols <- c(pal, "grey")
sps <- c("conifer", "deciduous", "generalist", "grassland", 
         "mixedwood", "shrub", "wetland", "average")
# finalTablePlotDT[, plotPlace := fifelse(Species != "average", "All Groups", "Average")]

if (make2scenarios){
  pB <- ggplot(finalTableSpecies2[proportionOfArea %in% as.character(c(0.15, 0.45)), ], 
               aes(x = Species, y = umbrellaIndex, fill = Species)) +
    geom_boxplot() +
    theme(legend.position="bottom") +
    scale_fill_manual(values = setNames(cols,
                                        sps),
                      breaks = c("meanIndex"),
                      labels = c("Umbrella Index")) +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_blank(), 
          axis.text = element_text(size = 12)) +
    geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
    facet_grid(proportionOfArea ~ .)
  
  pB
  nm <- file.path(Paths$outputPath, 
                  "groupsAveraged_boxplot.png")
  ggsave(device = "png", filename = nm, 
         width = 9, height = 6)
  drive_upload(nm, as_id(figuresFolder))
} else {
  pB <- ggplot(finalTableSpecies2, 
               aes(x = Species, y = umbrellaIndex, fill = Species)) +
    geom_boxplot() +
    theme(legend.position="bottom") +
    scale_fill_manual(values = setNames(cols,
                                        sps),
                      breaks = c("meanIndex"),
                      labels = c("Umbrella Index")) +
    theme_classic() +
    theme(legend.position = "none",
          legend.title = element_blank(), 
          axis.text = element_text(size = 12)) +
    geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
    labs(x = "Landbird Habitat Group", y = "Umbrella Index")
  
  pB
  nm <- file.path(Paths$outputPath, 
                  "groupsAveraged_boxplotAll.png")
  ggsave(device = "png", filename = nm, 
         width = 9, height = 6)
  drive_upload(nm, as_id(figuresFolder))
}

# Save each one individually for the book
if (makeIndividualProportionArea) {
  lapply(unique(finalTableSpecies2$proportionOfArea), function(scen){
    pB <- ggplot(finalTableSpecies2[proportionOfArea %in% scen, ], 
                 aes(x = Species, y = umbrellaIndex, fill = Species)) +
      geom_boxplot() +
      theme(legend.position="bottom") +
      scale_fill_manual(values = setNames(cols,
                                          sps),
                        breaks = c("meanIndex"),
                        labels = c("Umbrella Index")) +
      theme_classic() +
      theme(legend.position = "none",
            legend.title = element_blank(), 
            axis.text = element_text(size = 12)) +
      geom_hline(yintercept = 0, color = "darkgrey", linetype = "dashed") +
      facet_grid(proportionOfArea ~ .)
    
    pB
    nm <- file.path(Paths$outputPath, 
                    paste0("groupsAveraged_", scen,"_boxplot.png"))
    ggsave(device = "png", filename = nm, 
           width = 9, height = 6)
    drive_upload(nm, as_id(figuresFolder))
  })
 } 
} else {
  numberSpecies <- birdsGroupingTable[, .N, by = "Habitat"]
  finalTableA <- merge(finalTableSpecies, numberSpecies, by = "Habitat")
  weightedCI <- function(x, weights, 
                         conf.level = 0.95,
                         toReturn = "L",
                         type = "deviation") {
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
    if (type == "error"){
      LCI <- min(cint * stderr)
      UCI <- max(cint * stderr)
    } else {
      LCI <- mx-sqrt(vx)
      UCI <- mx+sqrt(vx)
    }
    if (toReturn == "L")
      return(LCI) else return(UCI)
  }
  
  finalTableA <- data.table(Species = "average", 
                            meanIndex = weighted.mean(x = finalTableA$umbrellaIndex, 
                                                      w = finalTableA$N),
                            LCIIndex = weightedCI(x = finalTableA$umbrellaIndex, 
                                                  weights = finalTableA$N,
                                                  toReturn = "L"),
                            UCIIndex = weightedCI(x = finalTableA$umbrellaIndex, 
                                                  weights = finalTableA$N, 
                                                  toReturn = "U"))
  
  finalTablePlotDT <- rbind(finalTableA, finalTableF)
  
  finalTablePlotDT[, Species := factor(Species, levels = c("conifer", "deciduous", "generalist", "grassland", 
                                                           "mixedwood", "shrub", "wetland", "average"))]
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
    labs(x = "Landbird Species Group", y = "Umbrella index") + 
    scale_y_continuous(breaks = seq(-1.6, 1, by = 0.05),
                       limits = c(-1.6, 1), expand = c(0,0), label = function(x) 
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
}

# Detecting range of results
finalTable4B <- Copy(finalTableSpecies2)
setkey(finalTable4B, "umbrellaIndex")
head(finalTable4B)
tail(finalTable4B)

## Table with higher and lower
tbHL <- copy(comparisonTableExtra) 
setkey(tbHL, "umbrellaMean")
tbHL[, "umbrellaSide" := fifelse(umbrellaMean < 0, "Negative", "Positive")]
round((table(tbHL$umbrellaSide)/NROW(tbHL))*100, 1)
# Negative Positive Umbrella mean
# 72.3     27.7 
# This is the average of each species group considering each area and each year 

#######################################################
# TABLE  2: Linear model through time groups detailed #
#######################################################

DTtime <- Copy(finalTable4)
DTtime <- merge(DTtime,
                birdsGroupingTable, by = "Species", all.x = TRUE)
DTtime[, Species := NULL]

throughTimeGroupsD <- rbindlist(lapply(unique(DTtime[["Habitat"]]), function(sp){
  allProportions <- rbindlist(lapply(unique(DTtime[["ProportionAreaChosen"]]), function(P){
    DT <- DTtime[Habitat == sp & ProportionAreaChosen == P, ]
    Mod <- lm(DT, formula = umbrellaIndex ~ Year)
    lmU <- summary(Mod)
      lBound <- lmU$coefficients["Year", "Estimate"] - lmU$coefficients["Year", "Std. Error"]  
      uBound <- lmU$coefficients["Year", "Estimate"] + lmU$coefficients["Year", "Std. Error"]
      # Deriving Standard Error  
      # pred <- predict(Mod, newdata = data.frame(Year = mean(DT[["Year"]])), se.fit = T)
        # SD <- pred[["se.fit"]]
        # lBound <- lmU$coefficients["Year", "Estimate"] - SD  
        # uBound <- lmU$coefficients["Year", "Estimate"] + SD 
    Dir <- ifelse(lBound < 0 & uBound > 0, 
                  0, ifelse(lmU$coefficients["Year", "Estimate"] < 0,
                            -1, 1))
    return(data.table(Habitat = sp,
                      ProportionAreaChosen = P,
                      umbrellaIndex = Dir))
  }))
}))

round((table(throughTimeGroupsD$umbrellaIndex)/NROW(throughTimeGroupsD))*100, 1)

throughTimeTableGroupsD <- rbindlist(lapply(unique(DTtime[["Habitat"]]), function(sp){
  allProportions <- rbindlist(lapply(unique(DTtime[["ProportionAreaChosen"]]), function(P){
    DT <- DTtime[Habitat == sp & ProportionAreaChosen == P,]
    lmU <- summary(lm(DT, formula = umbrellaIndex ~ Year))
    lmUf <- data.table(Habitat = c(sp, NA),
                       ProportionAreaChosen = c(P, NA),
                       Coefficient = c("Intercept", "Year"))
    lBound <- lmU$coefficients["Year", "Estimate"] - lmU$coefficients["Year", "Std. Error"]  
    uBound <- lmU$coefficients["Year", "Estimate"] + lmU$coefficients["Year", "Std. Error"]
    
    Trend <- ifelse(lBound < 0 & uBound > 0, 
                    "Stable", ifelse(lmU$coefficients["Year", "Estimate"] < 0,
                              "Declining", "Increasing"))
    lmUf <- cbind(lmUf, as.data.table(lmU$coefficients), data.table(Trend = c(Trend, NA)))
    return(lmUf)
  }))
}))

summGD <- table(na.omit(throughTimeTableGroupsD[["Trend"]]))

write.csv(throughTimeTableGroupsD, file.path(Paths$outputPath, "throughTimeTableGroupsD.csv"))
drive_upload(file.path(Paths$outputPath, "throughTimeTableGroupsD.csv"), as_id(figuresFolder))

############################################################
# TABLE  A4A: Linear model through time individual species #
############################################################

DTtime2 <- Copy(finalTable4)

throughTimeSD <- rbindlist(lapply(unique(DTtime2[["Species"]]), function(sp){
  allProportions <- rbindlist(lapply(unique(DTtime2[["ProportionAreaChosen"]]), function(P){
    DT <- DTtime2[Species == sp & ProportionAreaChosen == P, ]
    Mod <- lm(DT, formula = umbrellaIndex ~ Year)
    lmU <- summary(Mod)
    lBound <- lmU$coefficients["Year", "Estimate"] - lmU$coefficients["Year", "Std. Error"]  
    uBound <- lmU$coefficients["Year", "Estimate"] + lmU$coefficients["Year", "Std. Error"]
    Dir <- ifelse(lBound < 0 & uBound > 0, 
                  0, ifelse(lmU$coefficients["Year", "Estimate"] < 0,
                            -1, 1))
    return(data.table(Species = sp,
                      ProportionAreaChosen = P,
                      umbrellaIndex = Dir))
  }))
}))

round((table(throughTimeSD$umbrellaIndex)/NROW(throughTimeSD))*100, 1)

throughTimeTableSD <- rbindlist(lapply(unique(DTtime2[["Species"]]), function(sp){
  allProportions <- rbindlist(lapply(unique(DTtime2[["ProportionAreaChosen"]]), function(P){
    DT <- DTtime2[Species == sp & ProportionAreaChosen == P,]
    lmU <- summary(lm(DT, formula = umbrellaIndex ~ Year))
    lmUf <- data.table(Species = c(sp, NA),
                       ProportionAreaChosen = c(P, NA),
                       Coefficient = c("Intercept", "Year"))
    lBound <- lmU$coefficients["Year", "Estimate"] - lmU$coefficients["Year", "Std. Error"]  
    uBound <- lmU$coefficients["Year", "Estimate"] + lmU$coefficients["Year", "Std. Error"]
    
    Trend <- ifelse(lBound < 0 & uBound > 0, 
                    "Stable", ifelse(lmU$coefficients["Year", "Estimate"] < 0,
                                     "Declining", "Increasing"))
    lmUf <- cbind(lmUf, as.data.table(lmU$coefficients), data.table(Trend = c(Trend, NA)))
  }))
}))

summSD <- table(na.omit(throughTimeTableSD[["Trend"]]))

write.csv(throughTimeTableSD, file.path(Paths$outputPath, "throughTimeTableSD.csv"))
drive_upload(file.path(Paths$outputPath, "throughTimeTableSD.csv"), as_id(figuresFolder))

########################################
#  TABLE  GLM :   Individual Species   #
########################################

tableGLM <- Copy(finalTable4)

Require("nlme")
Require("lme4")

tableGLM[, ProportionAreaChosen := as.factor(ProportionAreaChosen)]
m3 <- lm(umbrellaIndex ~ ProportionAreaChosen + Species + ClimateScenario + Year + Run, 
         data = tableGLM)
anova(m3)
write.csv(anova(m3), file = "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/anovaLmCovariates.csv")

Require("googledrive")
drive_upload("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/anovaLmCovariates.csv", 
             as_id(figuresFolder))

allSp <- rbindlist(lapply(unique(tableGLM[["Species"]]), function(sp){
  subTableGLM <- tableGLM[Species == sp, ]
  m2 <- lm(umbrellaIndex ~ ClimateScenario + ProportionAreaChosen + Year + Run, 
           data = subTableGLM)
  B <- as.data.table(anova(m2))
  B[, Coefficient := rownames(anova(m2))]
  DT <- data.table(Species = sp,
                  ClimateScenario = ifelse(as.numeric(B[Coefficient == "ClimateScenario", 
                                                        'Pr(>F)']) < 0.05, 
                                           "Significant", 
                                           "Non-significant"),
                  ProportionAreaChosen = ifelse(as.numeric(B[Coefficient == "ProportionAreaChosen", 
                                                             'Pr(>F)']) < 0.05, 
                                                "Significant", 
                                                "Non-significant"),
                  Year = ifelse(as.numeric(B[Coefficient == "Year", 
                                             'Pr(>F)']) < 0.05, 
                                "Significant", 
                                "Non-significant"),
                  Run = ifelse(as.numeric(B[Coefficient == "Run", 
                                            'Pr(>F)']) < 0.05, 
                               "Significant", 
                               "Non-significant"))
  return(DT)
}))
# Run presents Non-significant differences for all models,
# Year: Significant for 85.3% species
round(100*(table(allSp$Year)/75), 1)
# ProportionArea: Significant for 98.7% species
round(100*(table(allSp$ProportionAreaChosen)/75), 1)
# ClimateScenario: Significant for 96% species
round(100*(table(allSp$ClimateScenario)/75), 1)


# Trend through time
trendTime <- rbindlist(lapply(unique(tableGLM[["Species"]]), function(sp){
  subTableGLM <- tableGLM[Species == sp, ]
  m2 <- lm(umbrellaIndex ~ Year, data = subTableGLM)
  B <- as.data.table(coefficients(summary(m2)))
  B[, Coefficient := rownames(anova(m2))]
  Min <- B[Coefficient == "Year", 'Estimate'] - B[Coefficient == "Year", 'Std. Error']
  Max <- B[Coefficient == "Year", 'Estimate'] + B[Coefficient == "Year", 'Std. Error']
  DT <- data.table(Species = sp,
                   trendDirection = ifelse(Max < 0,
                                           "Decreasing",
                                           ifelse(Min > 0,
                                                  "Increasing", 
                                                  "Stable")),
                   trendSignificancy = ifelse(as.numeric(B[Coefficient == "Year", 
                                                           'Pr(>|t|)']) < 0.05,
                                              "Significant",
                                              "Non-significant")
                   )
  return(DT)
}))
fl <- file.path(Paths$outputPath, "trendSummary.csv")
write.csv(trendTime, file = fl)
drive_upload(fl, as_id(figuresFolder))

############################################
#  TABLES: Groups and Individual Species   #
############################################

finalTable6 <- merge(finalTable4, 
                     birdsGroupingTable, 
                     by = "Species", 
                     all.x = TRUE)
finalTable6[, c("Run", "Year", "ClimateScenario") := NULL]
names(finalTable6)[names(finalTable6) == "ProportionAreaChosen"] <- "Scenario"

TBsp <- rbindlist(lapply(unique(finalTable6$Species), function(BIRD){
  TB <- rbindlist(lapply(unique(finalTable6$Scenario), function(SCEN){
    print(paste0("Processing ", BIRD, " for scenario ", SCEN))
    BCUI <- finalTable6[Species == BIRD & Scenario == SCEN, umbrellaIndex]
    Q1 <- as.numeric(quantile(BCUI, 0.25))
    Q3 <- as.numeric(quantile(BCUI, 0.75))
    IQR <- Q3-Q1
    DT <- data.table(Species = BIRD,
                     Habitat = unique(finalTable6[Species == BIRD & Scenario == SCEN, 
                                                  Habitat]),
                     Scenario = SCEN,
                     Mean = mean(BCUI),
                     SD = sd(BCUI),
                     Median = median(BCUI),
                     Q1 = Q1,
                     Q3 = Q3,
                     IQR = IQR,
                     Minimum = Q1-(1.5*IQR),
                     MinimumOutlier = min(BCUI),
                     Maximum = Q3+(1.5*IQR),
                     MaximumOutlier = max(BCUI)
                     )
    return(DT)
  }))
}))
fl <- file.path(Paths$outputPath, "speciesDT.csv")
write.csv(TBsp, file = fl)
drive_upload(fl, as_id(figuresFolder))

TBmsp <- rbindlist(lapply(unique(finalTable6$Species), function(BIRD){
    print(paste0("Processing ", BIRD))
    BCUI <- finalTable6[Species == BIRD, umbrellaIndex]
    Q1 <- as.numeric(quantile(BCUI, 0.25))
    Q3 <- as.numeric(quantile(BCUI, 0.75))
    IQR <- Q3-Q1
    DT <- data.table(Species = BIRD,
                     Habitat = unique(finalTable6[Species == BIRD, 
                                                  Habitat]),
                     Mean = mean(BCUI),
                     SD = sd(BCUI),
                     Median = median(BCUI),
                     Q1 = Q1,
                     Q3 = Q3,
                     IQR = IQR,
                     Minimum = Q1-(1.5*IQR),
                     MinimumOutlier = min(BCUI),
                     Maximum = Q3+(1.5*IQR),
                     MaximumOutlier = max(BCUI)
    )
    return(DT)
}))
fl <- file.path(Paths$outputPath, "meanSpeciesDT.csv")
write.csv(TBmsp, file = fl)
drive_upload(fl, as_id(figuresFolder))

TBmhb <- rbindlist(lapply(unique(finalTable6$Habitat), function(HAB){
    print(paste0("Processing ", HAB))
    BCUI <- finalTable6[Habitat == HAB, umbrellaIndex]
    Q1 <- as.numeric(quantile(BCUI, 0.25))
    Q3 <- as.numeric(quantile(BCUI, 0.75))
    IQR <- Q3-Q1
    DT <- data.table(Habitat = HAB,
                     Mean = mean(BCUI),
                     SD = sd(BCUI),
                     Median = median(BCUI),
                     Q1 = Q1,
                     Q3 = Q3,
                     IQR = IQR,
                     Minimum = Q1-(1.5*IQR),
                     MinimumOutlier = min(BCUI),
                     Maximum = Q3+(1.5*IQR),
                     MaximumOutlier = max(BCUI)
    )
    return(DT)
}))
fl <- file.path(Paths$outputPath, "meanHabitatDT.csv")
write.csv(TBmhb, file = fl)
drive_upload(fl, as_id(figuresFolder))

TBhb <- rbindlist(lapply(unique(finalTable6$Habitat), function(HAB){
  TB <- rbindlist(lapply(unique(finalTable6$Scenario), function(SCEN){
    print(paste0("Processing ", HAB, " for scenario ", SCEN))
    BCUI <- finalTable6[Habitat == HAB & Scenario == SCEN, umbrellaIndex]
    Q1 <- as.numeric(quantile(BCUI, 0.25))
    Q3 <- as.numeric(quantile(BCUI, 0.75))
    IQR <- Q3-Q1
    DT <- data.table(Habitat = HAB,
                     Scenario = SCEN,
                     Mean = mean(BCUI),
                     SD = sd(BCUI),
                     Median = median(BCUI),
                     Q1 = Q1,
                     Q3 = Q3,
                     IQR = IQR,
                     Minimum = Q1-(1.5*IQR),
                     MinimumOutlier = min(BCUI),
                     Maximum = Q3+(1.5*IQR),
                     MaximumOutlier = max(BCUI)
    )
    return(DT)
  }))
}))
fl <- file.path(Paths$outputPath, "habitatDT.csv")
write.csv(TBhb, file = fl)
drive_upload(fl, as_id(figuresFolder))

TBall <- TB <- rbindlist(lapply(unique(finalTable6$Scenario), function(SCEN){
  print(paste0("Processing scenario ", SCEN))
  BCUI <- finalTable6[Scenario == SCEN, umbrellaIndex]
  Q1 <- as.numeric(quantile(BCUI, 0.25))
  Q3 <- as.numeric(quantile(BCUI, 0.75))
  IQR <- Q3-Q1
  DT <- data.table(Scenario = SCEN,
                   Mean = mean(BCUI),
                   SD = sd(BCUI),
                   Median = median(BCUI),
                   Q1 = Q1,
                   Q3 = Q3,
                   IQR = IQR,
                   Minimum = Q1-(1.5*IQR),
                   MinimumOutlier = min(BCUI),
                   Maximum = Q3+(1.5*IQR),
                   MaximumOutlier = max(BCUI)
  )
  return(DT)
}))
fl <- file.path(Paths$outputPath, "scenarioDT.csv")
write.csv(TBall, file = fl)
drive_upload(fl, as_id(figuresFolder))

BCUI <- finalTable6[,umbrellaIndex]
Q1 <- as.numeric(quantile(BCUI, 0.25))
Q3 <- as.numeric(quantile(BCUI, 0.75))
IQR <- Q3-Q1
TBmall <- data.table(Mean = mean(BCUI),
                   SD = sd(BCUI),
                   Median = median(BCUI),
                   Q1 = Q1,
                   Q3 = Q3,
                   IQR = IQR,
                   Minimum = Q1-(1.5*IQR),
                   MinimumOutlier = min(BCUI),
                   Maximum = Q3+(1.5*IQR),
                   MaximumOutlier = max(BCUI)
  )
fl <- file.path(Paths$outputPath, "meanScenarioDT.csv")
write.csv(TBmall, file = fl)
drive_upload(fl, as_id(figuresFolder))

# Getting values for species across time and scenario
TBmsp[, overlapsZero := fifelse((Mean-SD) < 0 & (Mean+SD) > 0, 
                                TRUE, FALSE)]

########################################
#   STATS   EXTRA:   Normality  Test   #
########################################

normalityTable <- rbindlist(lapply(unique(finalTable6$Species), function(BIRD){
  print(paste0("Stats being run for ", BIRD))
  subDT <- finalTable6[Species == BIRD, umbrellaIndex]
  sTeste <- shapiro.test(subDT)
  pVal <- sTeste[["p.value"]]
  DT <- data.table(Species = BIRD,
                   pValue = pVal,
                   Normality = ifelse(pVal < 0.05, 
                                      "Non-normal", 
                                      "Normal")
  )
  return(DT)
}))


########################################
#  PLOTS  EXTRA:  Individual Species   #
########################################

redoIndividualSpecies <- FALSE
redoIndividualMaps <- FALSE
redoIndividualModels <- FALSE

if (redoIndividualSpecies){ # Don't need to touch individual species
  finalTable4 <- merge(finalTable, 
                       birdsGroupingTable, by = "Species", all.x = TRUE)
  DTd <- dcast(finalTable4,
               Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
               value.var = "ProportionIndividualsConserved")
  
  finalTable4 <- rbindlist(lapply(unique(DTd$Species), function(sp){
    allYears <- rbindlist(lapply(unique(DTd$Year), function(Y){
      allAreas <- rbindlist(lapply(unique(DTd$ProportionAreaChosen), function(A){
        toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                    "Habitat", "caribou", sp, "random")
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
  })) # I think this was fixed and should be deprecated
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland")
  
  cols <- gg_color_hue(length(sps))
  
  Require("ggplot2")
  
  finalTableSP <- merge(finalTable4,
                        birdsGroupingTable, by = "Species", all.x = TRUE)
  
  setkey(finalTableSP, "Habitat", "Species", "umbrellaIndex")
  
  byScenario <- TRUE
  byClimateModel <- FALSE
  
  Cls <- c("CanESM2" = "darkred", "CCSM4" = "goldenrod1", "INM-CM4" = "darkgreen")
  scens <- seq(0.05, 0.95, by = 0.1)
  source("posthocFunctions/makeMapsForSp.R")
  tictoc::tic("Total Time Elapsed: ")
  Require("future")
  Require("future.apply")
  # plan("sequential")
  spToRemove <- c("BOWA", "BANS", "PIWO", "NOFL", "BARS", "BRBL")
  bibis <- allb[!allb %in% spToRemove]
  print("Starting maps for species...")
  plan("multicore")
  future_lapply(bibis, function(sp){ #allb # 
    lapply(yearsWanted, function(y){
      lapply(scens, function(scen){
        print(paste0("Running ", sp, " for ", y, " for ", scen))
        # INDIVIDUAL MAPS
        tic(paste0("Time elapsed for ", sp, " for ", y, " for ", scen, ": "))
        if (redoIndividualMaps){
          mps <- makeMapsForSp(Species = sp,
                               Year = y, 
                               overwriteFig = FALSE,
                               birdSolPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               booSolPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               Scenario = scen,
                               forceUpload = FALSE,
                               outPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               uploadTo = individualSpFolder)
        }
      })
    })
    if (redoIndividualModels){
      # INDIVIDUAL MODELS
      tableGLM <- Copy(finalTable4)
      subTableGLM <- tableGLM[Species == sp, ]
      m2 <- lm(umbrellaIndex ~ ProportionAreaChosen + Year, 
               data = subTableGLM)
      fileName <- file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/",
                            paste0(sp, "_umbrellaThroughTime.csv"))
      capture.output(summary(m2), file = fileName)
      allFls <- drive_ls(as_id(individualSpFolder))
      fileThere <- any(grepl(pattern = basename(fileName), 
                             x = allFls$name))
      if (!fileThere)
        drive_upload(fileName, as_id(individualSpFolder))
      # INDIVIDUAL BOXPLOT 
      tableIndvPlots <- finalTableSP[Species == sp, ]
      finalPlot <- ggplot(data = tableIndvPlots, aes(x = Year, 
                                                     y = umbrellaIndex)) +
        geom_jitter(aes(color = ClimateScenario)) +
        scale_color_manual(name = "Climate Model", 
                           values = Cls) +
        facet_grid(cols = vars(ProportionAreaChosen),
                   rows = vars(Species)) +
        xlab("Time") + 
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 1, linetype = "dashed") +
        ylab(paste0("Umbrella Index")) +
        coord_cartesian(ylim = c(min(tableIndvPlots[["umbrellaIndex"]])-0.01, 
                                 max(tableIndvPlots[["umbrellaIndex"]])+0.01)) +
        geom_smooth(method = 'lm', colour = "black") +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12))
      fileNamePlot <- file.path(Paths$outputPath, 
                                paste0("speciesLM_", sp,".png"))
      ggsave(device = "png", filename = fileNamePlot, 
             width = 11, height = 8)
      fileThere <- any(grepl(pattern = basename(fileNamePlot), 
                             x = allFls$name))
      if (!fileThere)
        drive_upload(file.path(Paths$outputPath, paste0("speciesLM_", sp,".png")), 
                     as_id(individualSpFolder))
    }
    toc()
  })
  plan("sequential")
  toc()
}

# Figure 6 -- Individual species plots as examples
finalTableSP <- merge(finalTable4,
                      birdsGroupingTable, by = "Species", all.x = TRUE)

setkey(finalTableSP, "Habitat", "Species", "umbrellaIndex")

whichSpToUse1 <- c("AMRO", "LEYE")
whichSpToUse2 <- c("ATTW", "WIWR")

figure6Table1 <- finalTableSP[Species %in% whichSpToUse1, ]
figure6Table2 <- finalTableSP[Species %in% whichSpToUse2, ]

Cls <- c("CanESM2" = "darkred", 
         "CCSM4" = "goldenrod1", 
         "INM-CM4" = "royalblue")

labels1 <- c("American Robin", "Lesser Yellowlegs")
names(labels1) <- c("AMRO", "LEYE")

labels2 <- c("American T. Woodpecker", "Winter Wren")
names(labels2) <- c("ATTW", "WIWR")

figure6.1 <- ggplot(data = figure6Table1, aes(x = Year, 
                                               y = umbrellaIndex)) +
  geom_jitter(aes(color = ClimateScenario)) +
  scale_color_manual(name = "Climate Model", 
                     values = Cls) +
  facet_grid(cols = vars(ProportionAreaChosen),
             rows = vars(Species), 
             labeller = labeller(Species = labels1)) +
  # facet_wrap(ProportionAreaChosen ~ Species) +
  xlab("Time") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ylab(paste0("Umbrella Index")) +
  scale_x_continuous(breaks = c(2011, 2071)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25)) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  # coord_cartesian(ylim = c(min(tableIndvPlots[["umbrellaIndex"]])-0.01, 
  #                          max(tableIndvPlots[["umbrellaIndex"]])+0.01)) +
  geom_smooth(method = 'lm', colour = "black") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.size = unit(3, 'cm'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

figure6.2 <- ggplot(data = figure6Table2, aes(x = Year, 
                                              y = umbrellaIndex)) +
  geom_jitter(aes(color = ClimateScenario)) +
  scale_color_manual(name = "Climate Model", 
                     values = Cls) +
  facet_grid(cols = vars(ProportionAreaChosen),
             rows = vars(Species), 
             labeller = labeller(Species = labels2)) +
  # facet_wrap(ProportionAreaChosen ~ Species) +
  xlab("Time") + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  ylab(paste0("Umbrella Index")) +
  coord_cartesian(ylim = c(-0.5, 0.5)) +
  # coord_cartesian(ylim = c(min(tableIndvPlots[["umbrellaIndex"]])-0.01, 
  #                          max(tableIndvPlots[["umbrellaIndex"]])+0.01)) +
  scale_x_continuous(breaks = c(2011, 2071)) +
  scale_y_continuous(breaks = seq(-0.5, 0.5, 0.25)) +
  geom_smooth(method = 'lm', colour = "black") +
  theme_bw() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.key.size = unit(3, 'cm'),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 16))

figure6.1
fileNamePlot1 <- file.path(Paths$outputPath, 
                          paste0("!speciesLM_Figure6A_redo1.png"))
ggsave(device = "png", filename = fileNamePlot1, 
       width = 20, height = 20, unit = "cm")

figure6.2
fileNamePlot2 <- file.path(Paths$outputPath, 
                           paste0("!speciesLM_Figure6B_redo1.png"))
ggsave(device = "png", filename = fileNamePlot2, 
       width = 20, height = 20, unit = "cm")

lapply(c(fileNamePlot1, fileNamePlot2), 
       drive_upload, path = as_id("1D8-H4h-59vD3nea9sn3aICa4SqVRulQO"))
       # drive_upload, path = as_id(LMspFolder))

###########################################################
#                                                         #
# Identifying the groupping with best values per species  # 
#                                                         #
###########################################################

# BELOW I DEPRECATED!

# DT4 <- Copy(DT3)
# DT4[, meanPIC := mean(ProportionIndividualsConserved), by = c("Species", "PrioritizedFor")]
# DT5 <- unique(DT4[, c("Species", "Habitat", "PrioritizedFor", "meanPIC")])
# DT4[, suggestedClass := max(ProportionIndividualsConserved), by = c("Species", "PrioritizedFor")]
# DT5 <- dcast(DT4, 
#              Species + Habitat ~ PrioritizedFor,
#              value.var = "meanPIC", fun = sum)
# 
# DT5[, suggestedClass := colnames(.SD)[max.col(.SD, ties.method = "first")], 
#     .SDcols = c(unique(DTx1[["Habitat"]]), "random", "caribou")]
# DT5[, misclassified := Habitat != suggestedClass]
# unique(DTx2[misclassified == TRUE, c("Species", "Habitat", "suggestedClass")])
# 


