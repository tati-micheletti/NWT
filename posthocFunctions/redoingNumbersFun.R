climateScenarios <- c("CCSM4", "CanESM2", "INM-CMR4")
runs <- paste0("run", 1:5)
specificScenarios <- data.table(expand.grid(as.character(as.roman(32:41)), 
                                 c("shrub", "generalist", "deciduous",
                                   "conifer", "wetland", "grassland")))
specificScenarios[, scenarios := paste(Var1, Var2, sep = "_")]
wantedScenarios <- c(as.character(as.roman(22:31)),
                                  specificScenarios[["scenarios"]],
                     as.character(as.roman(42:51)))
yearsWanted <- seq(2011, 2091, by = 20)

if(any(overwriteFig1Numbers,
       !file.exists(allNumbersPath))){
  Fig1_numbers <- rbindlist(lapply(X = climateScenarios, function(ClimateScenario){
    allRuns <- rbindlist(lapply(X = runs, function(Run){
      allScenarios <- rbindlist(lapply(X = wantedScenarios, function(scen){
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
        # in the Paths$outputPath folder
        fold <- file.path(dirname(dirname(Paths$outputPath)),
                  paste0(ClimateScenario, "_", Run),
                  "hotspots/ms1")
        solRasName <- grepMulti(x = list.files(path = fold,
                                               full.names = TRUE), 
                                patterns = c(paste0(scen, "_solutions"), "Year2011"))
        if (!file.exists(solRasName)){
          stop(paste0("The file ", solRasName, " doesn't exist in the folder ", fold))
        } else {
          solRas <- raster::raster(solRasName)
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
        return(bnc)
        
      }))
    }))
  }))
} else {
  Fig1_numbers <- qs::qread(allNumbersPath)
}



# OLD VERSION!!! Useful to re-calculate the averages!

# if(any(overwriteFig1Numbers,
#        !file.exists(allNumbersPath))){
#   Fig1_numbers <- rbindlist(lapply(X = yearsWanted, 
#                                    FUN = function(Y){
#                                      allScen <- rbindlist(lapply(wantedScenarios, function(scen){
#                                        message("Calculating % caribou RSF and bird abundance for scenario ", 
#                                                scen,
#                                                " and year ",
#                                                Y)
#                                        solRasName <- file.path(Paths$outputPath, 
#                                                                paste0("solRas_", scen,
#                                                                       "_Year2011.tif"))
#                                        if (!file.exists(solRasName)){
#                                          allScenThisYear <- scenPerYear[[paste0("Year", Y)]]
#                                          allNms <- names(allScenThisYear)
#                                          scNms <- unlist(lapply(strsplit(allNms, split = "_"), 
#                                                                 function(S) return(S[2])))
#                                          whichToKeep <- which(scNms %in% scen)
#                                          solRas <- allScenThisYear[[whichToKeep]]
#                                          message(crayon::yellow("Saving solution raster for scenario ", 
#                                                                 scen))
#                                          writeRaster(solRas, filename = solRasName, 
#                                                      format = "GTiff")
#                                        } else {
#                                          message(crayon::green("Loading solution raster for scenario ", 
#                                                                scen))
#                                          solRas <- raster::raster(solRasName)
#                                        }
#                                        solRas[] <- solRas[]
#                                        # 3. Calculate total area of that
#                                        totAreaHa <- 6.25*sum(solRas[], na.rm = TRUE)
#                                        totRas <- solRas
#                                        totRas[!is.na(solRas)] <- 1
#                                        totAreaHaNWT <- 6.25*sum(totRas[], na.rm = TRUE)
#                                        # 4. Get all birds %
#                                        tic("Boo and birds calculations elapsed time: ")
#                                        birdsPerc <- rbindlist(lapply(allb, function(BIRD){
#                                          # NEED TO EXTRACT THE TOTAL BIRDS AT THE MOMENT -- 2011 AS A BASELINE
#                                          message(paste0("Extracting predictions of ", BIRD, 
#                                                         " for year 2011 (to calculate total birds today). 
#                                          Species ", which(allb == BIRD), " of ", length(allb)))
#                                          flName <- file.path(Paths$outputPath, paste0("allClimScen_mean_", BIRD,
#                                                                                       "_Year2011.tif"))
#                                          if (!file.exists(flName)){
#                                            message(crayon::yellow(flName, " doesn't exist. Creating..."))
#                                            fls <- grepMulti(list.files(path = Paths$inputPath,
#                                                                        recursive = TRUE, 
#                                                                        full.names = TRUE),
#                                                             patterns = c("predicted", BIRD, 2011, ".tif"))
#                                            stkOriginal <- calc(raster::stack(lapply(fls, raster)), 
#                                                                fun = mean, na.rm = TRUE, 
#                                                                filename = flName, 
#                                                                format = "GTiff")
#                                          } else {
#                                            message(crayon::green(flName, " exists. Loading..."))
#                                            stkOriginal <- raster::raster(flName)
#                                          }
#                                          # NEED TO EXTRACT THE TOTAL BIRDS AT EACH YEAR IN TIME -- 
#                                          # TO CALCULATE HOW MUCH WE ARE PROTECTING
#                                          message(paste0("Extracting predictions of ", BIRD, " for year ", Y,
#                                                         " (to calculate birds kept in year ", Y, ").
#                                                         Species ", which(allb == BIRD), " of ", length(allb)))
#                                          flName <- file.path(Paths$outputPath, paste0("allClimScen_mean_", 
#                                                                                       BIRD, "_Year", Y,".tif"))
#                                          if (!file.exists(flName)){
#                                            message(crayon::yellow(flName, " doesn't exist. Creating..."))
#                                            fls <- grepMulti(list.files(path = Paths$inputPath,
#                                                                        recursive = TRUE, 
#                                                                        full.names = TRUE),
#                                                             patterns = c("predicted", BIRD, Y, ".tif"))
#                                            stk <- calc(raster::stack(lapply(fls, raster)), 
#                                                        fun = mean, na.rm = TRUE, 
#                                                        filename = flName, 
#                                                        format = "GTiff")
#                                          } else {
#                                            message(crayon::green(flName, " exists. Loading..."))
#                                            stk <- raster::raster(flName)
#                                          }
#                                          totBirds <- 6.25*sum(stkOriginal[], na.rm = TRUE)
#                                          # Convert density to abundance (per pixel we have 6.25 ha)
#                                          conservSp <- stk*solRas
#                                          predBirds <- 6.25*sum(conservSp[], na.rm = TRUE)
#                                          DT <- data.table(species = BIRD,
#                                                           totPredictedWithPP = predBirds,
#                                                           totIndvsInitial = totBirds,
#                                                           percPredictedWithPP = predBirds/totBirds,
#                                                           Year = Y,
#                                                           scenario = scen)
#                                          return(DT)
#                                        }))
#                                        # 5. Get caribou %
#                                        booPerc <- rbindlist(lapply("relativeSelection", function(BOO){
#                                          message(paste0("Extracting predictions of ", BOO, " for year 2011 (to calculate total RSF today)"))
#                                          flName <- file.path(Paths$outputPath, paste0("allClimScen_mean_", 
#                                                                                       BOO, "_Year2011.tif"))
#                                          if (!file.exists(flName)){
#                                            fls <- grepMulti(list.files(path = Paths$inputPath,
#                                                                        recursive = TRUE, 
#                                                                        full.names = TRUE),
#                                                             patterns = c(BOO, 2011, ".tif"), 
#                                                             unwanted = "Uncertain")
#                                            stk <- calc(raster::stack(lapply(fls, raster)), 
#                                                        fun = mean, 
#                                                        na.rm = TRUE, 
#                                                        format = "GTiff")
#                                            # Need to bin
#                                            stkBin <- binRSFtoDeMars2019(stk)
#                                            writeRaster(x = stkBin, filename = flName, overwrite = TRUE)
#                                            rm(stkBin);gc()
#                                            stkOriginal <- raster::raster(flName)
#                                          } else {
#                                            stkOriginal <- raster::raster(flName)
#                                          }
#                                          
#                                          message(paste0("Extracting predictions of ", BOO, " for year ", Y,
#                                                         " (to calculate RSF kept in year ", Y, ")."))
#                                          flName <- file.path(Paths$outputPath, paste0("allClimScen_mean_", 
#                                                                                       BOO, "_Year", Y, ".tif"))
#                                          if (!file.exists(flName)){
#                                            fls <- grepMulti(list.files(path = Paths$inputPath,
#                                                                        recursive = TRUE, 
#                                                                        full.names = TRUE),
#                                                             patterns = c(BOO, Y, ".tif"), 
#                                                             unwanted = "Uncertain")
#                                            stk <- calc(raster::stack(lapply(fls, raster)), fun = mean, 
#                                                        na.rm = TRUE, 
#                                                        format = "GTiff")
#                                            # Need to bin
#                                            stkBin <- binRSFtoDeMars2019(stk)
#                                            writeRaster(x = stkBin, filename = flName, 
#                                                        overwrite = TRUE)
#                                            rm(stkBin);gc()
#                                            stk <- raster::raster(flName)
#                                          } else {
#                                            stk <- raster::raster(flName)
#                                          }
#                                          conservSp <- stk*solRas
#                                          sumBins <- sum(conservSp[], 
#                                                         na.rm = TRUE)
#                                          totBins <- sum(stkOriginal[], 
#                                                         na.rm = TRUE)
#                                          DT <- data.table(species = "caribou",
#                                                           totIndvsInitial = totBins,
#                                                           totPredictedWithPP = sumBins,
#                                                           percPredictedWithPP = sumBins/totBins,
#                                                           Year = Y,
#                                                           scenario = scen)
#                                          return(DT)
#                                        }))
#                                        toc()
#                                        bnc <- merge(booPerc, birdsPerc, all = TRUE)
#                                        bnc[, totalAreaSelected := totAreaHa]
#                                        bnc[, totalAreaNWT := totAreaHaNWT]
#                                        return(bnc)
#                                      }))
#                                      return(allScen)
#                                    }))
#   # Calculate the amount kept of total landbirds (caribou is calculated already)
#   message("Calculating bird and caribou protection for scenarios ", 
#           paste(wantedScenarios, collapse = ", "))
#   DTb <- Fig1_numbers[species != "caribou", ]
#   DTb[, totIndvsInitial := sum(totIndvsInitial), by = c("scenario", "Year")]
#   DTb[, totPredictedWithPP := sum(totPredictedWithPP), by = c("scenario", "Year")]
#   DTb[, percPredictedWithPP := totPredictedWithPP/totIndvsInitial]
#   DTb[, species := NULL]
#   DTb[, species := "landbirds"]
#   DTb <- unique(DTb)
#   Fig1_numbers <- rbind(Fig1_numbers, DTb, use.names = TRUE)
#   qs::qsave(Fig1_numbers, file = allNumbersPath)
# } else {
#   Fig1_numbers <- qs::qread(allNumbersPath)
# }

