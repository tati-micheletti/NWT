makeProbabilityPresenceRaster <- function(listOfRasters = NULL,
                                     species,
                                     yearOfAnalysis,
                                     outputFolder,
                                     noCorner,  # <~~~~~~~~ TEMPORARY!!!
                                     useFuture = TRUE,
                                     flammableRTM,
                                     percentToDiscard = 0.3,
                                     typeOfAnalysis = "colonization") # The other option is "density"
{ 
  # THIS CAN BE INCORPORATED INTO THE calcProportionPixelsLost
  source('~/projects/NWT/temp/calcColonization.R')
  library("rasterVis")
  finalTablefileNamePath <- file.path(outputFolder, paste0("probabilityRastersPath", yearOfAnalysis, 
                                                 typeOfAnalysis,".qs"))
  if (!file.exists(finalTablefileNamePath)){
    if (is.null(listOfRasters))
      stop("If final object does not exist. listOfRasters must be supplied")
    if (useFuture) plan("multiprocess", workers = length(species))
    allBirds <- future_lapply(names(listOfRasters), function(sp){ ########### future_lapply <~~~~~~~~~~~~~~~~~~~~future_
      allScenarios <- raster::stack(lapply(names(listOfRasters[[sp]]), function(scenario){
        tic(paste0("Calculating ", typeOfAnalysis," for ", paste(sp, scenario, sep = " ")))
        allMods <- lapply(names(listOfRasters[[sp]][[scenario]]), function(bmod){
          allRuns <- raster::stack(lapply(names(listOfRasters[[sp]][[scenario]][[bmod]]), function(runs){
            colRasPath <- file.path(outputFolder, paste(typeOfAnalysis, paste0("Year", yearOfAnalysis), 
                                                        sp, scenario, bmod, runs, sep = "_"))
            if (!file.exists(paste0(colRasPath, ".tif"))){
              rasStk <- listOfRasters[[sp]][[scenario]][[bmod]][[runs]]
              colRas <- calcColonization(rasT0Path = rasStk[[1]],
                                         rasName = colRasPath,
                                         typeOfAnalysis = typeOfAnalysis)
              gc()
            } else {
              colRas <- raster::raster(paste0(colRasPath, ".tif"))
            }
            return(colRas)
          }))
          names(allRuns) <- names(listOfRasters[[sp]][[scenario]][[bmod]])
          ## CALCULATE PROBABILITY OF PRESENCE FOR EACH SCENARIO (ACROSS RUNS)
          fileNamePath <- file.path(outputFolder, paste0(ifelse(typeOfAnalysis == "colonization", 
                                                                "probabilityPresence_", 
                                                                "density_"), yearOfAnalysis,"_", 
                                                         paste0(scenario, "_", bmod), "_", sp))
          calcWhat <- ifelse(typeOfAnalysis == "colonization",
                             "probability of presence", "density")
          tic(paste0("Calculating ",calcWhat," in ", yearOfAnalysis, " ", 
                     paste0(scenario, "_", bmod)," for ", sp))
          if (!file.exists(paste0(fileNamePath, ".tif"))){
            probPresence <- calc(x = allRuns, fun = mean, na.rm = TRUE,
                                 filename = fileNamePath,
                                 format = "GTiff")
            # TEMPORARY ####################
            ### TODO >>>> HERE CUT THE CORNER
            probPresence[is.na(noCorner)] <- NA # <~~~~~REMOVE
            # SAVE AGAIN!
            writeRaster(probPresence, 
                        filename = fileNamePath, 
                        overwrite = TRUE, format = "GTiff")
            message(crayon::green(paste0("Corner removed for ", sp)))
            # TEMPORARY ####################
            gc()
            probPresence <- raster(paste0(fileNamePath, ".tif"))
          } else {
            probPresence <- raster(paste0(fileNamePath, ".tif"))
          }
          
          #### MAKE LEVEL PLOT ALREADY FOR PRESENCE IN 2100
          library(viridis)
          palInf <- viridis(100)
          speciesChangesPath <- file.path(outputFolder, paste0(ifelse(typeOfAnalysis == "colonization", 
                                                                      "probabilityPresence_", 
                                                                      "density_"), yearOfAnalysis
                                                               ,"_", 
                                                               paste0(scenario, "_", bmod),
                                                         "_", sp, ".png"))
            if (!file.exists(speciesChangesPath)){
              probPresence[is.na(flammableRTM)] <- NA
              png(filename = speciesChangesPath,
                  width = 21, height = 29,
                  units = "cm", res = 300)
              print(levelplot(probPresence,
                              sub = paste0(sp, " ", calcWhat," in 2100"),
                              margin = FALSE,
                              maxpixels = 6e6,
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
                              col.regions = palInf, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                              par.strip.text = list(cex = 0.8,
                                                    lines = 1,
                                                    col = "black")))
              dev.off()
            }
        toc()
        return(probPresence)
      })
        names(allMods) <- names(listOfRasters[[sp]][[scenario]])
      return(allMods)
    }))
      names(allScenarios) <- names(listOfRasters[[sp]])
      return(allScenarios)
    })
    plan("sequential")
    names(allBirds) <- names(listOfRasters)
    qs::qsave(allBirds, finalTablefileNamePath)
  } else {
    allBirds <- qs::qread(finalTablefileNamePath)
    message(paste0("Table loaded for ", names(allBirds)))
  }
}
