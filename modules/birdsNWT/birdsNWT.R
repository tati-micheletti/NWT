defineModule(sim, list(
  name = "birdsNWT",
  description = paste0("This module loads a bird model from Stralberg (unpublished)", 
                       "for each species of interest",
                       " for the NWT, as well as static layers. Dynamic layers needed ", 
                       "for prediction come from LandR_Biomass"),
  keywords = c("NWT", "birds"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
              person("Diana", "Stralberg", email = "dstralberg@gmail.com", role = "aut")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", birdsNWT = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "birdsNWT.Rmd"),
  reqdPkgs = list("googledrive", "data.table", "raster", "gbm", "crayon", "plyr", "dplyr"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching?"),
    defineParameter("useParallel", "logical", FALSE, NA, NA, "Should bird prediction be parallelized?"),
    defineParameter("useTestSpeciesLayers", "logical", TRUE, NA, NA, "Should bird prediction be parallelized?"),
    defineParameter("nCores", "character|numeric", "auto", NA, NA, "If parallelizing, how many cores to use? Use 'auto' (90% of available), or numeric")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdsList", objectClass = "character", 
                 desc = "Bird species to be predicted", sourceURL = NA),
    expectsInput(objectName = "cloudFolderID", objectClass = "character", 
                 desc = "Folder ID for cloud caching", sourceURL = NA),
    expectsInput(objectName = "urlModels", objectClass = "character", 
                 desc = "Url for the GDrive folder that has all model objects",
                 sourceURL = "https://drive.google.com/open?id=1obSvU4ml8xa8WMQhQprd6heRrN47buvI"),
    expectsInput(objectName = "urlStaticLayers", objectClass = "RasterLayer", 
                 desc = "Static Layers (WAT, URBAG, lLED25, DEV25 and landform) url", 
                 sourceURL = "https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdPrediction", objectClass = "list", 
                  desc = "List per year of the bird species predicted rasters"),
    createsOutput(objectName = "birdModels", objectClass = "list", 
                  desc = "List of the bird models for prediction"),
    createsOutput(objectName = "staticLayers", objectClass = "RasterStack", 
                  desc = paste0("Raster stack of all static layers (WAT, URBAG,", 
                                "lLED25, DEV25 and landform) for the bird models")),
    createsOutput(objectName = "successionLayers", objectClass = "RasterStack", 
                  desc = paste0("Raster stack of all succession layers (species)", 
                                " and total biomass for the bird models"))
  )
))

doEvent.birdsNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadModels")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadFixedLayers")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "predictBirds", eventPriority = .last())
      
    },
    loadModels = {
      sim$birdModels <- Cache(loadBirdModels, birdsList = sim$birdsList,
                              folderUrl = extractURL("urlModels"),
                              # cloudFolderID = sim$cloudFolderID,
                              pathData = dataPath(sim),
                              # useCloud = TRUE,
                              omitArgs = c("pathData"))#, "cloudFolderID", "useCloud")) # NOT WORKING WITH CLOUDCACHE [ FIX ]
      message("Bird models loaded for: \n", paste(sim$birdsList, collapse = "\n"))
    },
    loadFixedLayers = {
      sim$staticLayers <- Cache(loadStaticLayers, fileURL = extractURL("urlStaticLayers"),
                                pathData = dataPath(sim), 
                                # cloudFolderID = sim$cloudFolderID,
                                studyArea = sim$studyArea,
                                # useCloud = TRUE,
                                omitArgs = c("pathData"))#, "cloudFolderID", "useCloud")) # NOT WORKING WITH CLOUDCACHE [ FIX ]
      message("The following static layers have been loaded: \n", 
              paste(names(sim$staticLayers), collapse = "\n"))
    },
    predictBirds = {
      if (P(sim)$useTestSpeciesLayers == TRUE){
        message("Using test layers for species. Predictions will be static and identical to original data.")
        sim$successionLayers <- Cache(loadTestSpeciesLayers, 
                                      modelList = sim$birdModels,
                                      pathData = dataPath(sim)) # Need to add Biomass and Structure Age to this layer!
      } else {
        if (any(!suppliedElsewhere("simulatedBiomassMap", sim), 
                !suppliedElsewhere("cohortData", sim),
                !suppliedElsewhere("pixelGroupMap", sim)))
          stop("useTestSpeciesLayers is FALSE, but apparently no vegetation simulation was run")
        
        sim$successionLayers <- Cache(createSpeciesStackLayer,
                                      modelList = sim$birdModels,
                                      simulatedBiomassMap = sim$simulatedBiomassMap,
                                      cohortData = sim$cohortData,
                                      staticLayers = sim$staticLayers,
                                      sppEquiv = sim$sppEquiv,
                                      pixelGroupMap = sim$pixelGroupMap,
                                      pathData = dataPath(sim),
                                      # cloudFolderID = sim$cloudFolderID,
                                      userTags = paste0("successionLayers", time(sim)),
                                      # useCloud = TRUE,
                                      omitArgs = c("pathData"))#, "cloudFolderID", "useCloud"))  # NOT WORKING WITH CLOUDCACHE [ FIX ]
      }

      sim$birdPrediction[[paste0("Year", time(sim))]] <- Cache(predictDensities, birdSpecies = sim$birdsList,
                                                               successionLayers = sim$successionLayers,
                                                               staticLayers = sim$staticLayers,
                                                               currentTime = time(sim),
                                                               modelList = sim$birdModels,
                                                               pathData = dataPath(sim),
                                                               overwritePredictions = FALSE,
                                                               useParallel = P(sim)$useParallel,
                                                               nCores = P(sim)$nCores,
                                                               # cloudFolderID = sim$cloudFolderID,
                                                               omitArgs = c("destinationPath", "nCores", 
                                                                            "useParallel", "pathData"),
                                                                            # "cloudFolderID", 
                                                                            # "useCloud"),
                                                               # useCloud = TRUE,
                                                               userTags = paste0("predictedBirds", time(sim)))

        sim <- scheduleEvent(sim, time(sim) + 10, "birdsNWT", "predictBirds")
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  if (!suppliedElsewhere(object = "birdsList", sim = sim)){
    sim$birdsList <- c("REVI", "HETH", "RCKI", "HAFL", "WIWR", "GRCA", "RBNU", "WIWA", 
                       "GRAJ", "RBGR", "WEWP", "GCKI", "PUFI", "WETA", "FOSP", "PISI", 
                       "WCSP", "EVGR", "WBNU", "PIGR", "BTNW", "EAPH", "PHVI", "WAVI", 
                       "BRTH", "EAKI", "BRCR", "PAWA", "VESP", "DEJU", "BRBL", "OVEN", 
                       "VEER", "CSWA", "BOCH", "VATH", "OSFL", "BLPW", "COYE", "TRES", 
                       "BLJA", "OCWA", "TOWA", "TEWA", "BLBW", "CORA", "NOWA", "SWTH", 
                       "BHVI", "CONW", "MOWA", "SWSP", "BHCO", "COGR", "MAWA", "CMWA", 
                       "SOSP", "BCCH", "LISP", "YRWA", "CHSP", "SEWR", "BBWA", "LEFL", 
                       "YBFL", "CEDW", "SAVS", "BAWW", "LCSP", "WWCR", "CCSP", "RWBL", 
                       "BAOR", "HOWR", "WTSP", "CAWA", "RUBL", "AMRO", "HOLA", "AMRE", 
                       "AMGO", "AMCR", "ALFL")  
  }
  return(invisible(sim))
}
