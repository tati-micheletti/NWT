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
  reqdPkgs = list("googledrive", "data.table", "raster"),
  parameters = rbind(
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "birdsList", objectClass = "character", 
                 desc = "Bird species to be predicted", sourceURL = NA),
    expectsInput(objectName = "cloudFolderID", objectClass = "character", 
                 desc = "Folder ID for cloud caching", sourceURL = NA),
    expectsInput(objectName = "urlModels", objectClass = "character", 
                 desc = "Url for the GDrive folder that has all model objects",
                 sourceURL = "BAM.SharedDrive/RshProjs/CC/CCImpacts/NWT-cc-fire/Models/BirdModelsv1/"),
    expectsInput(objectName = "urlStaticLayers", objectClass = "RasterLayer", 
                 desc = "Static Layers (WAT, URBAG, lLED25, DEV25 and landform) url", 
                 sourceURL = "https://drive.google.com/open?id=1OzWUtBvVwBPfYiI_L_2S1kj8V6CzB92D")
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "birdPrediction", objectClass = "list", 
                  desc = "List per year of the bird species predicted rasters"),
    createsOutput(objectName = "birdModels", objectClass = "list", 
                  desc = "List of the bird models for prediction"),
    createsOutput(objectName = "staticLayers", objectClass = "RasterStack", 
                  desc = paste0("Raster stack of all static layers (WAT, URBAG,", 
                                "lLED25, DEV25 and landform) for the bird models"))
  )
))

doEvent.birdsNWT = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadModels")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "loadFixedLayers")
      sim <- scheduleEvent(sim, start(sim), "birdsNWT", "predictBirds")
      
    },
    loadModels = {
      sim$birdModels <- Cache(loadBirdModels, birdsList = sim$birdsList,
                              folderUrl = extractURL("urlModels"),
                              cloudFolderID = sim$cloudFolderID,
                              pathData = dataPath(sim))
      message("Bird models loaded for: \n", paste(sim$birdsList, collapse = "\n"))
    },
    loadFixedLayers = {
      sim$staticLayers <- Cache(loadStaticLayers, fileURL = extractURL("urlStaticLayers"),
                                pathData = dataPath(sim), 
                                cloudFolderID = sim$cloudFolderID)
      message("The following static layers have been loaded: \n", paste(names(sim$staticLayers), collapse = "\n"))
      
    },
    predictBirds = {
      sim$birdPrediction[[paste0("Year", time(sim))]] <- Cache(predictDensities, birdSpecies = sim$birdsList,
                                                               successionTable = "TO CHECK FROM LandR",
                                                               staticLayers = sim$staticLayers,
                                                               currentTime = time(sim),
                                                               modelList = sim$birdModels,
                                                               pathData = dataPath(sim),
                                                               cacheId = paste0("predicted", time(sim)))
      sim <- scheduleEvent(sim, end(sim), "birdsNWT", "predictBirds")
      
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
