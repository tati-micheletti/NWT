defineModule(sim, list(
  name = "waterlandClassification",
  description = paste0("This module can be used to return a raster identifying", 
                       " which pixels are lowlands and which pixels are uplands by using LCC05 or LCC10",
                       "and the Hybrid Wetland Layer from Ducks Unlimited Canada v. 2.1"),
  keywords = "lowlands",
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.4", waterlandClassification = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "waterlandClassification.Rmd"),
  reqdPkgs = list("magrittr", "LandR", "rgdal", "data.table"),
  parameters = rbind(
    defineParameter(name = ".useCache", class = "logical", default = FALSE, min = NA, max = NA, 
                    desc = "Should this entire module be run with caching activated?"),
    defineParameter(name = "baseLayer", class = "character", default = c("LCC05", "LCC10"), min = NA, max = NA, 
                    desc = "Which layer should be used? LCC05, LCC10 or both?"),
    defineParameter(name = "wetValue", class = "numeric", default = NA, min = NA, max = NA, 
                    desc = "Which value represents wetland in the wetlandRaster?")
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "wetlandRaster", objectClass = "RasterLayer", 
                 desc = paste0("Any raster layer with wetlands. Default in this project is", 
                 " Hybrid Wetland Layer from Ducks Unlimited Canada v. 2.1"), 
                 sourceURL = "https://drive.google.com/open?id=1wNpBdLICWDJ-DGwDboPb9wVwRwtGm1go"),
    expectsInput(objectName = "studyArea", objectClass = "shapefile", 
                 desc = "Shapefile of the studyArea to be used",
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "wetLCC", objectClass = "list", 
                  desc = paste0("List of RasterLayers containing identifying", 
                                " uplands and lowlands in a given study", 
                                " area based on LCC05 and/or 2010")),
    createsOutput(objectName = "wetDiagnostics", objectClass = "list", 
                  desc = paste0(" It returns the diagnostics on which approach should be used:", 
                                "pixel based, or class using XXXXXX"))
  )
))

doEvent.waterlandClassification = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "loadWetlandLayer")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "createWetZone")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "diagnostics")
    },
    loadWetlandLayer = {
      
      sim$wetlandRaster <- Cache(prepSpeciesLayers_DUCKS, destinationPath = dataPath(sim), 
                                 studyArea = sim$studyArea, 
                                 userTags = "objectName:wetlandRaster")
      },
    createWetZone = {
      
      sim$wetLCC <- classifyWetlands(LCC = P(sim)$baseLayer,
                                     wetLayerInput = sim$wetlandRaster,
                                     pathData = dataPath(sim),
                                     studyArea = sim$studyArea)
    },
    diagnostics = {
      
      # 4. run diagnostics comparing to pure layer
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if (!suppliedElsewhere("studyArea", sim)) 
    sim$studyArea <- cloudCache(prepInputs, 
                                url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                                destinationPath = paths$inputPath, 
                                useCloud = TRUE, cloudFolderID = "https://drive.google.com/open?id=1PoEkOkg_ixnAdDqqTQcun77nUvkEHDc0")
  
  if (!suppliedElsewhere("wetlandRaster", sim)){
    message("wetlandRaster not supplied, default is Hybrid Wetland from DUCKS Unlimited Canada")
    if (!suppliedElsewhere("rasterToMatch", sim))
      message("rasterToMatch not supplied, wetlandRaster will not be reprojected nor resampled")
  }

  if (is.null(P(sim)$baseLayer))
    P(sim)$baseLayer  <- c("LCC05", "LCC10")

  return(invisible(sim))
}