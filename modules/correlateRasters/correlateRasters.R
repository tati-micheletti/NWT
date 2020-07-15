defineModule(sim, list(
  name = "correlateRasters",
  description = "Correlate rasters. used primarily for caribou RSF and diversity rasters", #"insert module description here",
  keywords = c("rasters", "correlation"), # c("insert key words here"),
  authors = person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.5.9008", correlateRasters = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "correlateRasters.Rmd"),
  reqdPkgs = list("raster", "data.table", "crayon", "sf","png", "fasterize","ggplot2","lme4", 
                  "tati-micheletti/usefulFuns", "PredictiveEcology/pemisc"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter(".plotInitialTime", "numeric", NA, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", NA, NA, NA, "This describes the simulation time interval between plot events"),
    defineParameter(".useCache", "logical", FALSE, NA, NA, "Should this entire module be run with caching activated? This is generally intended for data-type modules, where stochasticity and time are not relevant"),
    defineParameter("modelToUse", "character", "lm", NA, NA, paste0("Type of model to be used should be passed here. ",
                                                                     "Planned is: lmer, glmer*, gbm")),
    defineParameter("analysisInterval", "numeric", 20, NA, NA, paste0("Interval that the least frequent raster is created",
                                                                     "i.e. birds = 20y"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "raster1Name", objectClass = "character", 
                 desc = "In the original context of the module, 'relativeSelectionTaigaPlains'"),
    expectsInput(objectName = "raster2Name", objectClass = "character", 
                 desc = "In the original context of the module, 'currentDiversityRasters")
    
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "raster1", objectClass = "RasterLayer | RasterStack", 
                  desc = "In the original context of the module, caribou RSF"),
    createsOutput(objectName = "raster2", objectClass = "RasterLayer | RasterStack", 
                  desc = "In the original context of the module, bird Diversity"),
    createsOutput(objectName = "corrDT", objectClass = "data.table", 
                  desc = "Data table with richness and RSF per pixel, per year")
  )
))

doEvent.correlateRasters = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      
      sim$raster3 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1KqXTekDyH5kzwk1jqu4gJSwV1ZoPGLW4", 
                           targetFile = "rasterEdehzie.tif",
                           destinationPath = outputPath(sim), rasterToMatch = sim$raster2)
      
            # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim), "correlateRasters", "loadData")
      sim <- scheduleEvent(sim, end(sim), "correlateRasters", "fitModel")
      sim <- scheduleEvent(sim, end(sim), "correlateRasters", "plot")
    },
    loadData = {
      # 1. Load the raster for a specific year: will use all data (use the interval to schedule!) + schedule the next event
      # 2. Extract data: stuck it in the same DT (SAVE THIS DT)
      # currentDiversityRasters_yearXXXX.rds (rasterStack) # INPUT FOLDER: /outputs/30JUL19/comMetrics
      # relativeSelectionTaigaPlains_YearXXXX.tif (raster) # INPUT FOLDER: /outputs/30JUL19/RSF
      sim$raster1 <- Cache(usefulFuns::createModObject, data = sim$raster1Name, sim = sim,
                           pathInput = inputPath(sim), currentTime = time(sim), fun = raster,
                           useCache = TRUE)
      sim$raster2 <- Cache(usefulFuns::createModObject, data = sim$raster2Name, sim = sim,
                           pathInput = inputPath(sim), currentTime = time(sim), fun = raster,
                           useCache = TRUE)
      sim$raster2 <- checkAndAlignData(raster1 = sim$raster1, raster2 = sim$raster2) # Changes raster2 in function of raster1 if needed
      
      currentCorrDT <- data.table(pixelID = 1:ncell(sim$raster1),
                               RSF = raster::getValues(x = sim$raster1), 
                               richness = raster::getValues(x = sim$raster2),
                               location = raster::getValues(x = sim$raster3),
                               year = paste0("year", time(sim)))
      
      sim$corrDT <- rbind(sim$corrDT,currentCorrDT)
      
      if (time(sim) == start(sim)){
        sim <- scheduleEvent(sim, end(sim), "correlateRasters", "loadData", eventPriority = 1)
      }
      sim <- scheduleEvent(sim, time(sim) + P(sim)$analysisInterval, "correlateRasters", "loadData")
      
    },
    fitModel = {
      # 3. fit the model: one model per pixel with all data. ==> This is because I have too much variations 
      #             accross landscape and I can't put it together. And to find "stable" hotspots I need pixel
      #             based. I can also make a summary per year, so I have a general idea of what is happening in
      #             a given area (shapefile)
      #             (MAYBE NOT GO THERE YET BEFORE MORE THINKING...) Stable hotspots are the ones where the relationship approximates linear the most...
      #             # FOR NOW: ASSUME THE RELATIONSHIP THROUGH TIME IS LINEAR
      #             Need to look at the plots and see if fitting a straight line makes sense
      #             use this instead: lme(RSF ~ richnessIndex + LCC, correlation = corAR1()) # Assumes measurements are equally spaced
      #             # SHOULD I REALLY USE LCC? I will first try without it
      # 4. Extract the slope coefficient ==> This indicates the relationship type:
      #          + beta --> Umbrella species
      #          - beta --> tradeoff
      # # test RSF ~ YEAR :: if significant, means RSF incorporates the time change and use the correlation argument in the model
      # so time correlation is potentially not necessarily a problem
      
      sim$autoCorr <- testForAutocorrelationInTime(corrDT = sim$corrDT, sampleSize = 4000) # About 14% of the pixels have TIME as significant in a lm RSF ~ TIME
      sim$fittedDT <- fitTheModel(corrDT = sim$corrDT)
      sim$correlationDT <- correlationModel(corrDT = sim$corrDT)
      sim$hotspotIndex <- hotspotIndexModel(corrDT = sim$corrDT)
    },
    plot = {
      # 5. Plot the relationship! I need to see the plot and the R2 and model significancy
      # 6. Make a final plot with the value of beta through time
      sim$estimateTable <- estimatePlot(fittedDT = sim$fittedDT)
      sim$estimatePlot <- ggplot(data = sim$estimateTable, aes(x = sim$estimateTable$year,
                                                               y = sim$estimateTable$richnessEstimate)) +
        geom_line() +
        labs(y = "Richness coefficient estimate with the model
             \n RSF ~ richness",
             x = "years")
      
      # NOW just for Edehzie
      sim$fittedDTList <- fitTheModel(corrDT = sim$corrDT, byLocation = TRUE)
      sim$estimateTableLocation <- estimatePlot(fittedDT = sim$fittedDTList, byLocation = TRUE)
      browser()
      sim$estimateTableLocation$location[sim$estimateTableLocation$location == "location2"] <- "Outside Edehzhie"
      sim$estimateTableLocation$location[sim$estimateTableLocation$location == "location1"] <- "Edehzhie"
      sim$estimatePlotLocation <- ggplot(data = sim$estimateTableLocation, 
                                         aes(x = year,
                                             y = richnessEstimate, 
                                             group=location, colour=location)) +
        geom_line() +
        labs(y = "Richness coefficient estimate with the model
             \n RSF ~ richness",
             x = "years") +
        theme(legend.title = element_blank(),
              legend.position = "bottom" )
      
      # 7. Map the hotspots: where do we have hotspots for multispecies conservation?
      message(crayon::blue("Preparing hotspot maps..."))
      sim$hotspotMap <- hotspotMapping(hotspotIndex = sim$hotspotIndex, ras = sim$raster1, 
                                       shp = "https://drive.google.com/open?id=15n9BOtswKCJ81-us1u8Dbs0WT9f8bagq",
                                       folder = outputPath(sim))
      
      # trade-off in relation to boreal bird's diversity? THIS IS PROBABLY NOT POSSIBLE. Would need
      # to have something as time series per pixel (correlation of RSF + rch throught time), 
      # which is not possible due to possible autocorrelation RSF & TIME. Can't do this one, but
      # can analyze this in general per decade (using the `estimatePlot`).
      # 8. Map the hotspots (positive beta vs negative beta) where we have caribou as a umbrela and as trade-off?
      # message(crayon::blue("Preparing umbrella/trade-off maps..."))
      # sim$umbrellaTradeoff <- umbrellaTradeoffMapping(hotspotIndex = sim$hotspotIndex, ras = sim$raster1)
      
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  if (!suppliedElsewhere('raster1Name', sim)) {
    sim$raster1Name <- "relativeSelectionTaigaPlains"
  }

  if (!suppliedElsewhere('raster2Name', sim)) {
    sim$raster2Name <- "richnessRaster_"
  }
  
  cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  return(invisible(sim))
}
