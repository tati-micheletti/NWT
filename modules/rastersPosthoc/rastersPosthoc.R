defineModule(sim, list(
  name = "rastersPosthoc",
  description = paste0("Posthoc module for timse series of rasters (predictions). Prepares: ",
                       "1) rasters of difference between last and fist year (relative or absolute) - delta rasters, ",
                       "2) calculates significant changes of list of rasters per year (i.e. species) using bootstrapping through pixels, ", 
                       "3) Makes a summary of the pixels values, ",
                       "4) Make a GIF with predictions rasters"),
  keywords = c("rasters", "posthoc analysis", "plots", "predictions"),
  authors = c(person("Tati", "Micheletti", email = "tati.micheletti@gmail.com", role = c("aut", "cre"))),
  childModules = character(0),
  version = list(SpaDES.core = "0.2.6.9006", rastersPosthoc = "0.0.1"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "rastersPosthoc.Rmd"),
  reqdPkgs = list("raster", "data.table", "future", "future.apply", "tati-micheletti/usefun"),
  parameters = rbind(
    defineParameter("species", "character", NULL, NA, NA, paste0("Which species to use? If NULL, it tries guessing which species are available. ",
                                                                 "However, this only works if all species were ran for all years")),
    defineParameter("years", "numeric", c(seq(2011, 2100, by = 10), 2100), NA, NA, paste0("Years to use in the functions. For the delta rasters, ", 
                                                                                          "it will use the first and last years supplied here")),
    defineParameter("relativeDelta", "logical", TRUE, NA, NA, "Should the delta rasters be relative to the first one?"),
    defineParameter("patternsToRetrieveRasters", "character", NULL, NA, NA, "Which patterns should be used to find the rasters in the dataFolder?"),
    defineParameter("patternsUsedForGrouping", "character", NULL, NA, NA, paste0("Which patterns should be used to group the rasters?",
                                                                               "Should be passed as a vector if more than one. If 'NULL', ",
                                                                               "it uses all rasters as one group")),
    defineParameter("sampleSize", "character|numeric", "auto", NA, NA, paste0("How many pixels should be used for the bootstrapping as sample size? ",
                                                                    "If 'auto', it uses cohen.d() to calculate an ideal sample size ",
                                                                    "that reduces the effect of big sizes on significancy")),
    defineParameter("n", "numeric", 2, NA, NA, paste0("How many repetitions for the bootstrapping should be run?",
                                                      "The higher the repetition number, the longer it will take to run")),
    defineParameter("makeRasterChangeGIF", "logical", FALSE, NA, NA, paste0("Should the gif be made? It takes a bit of time if TRUE", 
                                                                            " AS OF 30th OCT 19 IT IS STILL NOT IMPLEMENTED")), 
    defineParameter("overwriteDelta", "logical", FALSE, NA, NA, paste0("Should the deltas maps be overwritten?")),
    defineParameter("makeRSFLikePlot", "logical", FALSE, NA, NA, paste0("Should it make the plots like ECCC2011 RSF?")),
    defineParameter("uploadPlots", "logical", FALSE, NA, NA, paste0("Should the plots be uploaded to ggdrive?")),
    defineParameter("typeOfAnalysis", "character", "generic", NA, NA, paste0("Type of analysis for naming purposes")),
    defineParameter("calculateSignificantChanges", "logical", TRUE, NA, NA, paste0("Should it calculate significant changes",
                                                                                   " between the first and last rasters? ",
                                                                                   "It might take a few minutes to hours depending ",
                                                                                   " on your system")),
    defineParameter("calculateSummary", "logical", TRUE, NA, NA, paste0("Should it calculate summary of changes?",
                                                                                   "It might take a few minutes to hours depending ",
                                                                                   " on your system")),
    defineParameter("plotCI", "logical", FALSE, NA, NA, paste0("Should it plot the CI",
                                                               " on the average through time plots? Remember there is a lot of spatial 
                                                               variation"))
  ),
  inputObjects = bind_rows(
    expectsInput(objectName = "dataFolder", objectClass = "list", 
                 desc = paste0("This is a named list of the folders where the results should be.",
                               " (i.e. `folders[['LandR.CS_fS']] = file.path(getwd(),'outputs/DATE/LandR.CS_fS/birdPrediction')`)",
                               " Pass as a vector of locations for running the functions in more than one location, as it",
                               " lapplies through it internally. The names should be the type of simulation ran. If this ",
                               "is not supplied, you have to supply the list of rasters, organized by "), 
                 sourceURL = NA),
    expectsInput(objectName = "listOfRasters", objectClass = "list", 
                 desc = paste0("This is a named 3 level list of the 1) location or simulation type, 2) objects to lapply through (i.e. can be a list of bird species, which one ",
                               "containing the 3) time series of rasters to analyze). This list can also be of just one location/simulation, and/or one species (i.e. caribou)",
                               " or another 'object' (i.e. temperature)"),
                 sourceURL = NA),
    expectsInput(objectName = "googleFolders", objectClass = "list", 
                 desc = paste0("This is a named folder to upload the results. There are no defaults, needs to be provided"),
                 sourceURL = NA)
  ),
  outputObjects = bind_rows(
    createsOutput(objectName = "deltaRasters", objectClass = "list", desc = "List of delta rasters"),
    createsOutput(objectName = "significantChanges", objectClass = "list", desc = paste0("List of several results showing which one ",
                                                                                                  "(i.e. species, if a list) significantly increased,", 
                                                                                                  "decreased or didn't change")),
    createsOutput(objectName = "pixelsSummaries", objectClass = "list", desc = "Summary of pixelValues mean, max, min, median per year"), #TODO CHECK!!! if this is abundance or
    createsOutput(objectName = "gifFigure", objectClass = "list", desc = "List of gifFigures delta rasters")
    )
))

doEvent.rastersPosthoc = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "makeDeltaRasters")
      if (P(sim)$calculateSignificantChanges)
        sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "calculatesSignificantChanges")
      if (P(sim)$calculateSummary)
        sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "makeSummary")
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "makeGIF") # Currently not implemented
      if (P(sim)$makeRSFLikePlot)
        sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "generateRSFbinned")
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "averageThroughTime")
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "averageThroughTimeComparison")
      },
    makeDeltaRasters = {
      sim$deltaRasters <- makeDeltaRasters(listOfRasters = sim$listOfRasters, 
                                           relativeDelta = P(sim)$relativeDelta, 
                                           years = P(sim)$years,
                                           outputFolder = getPaths()$outputPath,
                                           upload = P(sim)$uploadPlots,
                                           folderID = sim$googleFolders,
                                           overwrite = P(sim)$overwriteDelta)
    },
    calculatesSignificantChanges = {
      sim$significantChanges <- bootstrapPercentChanges(dataPath = sim$dataFolder, #TODO MODIFY THIS TO ACCEPT A LIST OF LISTS OF RASTER INSTEAD OF DATA FOLDER?
                                                        years = P(sim)$years, 
                                                        sampleSize = P(sim)$sampleSize, 
                                                        n = P(sim)$n, species = P(sim)$species,
                                                        useFuture = TRUE)
      },
    makeSummary = {
      sim$pixelsSummaries <- makeRastersSummary(listOfRasters = sim$listOfRasters, 
                                                years = P(sim)$years)
    },
    makeGIF = {
      #TODO sim$gif Figure NOT YET IMPLEMENTED. DON'T HAVE A GENERIC FUNCTION TO GENERATE THE GIF FROM R
    },
    generateRSFbinned = {
      sim$RSFlikePlot <- lapply(X = names(sim$listOfRasters), FUN = function(scenarios){ # [FIX] future_lapply
        ras <- tryCatch(sim$listOfRasters[[scenarios]][[usefun::grepMulti(x = names(sim$listOfRasters[[scenarios]]), patterns = "SelectionTaiga")]], 
                        error = function(e){stop("This event can only be ran for Caribou, which has to have the 'SelectionTaiga' in its raster name")})
        RSFlikePlot <- usefun::RSFplot(ras = ras,
                                       upload = P(sim)$uploadPlots,
                                       writeReclasRas = TRUE,
                                       outputFolder = getPaths()$outputPath,
                                       rasName = paste0("caribouBinned", scenarios),
                                       folderID = sim$googleFolders[[scenarios]])
        })
      },
    averageThroughTime = {
      sim$averageInTime <- future_lapply(X = names(sim$listOfRasters), FUN = function(scenarios){
       allRasInScenarios <- future_lapply(X = sim$listOfRasters[[scenarios]],
               FUN = function(index){
                 usefun::meanValuesTime(ras = index,
                                scenario = scenarios,
                                initialTime = P(sim)$years)
               })
      })
    },
    averageThroughTimeComparison = {
      if (is.null(sim$averageInTime)) stop("Average through time comparison can only be run if sim$averageInTime is not NULL")
      sim$averageComparison <- avrgTimeComparison(sim$averageInTime,
                                            upload = P(sim)$uploadPlots,
                                            outputFolder = getPaths()$outputPath,
                                            comparisonID = P(sim)$typeOfAnalysis,
                                            folderID = sim$googleFolders[[1]],
                                            plotCI = P(sim)$plotCI)
      },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

.inputObjects <- function(sim) {

  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  if(!suppliedElsewhere("dataFolder", sim)){
    if(!suppliedElsewhere("listOfRasters", sim)){
      stop("You must supply either a listOfRasters or a dataFolder")
    }
  }
  
  if(!suppliedElsewhere("listOfRasters", sim)){
    if(!suppliedElsewhere("dataFolder", sim)){
      stop("You must supply either a listOfRasters or a dataFolder")
    }
    # get the rasters if they are not provided! 
    sim$listOfRasters <- retrieveRasters(dataFolder = sim$dataFolder,
                                         years = P(sim)$years, 
                                         patternsToRetrieveRasters = P(sim)$patternsToRetrieveRasters, 
                                         patternsUsedForGrouping = P(sim)$patternsUsedForGrouping) #TODO fun Warning for NULL patterns!
    }
  if (isTRUE(P(sim)$uploadPlots)){
    if(!suppliedElsewhere("googleFolders", sim)){
      stop("If you set upload to TRUE, you need to provide a named folder (i.e. scenarios) with the google ids for uploading")
    }
  }
  
  return(invisible(sim))
}
