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
  reqdPkgs = list("raster", "data.table"),#, "tati-micheletti/usefun"),
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
    defineParameter("overwriteDelta", "logical", FALSE, NA, NA, paste0("Should the deltas maps be overwritten?"))
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
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "calculatesSignificantChanges")
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "makeSummary")
      sim <- scheduleEvent(sim, start(sim), "rastersPosthoc", "makeGIF")
    },
    makeDeltaRasters = {
      sim$deltaRasters <- makeDeltaRasters(listOfRasters = sim$listOfRasters, 
                                           relativeDelta = P(sim)$relativeDelta, 
                                           years = P(sim)$years,
                                           outputFolder = getPaths()$outputPath,
                                           overwrite = P(sim)$overwriteDelta)
    },
    calculatesSignificantChanges = {

      sim$significantChanges <- bootstrapPercentChanges(dataPath = sim$dataFolder, #TODO MODIFY THIS TO ACCEPT A LIST OF LISTS OF RASTER INSTEAD OF DATA FOLDER?
                                                        years = P(sim)$years, 
                                                        sampleSize = P(sim)$sampleSize, 
                                                        n = P(sim)$n, species = P(sim)$species)
      },
    makeSummary = {
      sim$pixelsSummaries <- makeRastersSummary(listOfRasters = sim$listOfRasters, 
                                                years = P(sim)$years)
    },
    makeGIF = {
      #TODO sim$gifFigure NOT YET IMPLEMENTED. DON'T HAVE A GENERIC FUNCTION TO GENERATE THE GIF FROM R
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
  return(invisible(sim))
}
