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
                 sourceURL = "https://drive.google.com/open?id=1sQBdeCyWvVH-aYcWNGyo6w8x6RNYMJgk"),
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
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "testStudyArea")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "createWetZone")
      sim <- scheduleEvent(sim, start(sim), "waterlandClassification", "diagnostics")
    },
    testStudyArea = {
      
      browser()
      # 2. Test that the study area is inside the DUCKS layer. 
      # If not, return warning that only the area (%?) is inside and will be assessed
      
      },
    createWetZone = {
    
      sim$wetLCC <- classifyWetlands(LCC = P(sim)$baseLayer,
                                     wetLayerInput = sim$rasterDUCKS) # Create the function
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
    sim$studyArea <- SpaDES.tools::randomPolygon(x = matrix(-119.226553, 62.528916, ncol = 2), 
                                                 area = 10^5)
  if (!suppliedElsewhere("wetlandRaster", sim)){
    message("wetlandRaster not supplied, default is Hybrid Wetland from DUCKS Unlimited Canada")
    if (!suppliedElsewhere("RTM", sim))
      message("RTM not supplied, wetlandRaster will not be reprojected nor resampled")
    # [ FIX ] prepInputs for it to download and read ".gdb"
    browser()
    message(paste0("googledrive package still doesn't support *.gdb files.", 
            " \nPlease, download the file manually and place it \n", dPath, ".\n",
            "Download from: ", extractURL(objectName = "wetlandRaster"))) 
            invisible(readline(prompt = "Press [ ENTER ] when ready..."))
            reproducible::Checksums(path = dPath, write = TRUE)
            # [ FIX ] reproducible is not loading the file...
            # ???
            browser()
            prepSpeciesLayers_DUCKS()
            
            sim$wetlandRaster <- rgdal::readOGR(dsn = file.path(dPath), layer = "HybridWetlandLayer2_1")
            # (studyArea = studyArea, rasterToMatch = sim$RTM)
      sim$wetlandRaster <- reproducible::prepInputs(url = "https://drive.google.com/open?id=1MglGyEw3ajuFB9Unu1s8NCZFnPlVxahE",
                                                    archive = "HybridWetlandLayer2_1.zip",
                                                    dsn = dPath, layer = "HybridWetlandLayer2_1",
                                                    destinationPath = dPath, fun = "rgdal::readOGR",
                                                    userTags = c(cacheTags, "objectName:wetlandRaster"))
  }

  if (is.null(P(sim)$baseLayer))
    P(sim)$baseLayer  <- c("LCC05", "LCC10")

  return(invisible(sim))
}