makeCaribouAverageTS <- function(climateScenarios,
                                 years = c(seq(2011, 2091, by = 20), 2100),
                                 reps = paste0("run", 1:5),
                                 pathData,
                                 pathInputs,
                                 pathOutputs,
                                 binningTable,
                                 overwriteRas = FALSE,
                                 overwritePNG = FALSE){
 Require("raster")
  # For each CS
  eachCS <- lapply(climateScenarios, function(CS){
    tic(paste0("Calculating average RSF change for ", CS))
    allYears <- lapply(years, function(Y){
      # FOR EACH YEAR, BRING ALL RUNS AND ... 
      allRuns <- stack(lapply(reps, function(RUN){
        boo <- raster::raster(usefulFuns::grepMulti(list.files(file.path(pathData, 
                                                          paste(CS, RUN, 
                                                                sep = "_"), 
                                                          "caribouPredictions"), 
                                                full.names = TRUE), 
                                     patterns = c("relativeSelectioncaribouRSF_NT", 
                                                    Y, ".tif")))
        boo[] <- boo[]
        # Need to bin these values!
        reclassMatrix <- matrix(cbind(binningTable[["Min.Value"]],
                                      binningTable[["Max.Value"]],
                                      binningTable[["RSF.Bin"]]), 
                                ncol = 3)
        # Make sure that the max value is infinite, so it accommodates any bigger value
        # than before
        reclassMatrix[nrow(reclassMatrix), 2] <- Inf
        rasBinned <- raster::reclassify(x = boo, 
                                        rcl = reclassMatrix)
        return(rasBinned)
  }))
      # ... CALCULATE AVERAGE
      aveBooPath <- file.path(pathOutputs, paste("averageRSF",
                                                       CS, "Year", Y,
                                                       sep = "_"))
      sdBooPath <- file.path(pathOutputs, paste("sdRSF",
                                                 CS, "Year", Y,
                                                 sep = "_"))
      if (any(!file.exists(paste0(aveBooPath, ".tif")), 
              !file.exists(paste0(sdBooPath, ".tif")), 
              overwriteRas)){
        message(paste0("Calculating average RSF bin for ", CS))
        relativeSelection <- calc(x = allRuns, fun = mean, 
                       na.rm = TRUE,
                       filename = aveBooPath,
                       overwrite = TRUE,
                       format = "GTiff")
        message(paste0("Calculating sd RSF bin for ", CS))
        relativeSelectionSD <- calc(x = allRuns, fun = sd, 
                      na.rm = TRUE,
                      filename = sdBooPath,
                      overwrite = TRUE,
                      format = "GTiff")
      } else {
        relativeSelection <- raster(paste0(aveBooPath,".tif"))
        relativeSelectionSD <- raster(paste0(sdBooPath, ".tif"))
      }
      
      # Make the png
      rasList <- lapply(X = c("relativeSelection", "relativeSelectionSD"), FUN = function(r){
        Ras <- get(r)
      pngPath <- file.path(pathOutputs, paste0(CS, "_", r, "Average","_Year", Y, ".png"))
      if (any(overwritePNG,
              !file.exists(pngPath))){
        library("lattice")
        library("rasterVis")
        library("viridis")
        library("maptools")
        png(filename = pngPath,
            width = 21, height = 29,
            units = "cm", res = 300)
        
        pathSHP <- file.path(pathInputs, "RSFshp.shp")
        if (!file.exists(pathSHP)){
          rgdal::writeOGR(obj = shp, dsn = Paths$inputPath, "RSFshp", 
                          driver = "ESRI Shapefile")
        }
        shpLoaded <- maptools::readShapeLines(pathSHP)
        
        # Add shp to levelplot
        if (r == "relativeSelection"){
          Pal <- c("#c2473d",
                   "#d96b52",
                   "#e1936b",
                   "#f4bd81",
                   "#f8e8b0",
                   "#ecedc2",
                   "#bec3c7",
                   "#95a3ca",
                   "#6c7ed0",
                   "#3260c6") # Handmade to match DeMars 2019
          Ras <- round(Ras, 0)
          rasBinned <- ratify(Ras)
          att <- "ID"
          txt <- "Average"
        } else {
          rasBinned <- Ras
          Pal <- c(brewer.pal(9, "YlOrRd"), "#510218")
          att <- NULL
          txt <- "SD"
        }
        print(rasterVis::levelplot(rasBinned,
                                   sub = paste0(txt, " caribou RSF in ", Y, 
                                                " for ", CS),
                                   att = att,
                                   margin = FALSE,
                                   maxpixels = 6e6,
                                   colorkey = list(
                                     space = 'bottom',
                                     at = 1:10,
                                     axis.line = list(col = 'black'),
                                     width = 0.75
                                   ),
                                   par.settings = list(
                                     strip.border = list(col = 'transparent'),
                                     strip.background = list(col = 'transparent'),
                                     axis.line = list(col = 'transparent')),
                                   scales = list(draw = FALSE),
                                   col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                                   par.strip.text = list(cex = 0.8,
                                                         lines = 1,
                                                         col = "black"),
                                   panel = function(...){
                                     lattice::panel.levelplot.raster(...)
                                     sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
                                   }))
        
        dev.off()
      } else {
        print(paste0("The file ", pngPath, 
                     " exists and overwritePNG is FALSE"))
      }
      })
      return(allRunsStack = allRuns)
    })
    names(allYears) <- paste0("Year", years)
    return(allYears)
})
  names(eachCS) <- climateScenarios
  # For all CS
  rasOrganized <- lapply(years, function(Y){
    yearRas <- raster::stack(lapply(eachCS, `[[`, paste0("Year", Y)))
    aveBooPath <- file.path(pathOutputs, paste0("averageRSF_allCS_Year", Y, ".tif"))
    sdBooPath <- file.path(pathOutputs, paste0("sdRSF_allCS_Year", Y, ".tif"))
    if (any(!file.exists(aveBooPath), !file.exists(sdBooPath), 
            overwriteRas)){
      message(paste0("Calculating average RSF bin for year ", Y))
      relativeSelection <- calc(x = yearRas, fun = mean, 
                                na.rm = TRUE,
                                filename = aveBooPath,
                                overwrite = TRUE,
                                format = "GTiff")
      message(paste0("Calculating sd RSF bin for year ", Y))
      relativeSelectionSD <- calc(x = yearRas, fun = sd, 
                                  na.rm = TRUE,
                                  filename = sdBooPath,
                                  overwrite = TRUE,
                                  format = "GTiff")
    } else {
      relativeSelection <- raster(aveBooPath)
      relativeSelectionSD <- raster(sdBooPath)
    }
    
    rasList <- lapply(X = c("relativeSelection", "relativeSelectionSD"), FUN = function(r){
      Ras <- get(r)
      pngPath <- file.path(pathOutputs, paste0(r, "_allCS_Year", Y, ".png"))
      if (any(overwritePNG,
              !file.exists(pngPath))){
        library("lattice")
        library("rasterVis")
        library("viridis")
        library("maptools")
        library("RColorBrewer")
        
        png(filename = pngPath,
            width = 21, height = 29,
            units = "cm", res = 300)
        
        pathSHP <- file.path(pathInputs, "RSFshp.shp")
        if (!file.exists(pathSHP)){
          rgdal::writeOGR(obj = shp, dsn = Paths$inputPath, "RSFshp", 
                          driver = "ESRI Shapefile")
        }
        shpLoaded <- maptools::readShapeLines(pathSHP)
        
        # Add shp to levelplot
        if (r == "relativeSelection"){
          Pal <- c("#c2473d",
                   "#d96b52",
                   "#e1936b",
                   "#f4bd81",
                   "#f8e8b0",
                   "#ecedc2",
                   "#bec3c7",
                   "#95a3ca",
                   "#6c7ed0",
                   "#3260c6") # Handmade to match DeMars 2019
          # round these up to the integer
          Ras <- round(Ras, 0)
          rasBinned <- ratify(Ras)
          att <- "ID"
          txt <- "Average"
        } else {
          rasBinned <- Ras
          # Pal <- viridis_pal(option = "D")(10) 
          Pal <- c(brewer.pal(9, "YlOrRd"), "#510218")
          att <- NULL
          txt <- "SD"
        }
        print(rasterVis::levelplot(rasBinned,
                                   sub = paste0(txt, " caribou RSF ",
                                                "in ", Y, " across climate ",
                                                "scenarios"),
                                   att = att,
                                   margin = FALSE,
                                   maxpixels = 6e6,
                                   colorkey = list(
                                     space = 'bottom',
                                     at = 1:10,
                                     axis.line = list(col = 'black'),
                                     width = 0.75
                                   ),
                                   par.settings = list(
                                     strip.border = list(col = 'transparent'),
                                     strip.background = list(col = 'transparent'),
                                     axis.line = list(col = 'transparent')),
                                   scales = list(draw = FALSE),
                                   col.regions = Pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                                   par.strip.text = list(cex = 0.8,
                                                         lines = 1,
                                                         col = "black"),
                                   panel = function(...){
                                     lattice::panel.levelplot.raster(...)
                                     sp::sp.polygons(shpLoaded, fill = 'black', lwd = 1)
                                   }))
        
        dev.off()
      } else {
        print(paste0("The file ", " exists and overwritePNG is FALSE"))
      }
    })
    return(list(meanPath = aveBooPath, 
           sdPath = sdBooPath))
  })
  names(rasOrganized) <- paste0("Years", years)
  return(rasOrganized)
}
