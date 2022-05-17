# Make map for RSF from GNWT
library(reproducible)

RSF <- Cache(prepInputs, targetFile = "GNWT_RSF_250m.tif",
             archive = "GNWT_RSF_250m.zip",
             alsoExtract = "similar",
             url = "https://drive.google.com/file/d/1srpkmgCq6CORPGAtbRXIka1TW4fI3_NQ",
             destinationPath = Paths$inputPath,
             filename2 = "GNWT_RSF_250m",
             fun = "raster::raster",
             userTags = c(stepCacheTag,
                          "outFun:Cache", "step:GNWT_RSFcaribou"))

rasName <- "GNWT_relativeSelectioncaribouRSF_NT_Year2017"

pngPath <- file.path(Paths$outputPath, paste0(rasName, ".png"))

library("lattice")
library("rasterVis")
library("viridis")
library("maptools")
png(filename = pngPath,
    width = 21, height = 29,
    units = "cm", res = 300)

pathSHP <- file.path(Paths$inputPath, "RSFshp.shp")
if (!file.exists(pathSHP)){
  rgdal::writeOGR(obj = shp, dsn = Paths$inputPath, "RSFshp", 
                  driver = "ESRI Shapefile")
}
shpLoaded <- maptools::readShapeLines(pathSHP)
shpProj <- raster::crs(RSF)

# Add shp to levelplot
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
  rasBinned <- ratify(RSF) 
  att <- "ID"

  print(rasterVis::levelplot(rasBinned,
                           sub = paste0("Caribou RSF in 2017 (GNWT version)"),
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

