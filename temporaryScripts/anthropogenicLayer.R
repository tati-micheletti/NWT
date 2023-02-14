library("Require")
Require("reproducible")

anthropogenicLayers <- prepInputs(targetFile = "anthropogenicDisturbanceLayers_NT1_BCR6.grd",
                             archive = "anthropogenicDisturbanceLayers_NT1_BCR6.zip",
                             alsoExtract = "anthropogenicDisturbanceLayers_NT1_BCR6.gri",
                             url = "https://drive.google.com/file/d/1npwXsabARoLeGKNKhdC_7j-OJSKCOJdC/view?usp=sharing",
                             destinationPath = tempdir(),
                             fun = "raster::stack")

library(magrittr)
library(sf)
library(raster)
library(reproducible)

studyAreaLarge <- prepInputs(url = 'https://drive.google.com/file/d/1LxacDOobTrRUppamkGgVAUFIxNT4iiHU/view?usp=sharing',
                             destinationPath = "data",
                             overwrite = TRUE,
                             useCache = 'overwrite',
                             FUN = 'sf::st_read') %>%
  sf::st_as_sf(.)

studyAreaCaribou <- prepInputs(targetFile = "NT1_BCR6.shp",
                          archive = "NT1_BCR6.zip",
                          alsoExtract = "similar",
                          url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/view?usp=sharing",
                          destinationPath = tempdir(),
                          fun = "raster::shapefile")
