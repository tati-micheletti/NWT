extractRasFromPolys <- function(year, rasList, polyList){
  library(data.table)
  if (raster::nlayers(rasList) == 1) # For now, not sure it works on a list of rasters because of the matrix it becomes when extracting. MAR 9th 2020 ~TM
    rasList <- rasList[[1]]
  extracted <- data.table(Cache(raster::extract, x = rasList, y = polyList, cellnumbers = TRUE, df = TRUE,
                                userTags = c("fun:extractRasFromPolys", paste0("year:", year))))
  names(extracted)[names(extracted) == "cell"] <- "pixelID"
  extracted$year <- usefun::substrBoth(strng = year, howManyCharacters = 4, fromEnd = TRUE)
  return(extracted) # Returns now a data.table of pixelID, rasterValue and year (i.e. biomass or MDC)
}
