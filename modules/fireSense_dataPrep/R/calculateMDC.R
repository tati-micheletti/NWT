calculateMDC <- function(pathInputs, doughtMonths = 4:9, years, 
                         rasterToMatch = NULL, studyArea = NULL, 
                         returnTable = TRUE){
  library(data.table)
  variables <- c(paste0("Tmax0", doughtMonths), paste0("PPT0", 
                                                       doughtMonths))
  
  yearsList <- lapply(X = years, FUN = function(y){ # future
    
  fileName <- file.path(pathInputs, paste0("MDC_", y, ".grd"))
    if (file.exists(fileName)){
      return(raster::stack(fileName))
    } else {
      filesToLoad <- paste0(variables, ".asc")
      variablesStack <- raster::stack(lapply(X = filesToLoad, FUN = function(variable){
        ras <- raster(x = file.path(pathInputs, paste0("Year_", y, "M"), variable))
        crs(ras) <- '+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0'
        if (any(!is.null(rasterToMatch), !is.null(studyArea)))
          ras <- postProcess(x = ras, studyArea = studyArea, rasterToMatch = rasterToMatch,
                             filename2 = NULL)
        return(ras)
      })
      )
      # Fixing the layers for the values that were multiplied by 10 in ClimateNA V6.11
      # The variables to potentially fix are:
      # •	Annual: MAT, MWMT, MCMT, TD, AHM, SHM, EMT, EXT and MAR;
      # •	Seasonal: Tmax, Tmin, Tave and Rad;
      # •	Monthly: Tmax, Tmin, Tave and Rad.'
      variablesStack <- raster::stack(lapply(names(variablesStack), function(lay){
        if (lay %in% c("MAT", "MWMT", "MCMT", "TD", "AHM", "SHM", "EMT", "EXT", "MAR",
                       paste0("Tmax0", doughtMonths),
                       paste0("Tmin0", doughtMonths),
                       paste0("Tave0", doughtMonths),
                       paste0("Rad0", doughtMonths),
                       paste0("Rad_", c("wt", "sm", "at", "sp")),
                       paste0("Tmax_", c("wt", "sm", "at", "sp")),
                       paste0("Tmin_", c("wt", "sm", "at", "sp")),
                       paste0("Tave_", c("wt", "sm", "at", "sp")))){
          message(crayon::red(paste0("ClimateNA 6.11 multiplies ", lay, " by 10 for storage.",
                                     "Backtransforming the layer")))
          variablesStack[[lay]] <- variablesStack[[lay]]/10
          return(variablesStack[[lay]])
        } else {
          return(variablesStack[[lay]])
        }
      }))
      
      # Day length adjustement L_f in Drought Code (taken from Van Wagner 1987)
      L_f <- function(Month){
        c('4' = 0.9,
          '5' = 3.8,
          '6' = 5.8,
          '7' = 6.4,
          '8' = 5.0,
          '9' = 2.4)[[as.character(Month)]] # TODO [ FIX ] Update for all Months, check latitude problem. Ideally, bring original table in here.
      }
      
      nDays <- function(Month){
        c('4' = 30,
          '5' = 31,
          '6' = 30,
          '7' = 31,
          '8' = 31,
          '9' = 30)[[as.character(Month)]]
      }
      
      # remove the variables from rasterStack for faster operations
      dt <- na.omit(data.table(raster::getValues(variablesStack), pixelID = 1:ncell(variablesStack)))
      dt[,MDC_0 := 0]
      for (Month in doughtMonths){
        dt[, MDC_m := pmax(MDC_0 + .25 * nDays(Month) * (.36 * eval(parse(text = paste0("Tmax0", Month))) + L_f(Month)) -
                             400 * log(1 + 3.937 * .83 * eval(parse(text = paste0("PPT0", Month))) / (800 * exp(-MDC_0/400))) +
                             .25 * nDays(Month) * (.36 * eval(parse(text = paste0("Tmax0", Month))) + L_f(Month)),0)]
        dt[, MDC_0 := pmax((MDC_0 + MDC_m) / 2, 0)]
      }
      # Set new raster variable to raster
      MDC <- merge(data.table(pixelID = 1:ncell(variablesStack)),
                   dt[, c("pixelID", "MDC_0")], by = "pixelID", all.x = TRUE)
      setkey(MDC, pixelID)
      variablesStack <- raster::setValues(x = variablesStack[[1]], values = MDC$MDC_0)
      names(variablesStack) <- paste0("MDC_", y)
      dType <- assessDataType(variablesStack)
      writeRaster(variablesStack, filename = fileName, datatype = dType, overwrite = TRUE)
      variablesStack <- raster::stack(fileName)
      return(raster::stack(fileName))
    }
})
  names(yearsList) <- paste0("Year", years)
  return(yearsList)

}
