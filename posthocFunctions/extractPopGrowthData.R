extractPopGrowthData <- function(currentTime, # OK
                                   resultsMainFolder = NULL, # Pass this if outside of module # OK
                                   climateModel = NULL, # OK
                                   predictedCaribou = NULL,
                                   reps = paste0("run", 1:5), # OK
                                   outputFolder,
                                   whichPolys = NULL, # Optional to ensure only specific polygons to be plotted
                                   timeSpan = "annual"){ # Optional = "timeStep" (normally every 10y)
  
if (!is.null(resultsMainFolder)){
  allcombs <- data.table(expand.grid(climateModel, reps))
  allcombs[, comb := paste0(Var1, "_",Var2)]
  pth <- file.path(resultsMainFolder, allcombs[["comb"]])
  
  predictedCaribou <- rbindlist(lapply(seq_along(pth), function(filePathIndex){
    tb <- readRDS(list.files(path = pth[filePathIndex], 
                             pattern = paste0("predictedCaribou_year", currentTime), 
                             full.names = TRUE, recursive = TRUE))
    addedTB <- rbindlist(lapply(names(tb), function(years){
      TB <- tb[[years]]
      climMod <- strsplit(basename(pth[filePathIndex]), "_")[[1]][1]
      replic <- strsplit(basename(pth[filePathIndex]), "_")[[1]][2]
      TB[, c("climateModel", "Replicate", "Year") := list(climMod, 
                                                          replic, 
                                                          usefulFuns::substrBoth(years, 4, T))]
      return(TB)
    }))
    return(addedTB)
  }))
  
  if (!is.null(whichPolys)){
    predictedCaribou <- predictedCaribou[Herd %in% whichPolys, ]
  }
}
  tableAll <- predictedCaribou
  
yaxis <- if (timeSpan == "annual") "annualLambda" else "growth"

names(tableAll)[names(tableAll) == "Herd"] <- "Polygon"
tableAll[, minRib := min(get(paste0(yaxis, "Min"))), by = c("Year", "Polygon", 
                                                            "climateModel", "femSurvMod_recrMod")]
tableAll[, maxRib := max(get(paste0(yaxis, "Max"))), by = c("Year", "Polygon", 
                                                            "climateModel", "femSurvMod_recrMod")]
tableAll[, paste0("average", yaxis) := mean(get(yaxis)), by = c("Year", "Polygon", "climateModel", 
                                                                "femSurvMod_recrMod")]
write.csv(x = tableAll, file = file.path(pathOutputs, "populationGrowthTable.csv"))
return(tableAll)
}