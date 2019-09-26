compareAveragesInOutSHP <- function(shp, folder, raster1Name, raster2Name = NULL, targetFile = NULL, 
                                    years, useSE = FALSE, facetPlot = FALSE, plotMedian = TRUE){
  
  if (class(shp) == "character"){
    studyArea <- Cache(prepInputs,
                       url = shp, targetFile = targetFile,
                       destinationPath = folder,
                       userTags = "studyArea", filename2 = NULL,
                       omitArgs = c("destinationPath"))
  } else {
    studyArea <- shp
  }
  
  rasList <- lapply(years, function(y){
    ras <- usefun::grepMulti(x = list.files(folder, full.names = TRUE, recursive = TRUE),
                             patterns = c(raster1Name, y))
    ras1 <- raster::raster(ras)
    return(ras1)
  })
  if (!is.null(raster2Name)){
    rasList2 <- lapply(years, function(y){
      ras <- usefun::grepMulti(x = list.files(folder, full.names = TRUE, recursive = TRUE),
                               patterns = c(raster2Name, y))
      ras1 <- raster::raster(ras)
      ras1 <- Cache(reproducible::postProcess, x = ras1, rasterToMatch = rasList[[1]], 
                    destinationPath = folder, filename2 = NULL,
                    useCache = TRUE)
      return(ras1)
    })
    rasList <- c(rasList, rasList2)
    names(rasList) <- sapply(rasList, names)
  }
  # Making inside and out of Study Area
  if (!is(studyArea, "RasterLayer")) {
    studyArea <- postProcess(studyArea, rasterToMatch = rasList[[1]],
                       filename2 = NULL, destinationPath = folder, useCache = TRUE)
    studyAreaSF <- sf::st_as_sf(studyArea)
    studyAreaSF$ID <- as.double(1:length(studyAreaSF$Name))
    sAras <- fasterize::fasterize(sf = studyAreaSF, raster = rasList[[1]], field = "ID")
    
    sAras <- postProcess(sAras, rasterToMatch = rasList[[1]], maskWithRTM = TRUE,
                         filename2 = NULL, destinationPath = folder, useCache = TRUE)
    sAras[is.na(sAras)] <- 2
    sAras <- postProcess(sAras, rasterToMatch = rasList[[1]], maskWithRTM = TRUE, format = "GTiff",
                         filename2 = file.path(folder, "rasterEdehzie"), destinationPath = folder, useCache = TRUE)
  } else {
    sAras <- studyArea
  }
  library("data.table")
  pixelID <- data.table::data.table(pixelID = 1:ncell(rasList[[1]]))
  dt <- usefun::cbindFromList(lapply(rasList, raster::getValues))
  names(dt) <- unlist(lapply(rasList, names))
  studyArea <-  data.table::data.table(studyArea = raster::getValues(x = sAras))
  dt <- cbind(pixelID, dt, studyArea)
  dt <- na.omit(dt)
  dt <- (dt)
  plotDT <- data.table(Reduce(merge, lapply(c("mean", "median", "min", "max", ifelse(useSE, "se", "sd")), function(fun){
    se <- function(x, na.rm) sd(x, na.rm)/sqrt(sum(!is.na(x))) # ==> standard error that ignores NA values
    tb <- dt[, lapply(.SD, get(fun), na.rm = TRUE), by = studyArea, .SDcols = unlist(lapply(rasList, names))]
    tb[["operation"]] <- paste0("F", fun)
    tb[, pixelID := NULL]
    library("reshape2")
    tb2 <- melt(data = tb, id = c("studyArea", "operation"))
    tb2$year <- usefun::substrBoth(strng = as.character(tb2[,variable]), howManyCharacters = 4, fromEnd = TRUE)
    tb2$index <- usefun::substrBoth(strng = as.character(tb2[,variable]), howManyCharacters = 8, fromEnd = FALSE)
    tb2[, variable := NULL]
    tb2$studyArea[tb2$studyArea == 2] <- "Outside Edehzhie"
    tb2$studyArea[tb2$studyArea == 1] <- "Edehzhie"
    tb3 <- dcast(tb2, formula = studyArea + year + index ~ operation)
    names(tb3)[names(tb3) == "index"] <- "ind"
    return(tb3)
  })
  ))
  plotDT <- na.omit(plotDT, cols = "studyArea")
  if (useSE)
    names(plotDT)[names(plotDT) == "Fse"] <- "Fsd"
  
  library("ggplot2")
  lapply(unique(plotDT$ind), function(grph){
    if (!facetPlot){
      dt <- plotDT[ind == grph,]
    } else {
      dt <- plotDT
    }
    plotDT$ind[plotDT$ind == "relative"] <- "RSF"
    plotDT$ind[plotDT$ind == "richness"] <- "bird richness"
    plotDT <- data.frame(plotDT)
    lowLim <- round(min(plotDT$Fmean) - 0.08*mean(plotDT$Fmean), 1)
    upLim <- round(max(plotDT$Fmean) + 0.05*mean(plotDT$Fmean), 1)
    barPlots <- ggplot(data = plotDT, 
                       aes(x = as.factor(year), y = Fmean, 
                           ymax = Fmean + Fsd,
                           ymin = Fmean - Fsd, 
                           fill = studyArea)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      geom_errorbar(width = 0.2, position = position_dodge(.9)) +
      labs(x = "Year", y = paste0("Average value")) +
      theme_bw() +
      theme(legend.title = element_blank(),
            legend.position = "bottom") +
      coord_cartesian(ylim = c(lowLim, upLim))
    if (plotMedian)
      barPlots <- barPlots + geom_point(aes(x = as.factor(year), y = Fmedian), 
                                        position = position_dodge(.9))
    if (facetPlot){
      browser() # THIS BROWSER IS HERE AS SAVING IS NOT WORKING PROPERLY
      barPlots <- barPlots + facet_grid(ind ~ ., scales = "free_y")
    }
    png(filename = file.path(folder, paste0("comparison", ifelse(facetPlot, grph, "bothRas"), ".png")), 
        height = 500, width = 700)
    browser() # IT DOESNT WORK PROPERLY HERE FOR SOME REASON...
    print(barPlots)
    dev.off()
  })
  return(list(plot = barPlots, plotDT = plotDT))
}