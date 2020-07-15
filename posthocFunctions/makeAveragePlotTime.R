makeAveragePlotTime <- function(dataFolder, 
                                Species, 
                                scenarios,
                                years,
                                shp = NULL, 
                                overwrite = FALSE){
  library("usefulFuns")
  library("raster")
  library("quickPlot")
  library("raster")
  library("data.table")
  sp <- if(Species == "caribou") "caribou" else "birds"
  averageTimePlotTablePath <- file.path(dataFolder, paste0(sp, "AverageTimePlotTable.rds"))
  if (all(file.exists(averageTimePlotTablePath), !isTRUE(overwrite))){
    plotMaps <- readRDS(averageTimePlotTablePath)
  } else {
    plotMaps <- rbindlist(lapply(scenarios, FUN = function(scen){
      plotMaps <- rbindlist(lapply(Species, FUN = function(sp){
        birdFiles <- grepMulti(x = list.files(dataFolder, full.names = T), 
                               patterns = c(sp, scen, "_mean.tif"))
        if (length(birdFiles) == 0) return(NULL)
        birdRasList <- stack(lapply(birdFiles, raster))
        # rescale <- function(x, new.min = -1, new.max = 1){
        #   vals <- data.table(getValues(x))
        #     x.min <- min(vals, na.rm = TRUE)
        #     x.max <- max(vals, na.rm = TRUE)
        #   newVal <- new.min + (vals - x.min) * ((new.max - new.min) / (x.max - x.min))
        #   rasStk <- setValues(x, as.matrix(newVal))
        # }
        # birdRasList <- rescale(birdRasList)
        # birdRasList <- normImage(birdRasList, norm = TRUE)
        names(birdRasList) <- paste0(sp, years)
        library(quickPlot)
        birdRasList <- as.list(birdRasList)
        names(birdRasList) <- lapply(birdRasList, names)
        dt <- rbindlist(lapply(birdRasList, function(ras){
          if (!is.null(shp)){
            shp2 <- projectInputs(x = shp, targetCRS = crs(ras))
            library(sf)
            shpSf <- st_as_sf(shp2)
            shpSf$poly <- as.numeric(shpSf$OBJECTID)
            rasPoly <- fasterize::fasterize(shpSf, raster = ras, field = "poly")
            tb1 <- data.table(pixelID = 1:ncell(ras), val = getValues(ras), polyID = getValues(rasPoly))
            dt <- data.table(species = usefulFuns::substrBoth(strng = names(ras), 
                                                          howManyCharacters = 4, fromEnd = FALSE), 
                             scenarios = scen,
                             polyID = setkey(na.omit(tb1[, mean(val, na.rm = TRUE), by = "polyID"]), "polyID")[["polyID"]],
                             average = setkey(na.omit(tb1[, mean(val, na.rm = TRUE), by = "polyID"]), "polyID")[["V1"]], 
                             std = setkey(na.omit(tb1[, sd(val, na.rm = TRUE), by = "polyID"]), "polyID")[["V1"]], 
                             year = usefulFuns::substrBoth(strng = names(ras), 
                                                       howManyCharacters = 4, fromEnd = TRUE))
          } else {
            dt <- data.table(species = usefulFuns::substrBoth(strng = names(ras), 
                                                          howManyCharacters = 4, fromEnd = FALSE), 
                             scenarios = scen,
                             average = mean(ras[], na.rm = TRUE), 
                             std = sd(ras[], na.rm = TRUE), 
                             year = usefulFuns::substrBoth(strng = names(ras), 
                                                       howManyCharacters = 4, fromEnd = TRUE))
            
          }
          return(dt)
        }))
        return(dt)
      }))
      return(plotMaps)
    }))
    if (unique(plotMaps$species) == "cari")
      plotMaps$species <- "caribou"
    saveRDS(plotMaps, file = averageTimePlotTablePath)
  }
  library(ggplot2)
  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS BY POLYGON
  if (length(unique(plotMaps$scenarios)) == 2)
    cols <- c("red4", "forestgreen") else 
      cols <- c("steelblue3", "red4", "forestgreen")
  allBirdsPlots <- lapply(Species, function(BIRD){
    birdTable <- plotMaps[species == BIRD,]
    p <-  ggplot(data = birdTable, aes(x = as.numeric(year), y = average,
                                      group = scenarios)) +  
      geom_ribbon(aes(fill = scenarios, ymin = (average - std),
                      ymax = (average + std)), alpha = 0.5) +
      scale_fill_manual(values = cols) +
      geom_line(aes(color = scenarios)) +
      scale_color_manual(values = cols) +
      scale_x_continuous(limits = c(2011, 2100)) +
      xlab(label = "years") +
      ylab(label = "Averaged direct and indirect effects of climate on bird density") +
      theme_bw() +
      facet_grid(polyID ~ scenarios) + 
      theme(legend.position = "none")
    ggsave(filename = file.path(dataFolder, paste0("averageTimePlot_", BIRD, "PerPolygon.png")), 
           plot = p, device = "png")
    
    return(p)
  })
  names(allBirdsPlots) <- Species
  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS

  plotMaps[, c("minVal", "maxVal") := list(average-std, average+std)]
  plotMaps <- melt(data = plotMaps, id.vars = c("species", "scenarios", "polyID", "average", "std", "year"), measure.vars = c("minVal", "maxVal"))
  plotMaps[ , c("averagePols", "sdPols") := list(mean(average), sd(value)), by = c("species", "scenarios", "year")]
  plotMaps <- unique(plotMaps, by = c("species", "scenarios", "year")) # Cleanup
  plotMaps <- plotMaps[, c("polyID","average", "std", "variable", "value") := NULL] # Cleanup
  p <-  ggplot(data = plotMaps, aes(x = as.numeric(year), y = averagePols,
                                    group = scenarios)) +  
    geom_ribbon(aes(fill = scenarios, ymin = (averagePols - sdPols),
                    ymax = (averagePols + sdPols)), alpha = 0.5) +
    scale_fill_manual(values = cols) +
    geom_line(aes(color = scenarios)) +
    scale_color_manual(values = cols) +
    scale_x_continuous(limits = c(2011, 2100)) +
    xlab(label = "years") +
    ylab(label = "Averaged direct and indirect effects of climate on bird density") +
    theme_bw() +
    facet_grid(species ~ scenarios, scales = "free") +
    theme(legend.position = "none")
  
  ggsave(filename = file.path(dataFolder, paste0(sp, "AverageTimePlot.png")), 
         plot = p, device = "png")
  
  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECT
  
  plotMaps[, c("minValCumm", "maxValCumm") := list(averagePols-sdPols, averagePols+sdPols)]
  plotMaps <- melt(data = plotMaps, id.vars = c("species", "scenarios", "year", "averagePols"), measure.vars = c("minValCumm", "maxValCumm"))
  plotMaps[ , c("averagePolsCumm", "sdPolsCumm") := list(mean(averagePols), sd(value)), by = c("species", "year")]
  plotMaps <- unique(plotMaps, by = c("species", "year")) # Cleanup
  plotMaps <- plotMaps[, c("averagePols", "variable", "value") := NULL] # Cleanup
  cols <- c("goldenrod2", "grey40", "darkorchid2")
  
  library(ggplot2)
  p2 <- ggplot(data = plotMaps, aes(x = as.numeric(year), y = averagePolsCumm,
                             group = species)) +  
    geom_ribbon(aes(fill = species, ymin = (averagePolsCumm - sdPolsCumm),
                    ymax = (averagePolsCumm + sdPolsCumm)), alpha = 0.5) +
    scale_fill_manual(values = cols) +
    geom_line(aes(color = species)) +
    scale_color_manual(values = cols) +
    scale_x_continuous(limits = c(2011, 2100)) +
    xlab(label = "years") +
    ylab(label = "Averaged cumulative effect of climate on bird density") +
    theme_bw() +
    facet_grid(species ~ .) +
    theme(legend.position = "none")
  ggsave(filename = file.path(dataFolder, paste0(sp, "AverageTimePlotCumulative.png")), 
         plot = p2, device = "png")
  
  return(list(effectByPolygon = allBirdsPlots, individualEffect = p, cumulativeEffect = p2))
}
