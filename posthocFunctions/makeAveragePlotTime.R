makeAveragePlotTime <- function(dataFolder, 
                                birds, 
                                scenarios,
                                shp = NULL){
  library("usefun")
  library("raster")
  library("quickPlot")
  library("raster")
  library("data.table")
  plotMaps <- rbindlist(lapply(scenarios, FUN = function(scen){
    plotMaps <- rbindlist(lapply(birds, FUN = function(sp){
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
      names(birdRasList) <- paste0(sp, c(2011, 2041, 2071, 2100))
      library(quickPlot)
      birdRasList <- as.list(birdRasList)
      names(birdRasList) <- lapply(birdRasList, names)
      # Plot(birdRasList)
      dt <- rbindlist(lapply(birdRasList, function(ras){
        if (!is.null(shp)){
          browser() # Need to implement the shapefile to extract the averages and sd of specific areas here...
          # Something like: extract(r.stack, poly, fun=mean, df=TRUE) https://gis.stackexchange.com/questions/237133/function-sample-code-to-extract-raster-value-per-polygon-in-r
        }
        dt <- data.table(species = usefun::substrBoth(strng = names(ras), 
                                                      howManyCharacters = 4, fromEnd = FALSE), 
                         scenarios = scen,
                         average = mean(ras[], na.rm = TRUE), 
                         std = sd(ras[], na.rm = TRUE), 
                         year = usefun::substrBoth(strng = names(ras), 
                                                   howManyCharacters = 4, fromEnd = TRUE))
        return(dt)
      }))
      return(dt)
    }))
    return(plotMaps)
  }))
  saveRDS(plotMaps, file = file.path(dataFolder, "averageTimePlotTable.rds"))
  library(ggplot2)
  
  # ~~~~~~~~~~~~~~~~~~~ INDIVIDUAL EFFECTS
  
  cols <- c("steelblue3", "red4", "forestgreen")
  p <-  ggplot(data = plotMaps, aes(x = as.numeric(year), y = average,
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
    facet_grid(species ~ scenarios) +
    theme(legend.position = "none")
  
  ggsave(filename = file.path(dataFolder, "averageTimePlot.png"), 
         plot = p, device = "png")
  
  # ~~~~~~~~~~~~~~~~~~~ CUMULATIVE EFFECT
  
  plotMaps[, cumulativeEffect := sum(average), by = c("species", "year")]
  plotMaps[, cumulativeEffectSD := max(std), by = c("species", "year")]
  cols <- c("goldenrod2", "grey40", "darkorchid2")
  dt <- plotMaps[, .(species, year, cumulativeEffect, cumulativeEffectSD)]
  
  library(ggplot2)
  p2 <- ggplot(data = dt, aes(x = as.numeric(year), y = cumulativeEffect,
                             group = species)) +  
    geom_ribbon(aes(fill = species, ymin = (cumulativeEffect - cumulativeEffectSD),
                    ymax = (cumulativeEffect + cumulativeEffectSD)), alpha = 0.5) +
    scale_fill_manual(values = cols) +
    geom_line(aes(color = species)) +
    scale_color_manual(values = cols) +
    scale_x_continuous(limits = c(2011, 2100)) +
    xlab(label = "years") +
    ylab(label = "Averaged cumulative effect of climate on bird density") +
    theme_bw() +
    facet_grid(species ~ .) +
    theme(legend.position = "none")
  ggsave(filename = file.path(rasFolder, "averageTimePlotCumulative.png"), 
         plot = p, device = "png")
  
  return(list(individualEffect = p, cumulativeEffect = p2))
}