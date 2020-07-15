umbrellaTradeoffMapping <- function(hotspotIndex, ras){
  ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
  mxVal <- ceiling_dec(max(hotspotIndex$hotspotsIndex), level = 2)
  breaks <- quantile(x = hotspotIndex$hotspotsIndex, 
                     probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) 
  # Define colors using the function also used by "scale_fill_gradient2"
  discr_colors_fct <- 
    scales::div_gradient_pal(low = "darkred",
                             mid = "yellow", 
                             high = "darkgreen")
  discr_colors <- discr_colors_fct(seq(0, 1, length.out = length(breaks)))
  
  listOfMaps <- lapply(unique(hotspotIndex$year), function(y){
    eachYear <- hotspotIndex[year == y]
    rasNA <- raster(ras)
    rasHot <- raster::setValues(x = rasNA, values = NA)
    rasHotVals <- data.table::data.table(pixelID = 1:raster::ncell(rasHot), vals = getValues(x = rasHot))
    rasHotVals <- merge(rasHotVals, eachYear, all = TRUE)
    rasHot <- setValues(x = rasHot, values = as.numeric(rasHotVals$hotspotsIndex))
    library("ggplot2")
    data <- raster::as.data.frame(rasHot,
                                  xy = TRUE, na.rm = FALSE, 
                                  long = FALSE)
    names(data) <- c("x", "y", "value")
    data <- data.table::data.table(data) 
    # Define category breaks
    data$valueDiscr <- cut(data$value,
                           breaks = breaks,
                           right = FALSE)
    p <-  ggplot(data = data, aes(x = x, y = y)) +
      geom_tile(aes(fill = valueDiscr)) +
      coord_equal() +
      scale_fill_manual(values = discr_colors, labels = c(1:10, NA)) +
      guides(fill = guide_legend(reverse = TRUE)) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            plot.background = element_blank(),
            panel.border = element_blank(),
            axis.title = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank()) +
      ggtitle(paste0("Hotspot index for multispecies conservation: ",
                     usefulFuns::substrBoth(y, 4, TRUE)))
    quickPlot::clearPlot()
    p
    ggsave(file.path(getwd, "outputs/30JUL19/", paste0("hotspots", y,".png")), device = "png", width = 20,
           height = 20, units = "cm")
    return(list(plotHotspot = p, dtHotspot = data))
  })
  names(listOfMaps) <- unique(hotspotIndex$year)
  return(listOfMaps)
}