createCaribouGIFFromList <- function(pathData, uploadFiles){
  
  reproducible::Require("animation")
  reproducible::Require("raster")
  reproducible::Require("ggplot2")
  reproducible::Require("magrittr")
  
  selectionRasList <- list.files(path = pathData, pattern = "relativeSelection")
  uncertainRasList <- list.files(path = pathData, pattern = "Uncertain")
  selectionRasList <- setdiff(selectionRasList, uncertainRasList)
  
  ysrName <- seq(0, 100, by = 10)
  selecRas <- lapply(X = ysrName, FUN = function(yr){
      r <- raster::raster(x = file.path(pathData,
                                        paste0("relativeSelectionTaigaPlains_Year", yr, ".tif")))
      return(r)
    })
  names(selecRas) <- paste0("Year", ysrName)
  
  uncertRas <- lapply(X = ysrName, FUN = function(yr){
    r <- raster::raster(x = file.path(pathData,
                                      paste0("relativeSelectionUncertainTaigaPlains_Year", yr, ".tif")))
    return(r)
  })
  names(uncertRas) <- paste0("Year", ysrName)
  rasList <- list(RSF = selecRas, RSFuncertainty = uncertRas)
  lapply(X = names(rasList), FUN = function(rasterType){
    rasterType <- names(rasList)[2]
    out <- raster::stack(rasList[[rasterType]])
    gifName <- file.path(pathData, paste0(rasterType, "_", toupper(format(Sys.time(), "%d%b%y")),".gif"))
    ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
    mxVal <- ceiling_dec(max(raster::maxValue(out)), level = 2)
    breaks <- quantile(x = out, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) %>%
      apply(MARGIN = 2, FUN = max)
    cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "Spectral")
    titleType <- if (rasterType == "RSF") "caribou selectivity" else "caribou selectivity uncertainty" 
    fixedStack <- lapply(X = 1:quickPlot::numLayers(out), FUN = function(ras){
      dt <- raster::as.data.frame(out[[ras]],
                                  xy = TRUE, na.rm = FALSE, 
                                  long = FALSE)
      names(dt) <- c("x", "y", "value")
      dtable <- data.table::data.table(dt) 
      dtable[, group := cut(value, breaks)]
      dt  <- as.data.frame(dtable)
      names(cols) <- unique(sort(dt$group))
      cols[is.na(cols)] <- "grey93"
      library(ggplot2)
      spPlot <- ggplot2::ggplot(data = dt, aes(x = x, y = y)) +
        geom_tile(aes(fill = group)) +
        scale_fill_manual(
          values = cols) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_rect(colour = "grey93"),
              axis.title = element_blank(),
              legend.title = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
        guides(fill = guide_legend(title.hjust = 0.5, reverse = TRUE)) +
        ggtitle(label = paste0("Predicted ", titleType, 
                               " for year ", strsplit(x = names(out[[ras]]), 
                                                      split = "Year")[[1]][2]))
      pngPlotName <- file.path(pathData, paste0("Predicted", rasterType, "Year", 
                                                          strsplit(x = names(out[[ras]]),
                                                                   split = "Year")[[1]][2],
                                                          "_", toupper(format(Sys.time(), "%d%b%y")),".png"))
      if (uploadFiles)
        googledrive::drive_upload(gifName, 
                                  path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))
      png(pngPlotName, width = 700, height = 480)
      print(spPlot)
      dev.off()
      return(spPlot)
    })
    
    animation::saveGIF(interval = 0.5, movie.name = gifName, expr = {
      for (i in seq(quickPlot::numLayers(out))) print(fixedStack[[i]])
    })
    if (uploadFiles)
      googledrive::drive_upload(gifName, 
                                path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))
})
}