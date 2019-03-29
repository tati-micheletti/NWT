createBirdsGIFFromList <- function(species, pathData, version, uploadFiles){
  
  reproducible::Require("animation")
  reproducible::Require("raster")
  reproducible::Require("ggplot2")
  reproducible::Require("magrittr")

  ysrName <- seq(0, 100, by = 10)
  spNames <- species
  predictedRas <- lapply(X = spNames, FUN = function(sp){
    ras <- lapply(X = ysrName, FUN = function(yr){
      r <- raster::raster(x = file.path(pathData,
                                        paste0("predicted", sp, "Year", yr, ".tif")))
      return(r)
    })
    names(ras) <- paste("Year", ysrName)
    return(ras)
  })
  names(predictedRas) <- spNames
  
  # List format should be sp -- year
  lapply(X = 1:length(predictedRas), FUN = function(sp){
    out <- raster::stack(predictedRas[[sp]])

    # FOR VERSION 1, WE NEED TO MASK ALL THE WATER + WATERLAND BEFORE MAKING THE PLOT
    # FOR VERSION 2, WE NEED TO MASK ONLY YEAR0 to WATER + WATERLAND BEFORE MAKING THE PLOT
    # FOR VERSION 3, WE NEED TO MASK for forest pixels only
    
    studyArea <- Cache(prepInputs,
                            url = "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU",
                            destinationPath = tempdir(),
                       omitArgs = "destinationPath", filename2 = NULL)
    
    rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df", 
                                studyArea = studyArea,
                                targetFile = "RTM.tif", destinationPath = tempdir(), 
                           filename2 = NULL,
                                omitArgs = "destinationPath")
    
    LCC05 <- LandR::prepInputsLCC(destinationPath = tempdir(),
                                          studyArea = studyAreaNWT,
                                          rasterToMatch = rasterToMatch)
    forestClasses <- c(1:15, 34:35)
    rasterToMatch[!LCC05 %in% forestClasses] <- NA
    
    if (version == "V2"){
      out[[1]] <- reproducible::postProcess(x = out[[1]], rasterToMatch = out[[2]],
                                            maskWithRTM = TRUE, filename2 = NULL)
      out <- reproducible::postProcess(x = out, rasterToMatch = rasterToMatch,
                                       maskWithRTM = TRUE, filename2 = NULL)
    } else {
      if (version == "V1"){
        out2 <- reproducible::postProcess(x = out, rasterToMatch = simList$uplands,
                                          maskWithRTM = TRUE, filename2 = NULL)
        out <- reproducible::postProcess(x = out, rasterToMatch = rasterToMatch,
                                         maskWithRTM = TRUE, filename2 = NULL)
        names(out2) <- names(out)
        out <- out2
      } else {
          out <- reproducible::postProcess(x = out, rasterToMatch = rasterToMatch,
                                                maskWithRTM = TRUE, filename2 = NULL)
      }
    }
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    gifName <- file.path(getwd(), paste0("outputs/birdPreds/", species[sp], "predNWT_", toupper(format(Sys.time(), "%d%b%y")),".gif"))
    ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
    mxVal <- ceiling_dec(max(raster::maxValue(out)), level = 2)
    breaks <- quantile(x = out, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) %>%
      apply(MARGIN = 2, FUN = max)
    cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "Spectral")
    
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
        ggtitle(label = paste0("Predicted ", species[sp], 
                               " for year ", strsplit(x = names(out[[ras]]), 
                                                      split = "Year")[[1]][2]))
pngPlotName <- file.path(getwd(), "outputs", paste0("Predicted", species[sp], "Year", 
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
