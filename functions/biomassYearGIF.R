biomassYearGIF <- function(dataPath){

    reproducible::Require("animation")
    reproducible::Require("raster")
    reproducible::Require("ggplot2")
    reproducible::Require("magrittr")
    
    ysrName <- paddedFloatToChar(seq(0,100, by = 10), padL = 3)
    predictedRas <- lapply(X = ysrName, FUN = function(yr){
      tryCatch({
        simBM <- readRDS(file.path(dataPath, paste0("simulatedBiomassMap_year", yr,".rds")))
        names(simBM) <- paste0("totalBiomassYear", yr)
        return(simBM)
      }, error = function(e){
        message("There is no data associated to year ", yr, ". Returning NULL")
        return(NULL)
      })
    })
    names(predictedRas) <- paste0("Year", ysrName)
    out  <- raster::stack(predictedRas)
      
      gifName <- file.path(dataPath, paste0("totalBiomass", toupper(format(Sys.time(), "%d%b%y")),".gif"))
      ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
      mxVal <- ceiling_dec(max(raster::maxValue(out)), level = 2)
      breaks <- quantile(x = out, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) %>%
        apply(MARGIN = 2, FUN = max)
      cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "BrBG")
      
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
        yrPlot <- ggplot2::ggplot(data = dt, aes(x = x, y = y)) +
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
          ggtitle(label = paste0("Predicted total biomass for year ", strsplit(x = names(out[[ras]]), 
                                                        split = "Year")[[1]][2]))
        
        png(file.path(dataPath, paste0("predictedBiomassYear", strsplit(x = names(out[[ras]]), 
                                                                                            split = "Year")[[1]][2],
                                                 "_", toupper(format(Sys.time(), "%d%b%y")),".png")), 
            width = 700, height = 480)
        print(yrPlot)
        dev.off()
        return(yrPlot)
      })
      
      animation::saveGIF(interval = 0.5, movie.name = gifName, expr = {
        for (i in seq(quickPlot::numLayers(out))) print(fixedStack[[i]])
      })
      
      googledrive::drive_upload(gifName, 
                                path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))
}