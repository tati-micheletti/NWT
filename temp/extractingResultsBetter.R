birdsFireCaribouList <- as(birdsFireCaribou, "simList_")
saveRDS(birdsFireCaribou, file.path(outputPath(birdsFireCaribou), 
                                    paste0("resultsNWTedehzhie_", 
                                           toupper(format(Sys.time(), "%d%b%y")),
                                           ".rds")))

birdsFireCaribou <- readRDS(file.path(getwd(), "outputs",
                                      paste0("resultsNWTedehzhie_",
                                             toupper(format(Sys.time(), "%d%b%y")),
                                             ".rds")))

# Caribou graph (last one)
timeStr <- paste0("Year", 0:100)
caribouPopulation <- unlist(lapply(X = timeStr, FUN = function(year){
  birdsFireCaribou$predictedCaribou[[year]][["M3"]]$currentPopUpdated
})
)
Time <- 0:100
png(file.path(getwd(), "outputs", paste0("caribouPop", toupper(format(Sys.time(), "%d%b%y")),".png")), 
    width = 700, height = 480)
quickPlot::Plot(x = Time, y = caribouPopulation, title = "Caribou population dynamics", new = TRUE)
dev.off()
reproducible::Require(googledrive)
googledrive::drive_upload(file.path(getwd(), "outputs", paste0("caribouPop", toupper(format(Sys.time(), "%d%b%y")),".png")), 
                          path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))

# Birds Gif
reproducible::Require(animation)
lapply(X = 1:length(birdsFireCaribou$birdSpecies), FUN = function(sp){ # NOT REALLY WORKING FOR THE SECOND SPECIES.. DEBUG WHEN HAVE TIME [ FIX ]
  tmpStack <- lapply(birdsFireCaribou$birdPrediction, `[[`, sp)
  out2 <- raster::stack(tmpStack) # See about title and all
  gifName <- file.path(getwd(), paste0("outputs/birdPreds/", sp, "pred.gif"))
  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  mxVal <- ceiling_dec(max(raster::maxValue(raster::stack(tmpStack))), level = 2)
  fixedStack <- lapply(X = 1:length(tmpStack), FUN = function(ras){
    data <- raster::as.data.frame(tmpStack[[ras]],
                                  xy = TRUE, na.rm = FALSE, 
                                  long = FALSE)
    names(data) <- c("x", "y", "value")
    spPlot <- ggplot2::ggplot() +
      geom_tile(data = data,
                aes(x = x, y = y, fill = value)) +
      scale_fill_gradientn(
        name = "value",
        colors = c("red", "orange", "yellow", "purple", "blue"),
        breaks = c(0, mxVal*0.25, mxVal*0.5, mxVal*0.75, mxVal),
        limits = c(0, mxVal),
        na.value = "grey93") + 
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(colour = "grey93"),
            axis.title = element_blank(),
            legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5)) +
      ggtitle(label = paste0("Predicted ", sp, " for year ", strsplit(x = names(tmpStack[[ras]]), 
                                                                      split = "Year")[[1]][2]))
    return(spPlot)
  })
  
  animation::saveGIF(interval = 0.1, movie.name = gifName, expr = {
    for (i in seq(quickPlot::numLayers(out2))) print(fixedStack)
  })
})

googledrive::drive_upload(file.path(getwd(), "outputs", "birdPreds/"), 
                          path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))

# # Fire burning
# out2 <- raster::stack(birdsFireCaribou$disturbanceMaps) # See about title and all
# gifName <- file.path(getwd(), paste0("outputs/fireSpread.gif"))
# animation::saveGIF(interval = 0.1, movie.name = gifName, expr = {
#   for (i in seq(quickPlot::numLayers(out2))) raster::plot(out2[[i]], xlab = names(out2[[i]]))
# })
# 
# drive_upload(file.path(getwd(), "outputs",
#                        paste0("resultsNWTedehzhie_",
#                               toupper(format(Sys.time(), "%d%b%y")),
#                               ".rds"), path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH")))
             