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

# When it crashes... Partial results

ysrName <- c(0, 10, 20, 30, 40, 50)
spNames <- c("BBWA", "BOCH")
predictedRas <- lapply(X = spNames, FUN = function(sp){
  ras <- lapply(X = ysrName, FUN = function(yr){
    r <- raster::raster(x = file.path("/home/tmichele/Documents/GitHub/NWT", 
                                      "modules/birdsNWT/data/NWT", 
                                      paste0("predicted", sp, "Year", yr, ".tif")))
    return(r)
  })
  names(ras) <- paste("Year", ysrName)
  return(ras)
})
names(predictedRas) <- spNames
reproducible::Require("animation")
reproducible::Require("raster")
reproducible::Require("ggplot2")
reproducible::Require("magrittr")

lapply(X = 1:length(predictedRas), FUN = function(sp){ # NOT REALLY WORKING FOR THE SECOND SPECIES.. DEBUG WHEN HAVE TIME [ FIX ]
  out2 <- raster::stack(predictedRas[[sp]])
  gifName <- file.path(getwd(), paste0("outputs/birdPreds/", sp, "predNWT.gif"))
  ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
  mxVal <- ceiling_dec(max(raster::maxValue(out2)), level = 2)
  breaks <- quantile(x = out2, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) %>%
    apply(MARGIN = 2, FUN = max)
  cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "Spectral")
  
  fixedStack <- lapply(X = 1:quickPlot::numLayers(predictedRas[[sp]]), FUN = function(ras){
    dt <- raster::as.data.frame(predictedRas[[sp]][[ras]],
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
      ggtitle(label = paste0("Predicted ", sp, " for year ", ras*10))
    browser()
    return(spPlot)
  })
  
  png(file.path("/tmp/RtmpBNTM1M/rasters/predictedBBWA_TEST.tif"), 
      width = 700, height = 480)
  print(fixedStack[[1]])
  dev.off()
  
  animation::saveGIF(interval = 0.5, movie.name = gifName, expr = {
    for (i in seq(quickPlot::numLayers(out2))) print(fixedStack)
  })
})
  
googledrive::drive_upload(file.path(getwd(), "outputs", "birdPreds/"), 
                          path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))


# When simList is fine
reproducible::Require(animation)
lapply(X = 1:length(birdsFireCaribou$birdSpecies), FUN = function(sp){ # NOT REALLY WORKING FOR THE SECOND SPECIES.. DEBUG WHEN HAVE TIME [ FIX ]
  browser()
  tmpStack <- lapply(birdsFireCaribou$birdPrediction, `[[`, sp)
  out2 <- raster::stack(tmpStack) # See about title and all
  gifName <- file.path(getwd(), paste0("outputs/birdPreds/", sp, "predNWT.gif"))
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

# ~~~~~~~~~~~~~~~~~ USE THIS ONE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~`
# When simList is fine: PLOTS FIXED FOR GROUPING VALUES
# NOT REALLY WORKING FOR THE SECOND SPECIES.. DEBUG WHEN HAVE TIME [ FIX ]
reproducible::Require("animation")
reproducible::Require("raster")
reproducible::Require("ggplot2")
reproducible::Require("magrittr")
lapply(X = 1:length(birdsFireCaribou$birdSpecies), FUN = function(sp){ 
  browser()
  tmpStack <- lapply(birdsFireCaribou$birdPrediction, `[[`, sp)
  out2 <- raster::stack(tmpStack) # See about title and all
  gifName <- file.path(getwd(), paste0("outputs/birdPreds/", sp, "predNWT.gif"))
  ceiling_dec <- function(x, level = 1) round(x + 5*10^(-level-1), level)
  mxVal <- ceiling_dec(max(raster::maxValue(out2)), level = 2)
  breaks <- quantile(x = out2, probs = seq(from = 0, to = 1, by = 0.1), na.rm = TRUE) %>%
    apply(MARGIN = 2, FUN = max)
  cols <- RColorBrewer::brewer.pal(n = length(breaks), name = "Spectral")
  
  fixedStack <- lapply(X = 1:quickPlot::numLayers(predictedRas[[sp]]), FUN = function(ras){
    dt <- raster::as.data.frame(predictedRas[[sp]][[ras]],
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


# GGplot for biomass when only one year is used
data <- raster::as.data.frame(birdsFireCaribouV1$biomassMap,
                              xy = TRUE, na.rm = FALSE, 
                              long = FALSE)
greens <- RColorBrewer::brewer.pal(n = 5, name = "Greens")
breaks <- quantile(x = data$value, probs = seq(from = 0, to = 1, by = 0.2), na.rm = TRUE)

names(data) <- c("x", "y", "value")
spPlot <- ggplot2::ggplot() +
  geom_tile(data = data,
            aes(x = x, y = y, fill = value)) +
  scale_fill_gradientn(
    name = "value",
    colors = c("white", greens),
    breaks = c(0, breaks),
    limits = c(0, max(data$value)),
    na.value = "grey93") + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "grey93"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = paste0("Predicted biomass for year 12"))

png(file.path(getwd(), "outputs", paste0("biomassYear12_", toupper(format(Sys.time(), "%d%b%y")),".png")), 
    width = 700, height = 480)
print(spPlot)
dev.off()

# GGplot for burnmap when only one year is used
data <- raster::as.data.frame(birdsFireCaribouV1$burnMap,
                              xy = TRUE, na.rm = FALSE, 
                              long = FALSE)
names(data) <- c("x", "y", "value")
firePlot <- ggplot2::ggplot() +
  geom_tile(data = data,
            aes(x = x, y = y, fill = value)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "grey93"),
        axis.title = element_blank(),
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  ggtitle(label = paste0("Predicted cummulative fire for year 12"))

png(file.path(getwd(), "outputs", paste0("cummulativeFireYear12_", toupper(format(Sys.time(), "%d%b%y")),".png")), 
    width = 700, height = 480)
print(firePlot)
dev.off()

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
             