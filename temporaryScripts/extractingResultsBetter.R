########## TO LOAD FILES ###############

birdsFireCaribouV1 <- readRDS(file = file.path(getwd(), "outputs/birdsFireCaribouV1_05MAR19.rds"))
birdsFireCaribouV2 <- readRDS(file = file.path(getwd(), "outputs/birdsFireCaribouV2_11MAR19.rds"))

#########################################

########## BIOMASS ######################

dt <- as.data.frame(birdsFireCaribouV1$summaryBySpecies)
library("ggplot2")
plt <- ggplot(dt, aes(x = year, y = BiomassBySpecies, group = speciesCode)) + 
  geom_line(size=1.2, aes(color = speciesCode))

#########################################
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

# GGplot for burnmap when only one year is used
data <- raster::as.data.frame(birdsFireCaribouV2$burnMap,
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
             
# Caribou graph (last one)
timeStr <- paste0("Year", 0:100)
caribouPopulation <- unlist(lapply(X = timeStr, FUN = function(year){
  birdsFireCaribou$predictedCaribou[[year]][["M3"]]$currentPopUpdated
})
)
caribouPopulationRec <- unlist(lapply(X = timeStr, FUN = function(year){
  birdsFireCaribou$predictedCaribou[[year]][["M3"]]$Rec
})
)

caribouLambda <- numeric(length(caribouPopulation)-1)
for (i in 1:(length(caribouPopulation)-1)){
  caribouLambda[i] <- caribouPopulation[i + 1]/caribouPopulation[i]
}

# RECRUITMENT
Time <- 0:100
dt <- data.frame(Recruitment = caribouPopulationRec, Time = Time[2:101])
library("ggplot2")
plt <- ggplot2::ggplot(data = dt, aes(x = Time, y = Recruitment)) + geom_point()

png(file.path(getwd(), "outputs", paste0("caribouLambdaNWT_", toupper(format(Sys.time(), "%d%b%y")),".png")), 
    width = 700, height = 480)
plot(x = Time, y = caribouLambda, title = "Caribou lambda for BCR6 within NWT", new = TRUE)
dev.off()
reproducible::Require(googledrive)
googledrive::drive_upload(file.path(getwd(), "outputs", paste0("caribouPop", toupper(format(Sys.time(), "%d%b%y")),".png")), 
                          path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))

# LAMBDA
Time <- 0:100
dt <- data.frame(caribouLambda = caribouLambda, Time = Time[2:100])
library("ggplot2")
plt <- ggplot2::ggplot(data = dt, aes(x = Time, y = caribouLambda)) + geom_point()

png(file.path(getwd(), "outputs", paste0("caribouLambdaNWT_", toupper(format(Sys.time(), "%d%b%y")),".png")), 
    width = 700, height = 480)
plot(x = Time, y = caribouLambda, title = "Caribou lambda for BCR6 within NWT", new = TRUE)
dev.off()
reproducible::Require(googledrive)
googledrive::drive_upload(file.path(getwd(), "outputs", paste0("caribouPop", toupper(format(Sys.time(), "%d%b%y")),".png")), 
                          path = as_id("1ZqPVs33HxnnmjLUW94i7AuwAS-nloPGH"))

