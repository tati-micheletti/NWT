
caribouLCC[caribouLCC < 200] <- 0
caribouLCC[caribouLCC > 200 & caribouLCC < 220] <- 212
caribouLCC[caribouLCC > 220 & caribouLCC < 223] <- 222
caribouLCC[caribouLCC == 223] <- 223
caribouLCC[caribouLCC > 230 & caribouLCC < 233] <- 232
caribouLCC[caribouLCC == 233] <- 233

r <- raster::ratify(caribouLCC)
pngPath <- file.path("/home/tmichele/projects/NWT/outputs/climateScenarios/LandR.CS_fS/runACCESS1-0/caribouPredictions", "EOSDnoReclass.png")
png(filename = pngPath,
    width = 21, height = 29,
    units = "cm", res = 300)
print(rasterVis::levelplot(r,
                           sub = paste0("EOSD resampled 250m -- no reclassification of 233 and 223"),
                           att = "ID",
                           margin = FALSE,
                           maxpixels = 5e6,
                           colorkey = list(
                             space = 'bottom',
                             # at = 1:10,
                             axis.line = list(col = 'black'),
                             width = 0.75
                           ),
                           par.settings = list(
                             strip.border = list(col = 'transparent'),
                             strip.background = list(col = 'transparent'),
                             axis.line = list(col = 'transparent')),
                           scales = list(draw = FALSE),
                           col.regions = c("white", "red", "yellow", 
                                           "darkgreen", "blue", "purple"),#pals::kovesi.rainbow(length(unique(r))), #viridis_pal(option = "D")(nlev),
                           par.strip.text = list(cex = 0.8,
                                                 lines = 1,
                                                 col = "black")
                           ))
dev.off()
