speciesChangesPath <- file.path("~/projects/NWT/outputs/05DEC20/", 
                                                            "BurnMap_2100.png")
# ras <- raster::raster("~/projects/NWT/outputs/05DEC20/LandR.CS_fS/LandR.CS_fS_RAS_biomassYear2100.tif")
ras <- readRDS("~/projects/NWT/outputs/05DEC20/LandR.CS_fS/run1/burnMap_year2100.rds")

library("lattice")
library("rasterVis")
library("viridis")
# palInf <- RColorBrewer::brewer.pal(22, "YlOrRd")
palInf <- heat.colors(22, rev = TRUE)
png(filename = speciesChangesPath,
    width = 21, height = 29,
    units = "cm", res = 300)
print(levelplot(ras,
                sub = paste0("Burn Map in 2100"),
                margin = FALSE,
                maxpixels = 6e6,
                colorkey = list(
                  space = 'bottom',
                  axis.line = list(col = 'black'),
                  width = 0.75
                ),
                par.settings = list(
                  strip.border = list(col = 'transparent'),
                  strip.background = list(col = 'transparent'),
                  axis.line = list(col = 'transparent')),
                scales = list(draw = FALSE),
                col.regions = palInf, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                par.strip.text = list(cex = 0.8,
                                      lines = 1,
                                      col = "black")))
dev.off()
