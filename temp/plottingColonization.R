library(raster)
library(ggplot2)
library(data.table)
library(rasterVis)
library(ggthemes)
library(grid)
library(tictoc)
folder <- "~/projects/NWT/outputs/posthoc/colonization"

sp <- usefulFuns::substrBoth(strng = list.files("~/projects/NWT/modules/birdsNWT/data/models", 
                                                pattern = "brt6a.R"), howManyCharacters = 4, fromEnd = FALSE)
effect <- c("climate", "vegetation", "fire", "netEffect")
realEffect <- c("directClimate", "vegetation", "fire", "netEffect")
typePlot <- c("colonization", "extirpation")
finalNames <- data.table(expand.grid(realEffect, typePlot))
finalNames[, nm := paste(Var2, Var1, sep = ".")]

allSpecies <- lapply(sp, function(species){
  allColExt <- lapply(typePlot, function(cORe){
    allEffects <- raster::stack(lapply(effect, function(eachEffect){
      r <- raster::raster(file.path(folder, paste0("difference_", eachEffect,"_", species, "_", cORe,".tif")))
      return(r)
    }))
    names(allEffects) <- realEffect
    return(allEffects)
  })
  fileNamePath <- file.path("~/projects/NWT/outputs/posthoc/colonization", 
                            paste0(realEffect, "_netColonization_", species))
  if (any(!file.exists(paste0(fileNamePath, ".tif")))){
    netColonization <- (allColExt[[1]] - allColExt[[2]])/2
    names(netColonization) <- paste0(realEffect, "_netColonization_", species)
    lapply(1:nlayers(netColonization), function(index){
      writeRaster(netColonization[[index]], 
                  filename = fileNamePath[index], 
                  format = "GTiff")
    })
  } else {
    netColonization <- raster::stack(paste0(fileNamePath, ".tif"))
  }
    names(netColonization) <- realEffect
    
    ### APPENDIX PLOT ###
    spFilePath <- file.path(folder, paste0("APPENDIX_plot_colonization_", species, ".png"))
    if (!file.exists(spFilePath)){
      pal <- RColorBrewer::brewer.pal(9, name = "RdYlGn")
      library(gridExtra)
      library(rasterVis)
      png(filename = spFilePath,
          width = 21, height = 29,
          units = "cm", res = 120)
      print(levelplot(netColonization,
                      main = species,
                      sub = "Probability of extirpation or colonization due to climate effects",
                      margin = FALSE,
                      colorkey = list(
                        space = 'bottom',
                        labels = list(at = -1:1, font = 4),
                        axis.line = list(col = 'black'),
                        width = 0.75
                      ),
                      par.settings = list(
                        strip.border = list(col = 'transparent'),
                        strip.background = list(col = 'transparent'),
                        axis.line = list(col = 'transparent')),
                      scales = list(draw = FALSE),
                      col.regions = pal,
                      at = seq(-1, 1, len = 10),
                      par.strip.text = list(cex = 0.8,
                                            lines = 1,
                                            col = "black")))
      dev.off()
    }
    
    

  return(netColonization)
})
names(allSpecies) <- sp

# NOW MAKE THE:
# 0. Stack netEffect rasters, and individually each one of the effects.
# MAP 1. Make a map of how many pixels (calc) are != 0 (4 maps)
# MAP 2. Sum all probabilities across species: lighter color x brighter color, -1:1 (4 maps)
# Interpretation on MAP 2: Each pixel is expected to lose or win this number of species (-64 to 64)


