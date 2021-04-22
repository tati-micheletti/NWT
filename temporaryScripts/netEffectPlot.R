# pal <- c("red3", "darkorange2", "gold2",
#          "steelblue1", "dodgerblue2", "blue3", "navy")

eachComparison <- "netEffect"

fl <- paste0("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/",
             "posthoc/vegetationResults/vegetationPlots/difference_",
             eachComparison,"_Leading.tif")

biomassDiffPlotPath <- paste0(tools::file_path_sans_ext(fl), "2.png")

typeName <- ifelse(eachComparison == "netEffect", 
                                       "net climate effect", 
                                       paste0("climate effect via ", eachComparison))

climateDiffAverage <- raster::raster(fl)
climateDiffAverage[is.na(flammableRTM)] <- NA

pal <- RColorBrewer::brewer.pal(11, "RdYlBu")
pal[6] <- "#f7f4f2"
checkColorPalette(pal)
  
png(filename = biomassDiffPlotPath,
      width = 21, height = 29,
      units = "cm", res = 300)
  
  maxV <- max(abs(round(minValue(climateDiffAverage), 0)),
              abs(round(maxValue(climateDiffAverage), 0)))
  
  AT <- seq(-maxV, maxV, length.out = 12)
  
  print(levelplot(climateDiffAverage,
                  sub = paste0("Difference in vegetation biomass due to ", 
                               typeName),
                  margin = FALSE,
                  maxpixels = 6e6,
                  at = AT,
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
                  col.regions = pal,
                  par.strip.text = list(cex = 0.8,
                                        lines = 1,
                                        col = "black")))
  dev.off()
  # Check the pallete
  
checkColorPalette <- function(pal){
  plot(NULL, xlim=c(0,length(pal)), ylim=c(0,1),
       xlab="", ylab="", xaxt="n", yaxt="n")
  rect(0:(length(pal)-1), 0, 1:length(pal), 1, col=pal) 
}
