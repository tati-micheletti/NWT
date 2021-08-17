
# For biomass change in function of climate only
# totBiom_LandR_fS <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR_fS_2100.tif")
# totBiom_LandR.CS_fS <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR.CS_fS_2100.tif")
# totBiom_LandR_SCFM <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR_SCFM_2100.tif")
# totBiom_LandR.CS_SCFM <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR.CS_SCFM_2100.tif")

totBiom_LandR_fS <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_fS_2100.tif")
totBiom_LandR_fS_2011 <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_fS_2011.tif")

totBiom_LandR.CS_fS <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_fS_2100.tif")
totBiom_LandR.CS_fS_2011 <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_fS_2011.tif")

totBiom_LandR_SCFM <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_SCFM_2100.tif")
totBiom_LandR_SCFM_2011 <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_SCFM_2011.tif")

totBiom_LandR.CS_SCFM <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_SCFM_2100.tif")
totBiom_LandR.CS_SCFM_2011 <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_SCFM_2011.tif")

LandR.CS_fS <- totBiom_LandR.CS_fS-totBiom_LandR.CS_fS_2011
LandR_fS <- totBiom_LandR_fS-totBiom_LandR_fS_2011
LandR.CS_SCFM <- totBiom_LandR.CS_SCFM-totBiom_LandR.CS_SCFM_2011
LandR_SCFM <- totBiom_LandR_SCFM-totBiom_LandR_SCFM_2011

directEffect <- LandR.CS_fS-LandR_fS # Vegetation
fullEffect <- LandR.CS_fS-LandR_SCFM # Fire and Vegetation
indirectEffect <- LandR.CS_fS-LandR.CS_SCFM # Fire

# Make a plot of the number of pixel by the values in change of total biomass

Require("plyr")

# Direct effect
directEffectTB <- table(round_any(directEffect[], 100))
directEffectDT <- na.omit(data.table(val = round_any(getValues(directEffect), 100),
                                     effectType = "Direct Effect"))
# Indirect effect
indirectEffectTB <- table(round_any(indirectEffect[], 100))
indirectEffectDT <- na.omit(data.table(val = round_any(getValues(indirectEffect), 100),
                                       effectType = "Indirect Effect"))

# Full effect
fullEffectTB <- table(round_any(fullEffect[], 100))
fullEffectDT <- na.omit(data.table(val = round_any(getValues(fullEffect), 100),
                                   effectType = "Full Effect"))

plotDT <- rbindlist(list(directEffectDT, fullEffectDT, indirectEffectDT), use.names = TRUE)
plotDT[, average0 := round(mean(val), 0), by = "effectType"]
plotDT[val != 0, average := round(mean(val), 0), by = "effectType"]
plotDT[, Median0 := round(median(val), 0), by = "effectType"]
plotDT[val != 0, Median := round(median(val), 0), by = "effectType"]
plotDT[, Count := .N, by = c("effectType", "val")]
plotDT[, CountTot := .N, by = c("effectType")]
plotDT[val != 0, CountTot := sqrt(ifelse(effectType == "Direct Effect",
                                         222,293
)), by = "effectType"]
plotDT[val != 0, stdDev := round(sd(val), 0), by = "effectType"]
plotDT[val != 0, stdErr := stdDev/CountTot, by = "effectType"]

DTveg <- unique(plotDT[val != 0,])
DTvegSimple <- unique(DTveg[, c("effectType", "Median", "stdDev", "stdErr")])

Require("ggplot2")
pVeg <- ggplot(data = unique(plotDT[val != 0,]), aes(x = val, 
                                                     y = (Count*6.25)/(10^5),
                                                     group = effectType,
                                                     color = effectType,
                                                     fill = effectType))+
  # geom_area(alpha = 0.5, position = position_stack()) +
  geom_line() +
  scale_color_manual(breaks = c("Full Effect", "Direct Effect", "Indirect Effect"),
                     values = c("blue", "green", "red")) +
  geom_vline(xintercept = na.omit(unique(plotDT[effectType == "Full Effect", Median])), 
             colour = "blue", linetype = "dashed") + 
  geom_vline(xintercept = na.omit(unique(plotDT[effectType == "Direct Effect", Median])), 
             colour = "green", linetype = "dashed") +
  geom_vline(xintercept = na.omit(unique(plotDT[effectType == "Indirect Effect", Median])), 
             colour = "red", linetype = "dashed") +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16, 
                            family = "Arial"),
        panel.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"),
  ) +
  xlim(-5000, 5000) +
  xlab("Change in total biomass due to climate effects") +
  ylab("Area in ha (x 10\u2075)")
pVeg # <~~~~~~~~~~~~~~~~~~~~~~~~ HERE
ggsave(plot = pVeg, device = "png", filename = file.path("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc", "vegetationResults", #posthocFolder, "vegetationResults", 
                                                         "biomassAffectedByClimateChange.png"), 
       width = 8, height = 11)
