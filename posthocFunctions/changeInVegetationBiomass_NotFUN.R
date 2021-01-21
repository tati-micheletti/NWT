# Change in vegetatio biomass
# 1. First load study area, rasterToMatch, and caribouArea2
source("1_generalSetup.R")
source("2_generatingInputs.R")

# After loading study area and caribouArea2, consolidate areas
# Make a map of the areas

caribouArea2$REGIONS <- as.numeric(c(1, 2, 2, 1, 3, 3, 4))
convTable <-  data.table::data.table(regionName = c("central", "southern", "western", "northeastern"),
                                     region = c(1, 2, 3, 4))
divNWT <- sf::st_as_sf(caribouArea2)
divRas <- fasterize::fasterize(divNWT, raster = rasterToMatch, field = "REGIONS")

# Check the average biomass per species
sp <- c("Pice_Mar", "Pice_Gla", "Popu_Tre", "Betu_Pap", "Lari_Lar")
mods <- c("LandR.CS_fS", "LandR_SCFM")
ys <- c(2011, 2100)

treeBiomassSummary <- lapply(sp, function(Species){
  allSc <- lapply(mods, function(Scenario){
    message(paste0("Calculating summary statistics for ", Species, " ", Scenario))
    # If needed, can adjust the ras to be for both 2011 and 2100 separately.
    # Just needs:
    # 1. a lapply around the species for year with function 'ys', for example;
    # 2. Remove the ras2011, ras2100 below; 
    # 3. Replace ras <- ras2100-ras2011 by
    #   ras <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
    #                             "vegetationResults/averageBiomass_",
    #                             Scenario,"_", ys,"_", Species,".tif"))
    ras2011 <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
                                     "vegetationResults/averageBiomass_",
                                     Scenario,"_2011_", Species,".tif"))
    ras2100 <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
                                     "vegetationResults/averageBiomass_",
                                     Scenario,"_2100_", Species,".tif"))
    ras <- ras2100-ras2011
    
    library("data.table")
    DT <- na.omit(data.table::data.table(pixelID = 1:ncell(ras),
                                         val = raster::getValues(ras),
                                         region = raster::getValues(x = divRas)))
    DT <- merge(DT, convTable)
    plotDT <- data.table(Reduce(merge, lapply(c("mean", "median", "min", "max", "se"), function(fun){
      se <- function(x, na.rm) sd(x, na.rm)/sqrt(sum(!is.na(x))) # ==> standard error that ignores NA values
      tb <- DT[, lapply(.SD, get(fun), na.rm = TRUE), by = regionName, .SDcols = "val"]
      tb[["operation"]] <- paste0("F", fun)
      library("reshape2")
      tb2 <- data.table(melt(data = tb, id = c("regionName", "operation")))
      tb2[, variable := NULL]
      tb2$species <- Species
      tb2$scenario <- Scenario
      tb3 <- dcast(tb2, formula = regionName + species + scenario ~ operation)
      return(tb3)
    })
    ))
    return(plotDT)
  })
})
treeBiomassSummarySD <- lapply(sp, function(Species){
    allSc <- lapply(mods, function(Scenario){
      message(paste0("Calculating summary statistics for ", Species, " ", Scenario))
      # If needed, can adjust the ras to be for both 2011 and 2100 separately.
      # Just needs:
      # 1. a lapply around the species for year with function 'ys', for example;
      # 2. Remove the ras2011, ras2100 below; 
      # 3. Replace ras <- ras2100-ras2011 by
      #   ras <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
      #                             "vegetationResults/averageBiomass_",
      #                             Scenario,"_", ys,"_", Species,".tif"))
      ras2011 <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
                                   "vegetationResults/averageBiomass_",
                                   Scenario,"_2011_", Species,".tif"))
      ras2100 <- raster::raster(paste0("~/projects/NWT/outputs/posthoc/",
                                       "vegetationResults/averageBiomass_",
                                       Scenario,"_2100_", Species,".tif"))
      ras <- ras2100-ras2011
      
      library("data.table")
      DT <- na.omit(data.table::data.table(pixelID = 1:ncell(ras),
                                   val = raster::getValues(ras),
                                   region = raster::getValues(x = divRas)))
      DT <- merge(DT, convTable)
      plotDT <- data.table(Reduce(merge, lapply(c("mean", "median", "min", "max", "sd"), function(fun){
        se <- function(x, na.rm) sd(x, na.rm)/sqrt(sum(!is.na(x))) # ==> standard error that ignores NA values
        tb <- DT[, lapply(.SD, get(fun), na.rm = TRUE), by = regionName, .SDcols = "val"]
        tb[["operation"]] <- paste0("F", fun)
        library("reshape2")
        tb2 <- data.table(melt(data = tb, id = c("regionName", "operation")))
        tb2[, variable := NULL]
        tb2$species <- Species
        tb2$scenario <- Scenario
        tb3 <- dcast(tb2, formula = regionName + species + scenario ~ operation)
        return(tb3)
      })
      ))
      return(plotDT)
    })
})
  
treeBiomassSummary2 <- unlist(treeBiomassSummary, recursive = FALSE)
treeBiomassSummary <- rbindlist(treeBiomassSummary2)

treeBiomassSummary2 <- unlist(treeBiomassSummarySD, recursive = FALSE)
treeBiomassSummarySD <- rbindlist(treeBiomassSummary2) # Obvisouly we have a huge SD!
names(treeBiomassSummarySD)[names(treeBiomassSummarySD) == "Fsd"] <- "Fse" 
library("ggplot2")

makeBiomassChangePlot <- function(treeBiomassSummary){
  lowLim <- round(min(treeBiomassSummary$Fmean) - 0.08*mean(treeBiomassSummary$Fmean), 1)
  upLim <- round(max(treeBiomassSummary$Fmean) + 0.05*mean(treeBiomassSummary$Fmean), 1)
  barPlots <- ggplot(data = treeBiomassSummary, 
                     aes(x = regionName, y = Fmean, 
                         ymax = Fmean + Fse,
                         ymin = Fmean - Fse, 
                         fill = regionName)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(width = 0.2, position = position_dodge(.9)) +
    scale_fill_manual(values = c("darkred", "darkgoldenrod", "darkblue","darkgreen")) +
    geom_text(aes(label = round(Fmean, 0)), vjust = -3, color = "black", size = 3) +
    labs(x = "Regions", y = paste0("Average value")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "right") +
    coord_cartesian(ylim = c(lowLim, upLim))
  # barPlots <- barPlots + geom_point(aes(x = as.factor(year), y = Fmedian), 
  #                                   position = position_dodge(.9))
  barPlots <- barPlots + facet_grid(species ~ scenario, scales = "free_y")
  barPlots
}

treeBiomassSummarySEplot <- makeBiomassChangePlot(treeBiomassSummary)
treeBiomassSummarySDplot <- makeBiomassChangePlot(treeBiomassSummarySD)

makeBiomassChangePlotSP <- function(treeBiomassSummary){
  lowLim <- round(min(treeBiomassSummary$Fmean) - 0.08*mean(treeBiomassSummary$Fmean), 1)
  upLim <- round(max(treeBiomassSummary$Fmean) + 0.05*mean(treeBiomassSummary$Fmean), 1)
  scenLabs <- c("Non-climate-sensitive", "Climate-sensitive") # The one I want
  names(scenLabs) <- c("LandR_SCFM", "LandR.CS_fS") # The one I have
  barPlots <- ggplot(data = treeBiomassSummary, 
                     aes(x = species, y = Fmean, 
                         ymax = Fmean + Fse,
                         ymin = Fmean - Fse, 
                         fill = species)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_errorbar(width = 0.2, position = position_dodge(.9)) +
    scale_fill_manual(values = c("purple", "darkred", "darkgoldenrod", 
                                 "darkblue","darkgreen", "yellow")) +
    geom_text(aes(label = round(Fmean, 0)), vjust = -3, color = "black", size = 3) +
    labs(x = "Species", y = paste0("Average value")) +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "right") +
    coord_cartesian(ylim = c(lowLim, upLim))
  # barPlots <- barPlots + geom_point(aes(x = as.factor(year), y = Fmedian), 
  #                                   position = position_dodge(.9))
  barPlots <- barPlots + facet_grid(regionName ~ scenario, scales = "free_y",
                                    labeller = labeller(scenario = scenLabs))
  barPlots
}
treeBiomassSummarySPplot <- makeBiomassChangePlotSP(treeBiomassSummary)

# Leading vegetation
rasPath <- "~/projects/NWT/outputs/posthoc/vegetationResults"
treeLeadingSummary <- lapply(ys, function(Year){
    allSc <- lapply(mods, function(Scenario){
    message(paste0("Calculating summary statistics for sp leading for ", 
                   Scenario, " ", Year))
      ras <- raster::raster(file.path(rasPath, paste0("leadingSpeciesRaster_",
                                Scenario,"_", Year,".tif")))
    library("data.table")
    DT <- na.omit(data.table::data.table(pixelID = 1:ncell(ras),
                                         val = raster::getValues(ras),
                                         region = raster::getValues(x = divRas)))
    convTableLead <- as.data.table(ras@data@attributes)
    convTableLead <- convTableLead[category != "",]
    names(convTableLead) <- c("val", "leading")
    DT <- merge(DT, convTableLead, by = "val")    
    DT <- merge(DT, convTable, by = "region")
    DT[, c("val","region","pixelID") := NULL]
    DT$year <- Year
    DT$scenario <- Scenario
    return(DT)
    })
})

# Summarizing leading vegetation
treeLeadingSummary2 <- unlist(treeLeadingSummary, recursive = FALSE)
treeLeadingSummary <- rbindlist(treeLeadingSummary2)
treeLeadingSummary[, freq := .N, by = c("leading", "regionName", "year", "scenario")]
treeLeadingRed <- unique(treeLeadingSummary)
treeLeadingRedTotal <- treeLeadingRed[year == 2011, sum(freq, na.rm = TRUE), 
                                      by = c("regionName", "year", "scenario")]
treeLeadingRedTotal[, year := NULL]
names(treeLeadingRedTotal) <- c("region", "scenario", "totalPixels2011")

# To make the difference
diffLeading <- data.table(dcast(treeLeadingRed, 
                                formula = leading + regionName + scenario ~ year, 
                                value.var = "freq"))
names(diffLeading) <- c("leading", "region", "scenario", "year2011", "year2100")
for (j in seq_len(ncol(diffLeading)))
  set(diffLeading,which(is.na(diffLeading[[j]])),j,0)
diffLeading[, diffLeadingSp := year2100-year2011]
diffLeading[, c("year2100", "year2011") := NULL]
diffLeading[, totalChangedPixels := sum(abs(diffLeadingSp), na.rm = TRUE), by = c("region", "scenario")]
diffLeading[, percOfChangedPixels := 100*round(diffLeadingSp/totalChangedPixels, 3)]
# ASSERTION
# Percentage over the pixels that CHANGED, 
# not over the total amount of pixels!!!
diffLeading[, sum(abs(percOfChangedPixels), na.rm = TRUE), by = c("region", "scenario")]

diffLeading <- merge(diffLeading, treeLeadingRedTotal)
diffLeading[, percOfChangedTotal := 100*round(diffLeadingSp/totalPixels2011, 3)]
diffLeading[, percOfPixChanged := 100*round(totalChangedPixels/totalPixels2011, 3)]
diffLeading[, sum(diffLeadingSp, na.rm = TRUE), by = c("region", "scenario")] # Number of pixels lost/gained

# For all areas
diffLeading[, totalChangedPixelsAllAreas := sum(abs(diffLeadingSp), na.rm = TRUE), by = "scenario"]
treeLeadingRedTotalAll <- treeLeadingRed[year == 2011, sum(freq, na.rm = TRUE), 
                                      by = c("year", "scenario")]
treeLeadingRedTotalAll[, year := NULL]
names(treeLeadingRedTotalAll) <- c("scenario", "totalPixels2011All")
diffLeading <- merge(diffLeading, treeLeadingRedTotalAll, by = "scenario")
diffLeading[, percOfPixChangedAll := 100*round(totalChangedPixelsAllAreas/totalPixels2011All, 3)]

# Number of pixels lost/gained
diffLeading[, sum(diffLeadingSp, na.rm = TRUE), by = c("region", "scenario")]

# % of area that changed species composition
unique(diffLeading[, c("region", "scenario", "percOfPixChanged")])
unique(diffLeading[, c("scenario", "percOfPixChangedAll")])

# Classify the changes deciduous x conifer, pure x mixed
diffLeading[, typeLeading := ifelse(grepl(pattern = "Mixed", x = leading), "Mixed", "Pure")]
diffLeading[, typeSpecies := ifelse(grepl(pattern = "Betu_Pap|Lari_Lar|Popu_Tre", x = leading), "Deciduous", "Conifer")]
diffLeading[, fullType := paste0(typeLeading, "_", typeSpecies)]
# diffLeading[, diffByType := sum(diffLeadingSp, na.rm = TRUE), by = c("region", "scenario", "fullType")]

diffLeadingFull <- copy(diffLeading)

# Here I can make the pizza plot of the percentages! Pure_Dec, Pure_Con, Mix_dec, Mix_Con
# TODO plot?
library(ggplot2)
pal <- c("#27408B", "#8B7D6B", "#CD0000", "#EEB422", "#9A32CD", "#006400",
         "#A6BFFF", "#F1E3D1", "#FF7F7F", "#FFDC66", "#FFB1FF", "#7FE37F")
treeLeadingRed[, leading := factor(x = leading, levels = c(
"Betu_Pap", "Lari_Lar", "Pice_Gla", "Pice_Mar", "Pinu_Ban", "Popu_Tre",
"Mixed_Betu_Pap", "Mixed_Lari_Lar", "Mixed_Pice_Gla", "Mixed_Pice_Mar", "Mixed_Pinu_Ban", "Mixed_Popu_Tre"))]
# Pizza plot 2011

# TO Save the table:
paper1TableA1 <- copy(treeLeadingRed)
# Remove totalLeading, ypos
# Multiply totByLeading nby 6.25
paper1TableA1[, c("totalLeading", "ypos", "percLeading") := NULL]
paper1TableA1[, totByLeading := round(totByLeading*6.25, 0)]
names(paper1TableA1) <- c("Dominant Species", "Year of Simulation", "Scenario", "Total Area (ha)")
write.csv(paper1TableA1, file = file.path(Paths$outputPath, "TableA1.csv"))
library("googledrive")
drive_upload(file.path(Paths$outputPath, "TableA1.csv"), as_id("1saYqg2kJ0M1PALY-EsHaexJr9BLahX5v"))

library(ggplot2)
treeLeadingRed[, totByLeading := sum(freq), by = c("leading", "year", "scenario")]
treeLeadingRed[, totalLeading := sum(freq), by = c("year", "scenario")]
treeLeadingRed[, percLeading := round((totByLeading/totalLeading)*100,1), by = c("year", "scenario")]
treeLeadingRedFull <- copy(treeLeadingRed)
treeLeadingRed[, c("regionName", "freq") := NULL]
treeLeadingRed <- unique(treeLeadingRed)
treeLeadingRed[, ypos := cumsum(percLeading) - 0.5*percLeading, by = c("year", "scenario")]

library(dplyr)

# 2011 Non Climate Sensitive
# ####
treeLeadingRed_2011nCS <- treeLeadingRed[year == 2011 & scenario == "LandR_SCFM",]
treeLeadingRed_2011nCS <- treeLeadingRed_2011nCS %>% 
  arrange(desc(leading)) %>%
  mutate(prop = totByLeading / sum(treeLeadingRed_2011nCS$totByLeading) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
treeLeadingRed_2011nCS[, prop := round(prop, 2)]

pp2011_nCS <- ggplot(treeLeadingRed_2011nCS, 
                     aes(x = "", y = percLeading, fill = leading)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal) +
  labs(fill = "Dominant Tree Species") + 
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_text(size = 18),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    # Change legend key size and key width
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(1.5,"cm"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)) +
  geom_label_repel(aes(y = ypos, label = paste0(prop, "%")),
                   size = 9,
                   box.padding   = 0, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   show.legend = FALSE) +
  theme(legend.position = "bottom")+
  ylab("Non-climate-sensitive models")
pp2011_nCS
# ####

# 2011 Climate Sensitive
# ####
treeLeadingRed_2011CS <- treeLeadingRed[year == 2011 & scenario == "LandR.CS_fS",]
treeLeadingRed_2011CS <- treeLeadingRed_2011CS %>% 
  arrange(desc(leading)) %>%
  mutate(prop = totByLeading / sum(treeLeadingRed_2011CS$totByLeading) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
treeLeadingRed_2011CS[, prop := round(prop, 2)]

pp2011_CS <- ggplot(treeLeadingRed_2011CS, 
                    aes(x = "", y = percLeading, fill = leading)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal) +
  labs(fill = "Dominant Tree Species")+
  theme(
    axis.title.x=element_blank(),
    axis.title.y=element_text(size = 18),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    # Change legend key size and key width
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(1.5,"cm"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)) +
  geom_label_repel(aes(y = ypos, label = paste0(prop, "%")),
                   size = 9,
                   box.padding   = 0, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   show.legend = FALSE) + 
  theme(legend.position = "bottom") +
  ylab("Climate-sensitive models")
pp2011_CS
# ####

# 2100 Non Climate Sensitive
# ####
treeLeadingRed_2100nCS <- treeLeadingRed[year == 2100 & scenario == "LandR_SCFM",]
treeLeadingRed_2100nCS <- treeLeadingRed_2100nCS %>% 
  arrange(desc(leading)) %>%
  mutate(prop = totByLeading / sum(treeLeadingRed_2100nCS$totByLeading) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
treeLeadingRed_2100nCS[, prop := round(prop, 2)]

pp2100_nCS <- ggplot(treeLeadingRed_2100nCS, 
                     aes(x = "", y = percLeading, fill = leading)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal) +
  labs(fill = "Dominant Tree Species") + 
  theme(
    axis.title.x=element_text(size = 18),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    # Change legend key size and key width
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(1.5,"cm"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)) +
  geom_label_repel(aes(y = ypos, label = paste0(prop, "%")),
                   size = 9,
                   box.padding   = 0, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   show.legend = FALSE) + 
  theme(legend.position = "none") +
  ylab("Non-climate-sensitive models")
pp2100_nCS
# ####

# 2100 Climate Sensitive
# ####
treeLeadingRed_2100CS <- treeLeadingRed[year == 2100 & scenario == "LandR.CS_fS",]
treeLeadingRed_2100CS <- treeLeadingRed_2100CS %>% 
  arrange(desc(leading)) %>%
  mutate(prop = totByLeading / sum(treeLeadingRed_2100CS$totByLeading) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop)
treeLeadingRed_2100CS[, prop := round(prop, 2)]

pp2100_CS <- ggplot(treeLeadingRed_2100CS, 
                    aes(x = "", y = percLeading, fill = leading)) +
  geom_bar(width = 1, stat = "identity", color="white") +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = pal) +
  labs(fill = "Dominant Tree Species") + 
  theme(
    axis.title.x=element_text(size = 18),
    axis.title.y=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    # Change legend key size and key width
    legend.key.size = unit(1.5, "cm"),
    legend.key.width = unit(1.5,"cm"),
    legend.text = element_text(size = 18),
    legend.title = element_text(size = 18)) +
  geom_label_repel(aes(y = ypos, label = paste0(prop, "%")),
                   size = 9,
                   box.padding   = 0, 
                   point.padding = 0,
                   segment.color = 'grey50',
                   show.legend = FALSE)  + 
  theme(legend.position = "right") +
  ylab("Climate-sensitive models")

library("ggpubr")
# Extract the legend. Returns a gtable
leg <- get_legend(pp2100_CS)

# Convert to a ggplot and print
leg <- as_ggplot(leg)

pp2100_CS <- pp2100_CS + theme(legend.position = "none") 
# ####
library("cowplot")

# NEED TO INCREASE SIZE OF TEXT IN GENERAL

pizzaPlots <- plot_grid(pp2100_nCS, pp2100_CS, leg, ncol = 3,
                        rel_heights = c(1, 1, 2))

ggsave(file.path(Paths$outputPath, "proportionsPlot.png"), 
       device = "png",
       plot = pizzaPlots, width = 28, height = 10)

#CLEANUP
diffLeading[, c("typeSpecies", "typeLeading", "totalPixels2011", "percOfPixChanged",
                "totalPixels2011All", "percOfPixChangedAll", "totalChangedPixelsAllAreas") := NULL]
# MAX
#for each group of region, scenario 
#find the row indices where leading == max Lead  
diffLeading[, maxLead := max(diffLeadingSp), by = .(region, scenario)]
idxDT <- diffLeading[, .I[diffLeadingSp == maxLead[.N]], .(region, scenario)]
#for those row indices, set the leadSP to be leading
#(NA rows or excluded rows defaults to NA by design in data.table)
diffLeading[idxDT$V1, leadSP := leading]
# MaxTB
maxTB <- diffLeading[idxDT$V1, c("scenario", "region", "leadSP")]

# MIN
diffLeading[, minLead := min(diffLeadingSp), by = .(region, scenario)]
idxDT <- diffLeading[, .I[diffLeadingSp == minLead[.N]], .(region, scenario)]
#for those row indices, set the leadSP to be leading
#(NA rows or excluded rows defaults to NA by design in data.table)
diffLeading[idxDT$V1, lastSP := leading]
# MinTB
minTB <- diffLeading[idxDT$V1, c("scenario", "region", "lastSP")]

diffLeading[, c("lastSP", "leadSP") := NULL]
diffLeading <- merge(diffLeading, minTB, by = c("scenario", "region"))
diffLeading <- merge(diffLeading, maxTB, by = c("scenario", "region"))

percTable <- diffLeading[, c("scenario", "region", "leading", "percOfChangedPixels")]

diffLeading[, c("leading", "diffLeadingSp", "totalChangedPixels",
                                "percOfChangedPixels","percOfChangedTotal","fullType"):=NULL]
diffLeading <- unique(diffLeading)
diffLeadingMax <- diffLeading[, c("scenario", "region", "maxLead", "leadSP")]
names(diffLeadingMax)[names(diffLeadingMax) == "leadSP"] <- "leading"
diffLeadingMax <- merge(diffLeadingMax, percTable, all.x = TRUE, all.y = FALSE, 
                        by = c("scenario", "region", "leading"))
diffLeadingMin <- diffLeading[, c("scenario", "region", "minLead", "lastSP")]
names(diffLeadingMin)[names(diffLeadingMin) == "lastSP"] <- "leading"
diffLeadingMin <- merge(diffLeadingMin, percTable, all.x = TRUE, all.y = FALSE, 
                        by = c("scenario", "region", "leading"))
names(diffLeadingMax)[names(diffLeadingMax) == "percOfChangedPixels"] <- "percChangeMax"
names(diffLeadingMin)[names(diffLeadingMin) == "percOfChangedPixels"] <- "percChangeMin"
names(diffLeadingMin)[names(diffLeadingMin) == "leading"] <- "last"

finalLeadingTable <- merge(diffLeadingMax, diffLeadingMin, by = c("scenario", "region"))

# TODO plot?
# makeLeadingChangePlot <- function(treeLeadingSummary){
#   barPlots <- ggplot(data = treeLeadingSummary, 
#                      aes(x = regionName, y = Fmean, 
#                          ymax = Fmean + Fse,
#                          ymin = Fmean - Fse, 
#                          fill = regionName)) +
#     geom_bar(stat = "identity", position = position_dodge()) +
#     geom_errorbar(width = 0.2, position = position_dodge(.9)) +
#     scale_fill_manual(values = c("darkred", "darkgoldenrod", "darkblue","darkgreen")) +
#     geom_text(aes(label = round(Fmean, 0)), vjust = -3, color = "black", size = 3) +
#     labs(x = "Regions", y = paste0("Average value")) +
#     theme_bw() +
#     theme(legend.title = element_blank(),
#           legend.position = "right") +
#     coord_cartesian(ylim = c(lowLim, upLim))
#   # barPlots <- barPlots + geom_point(aes(x = as.factor(year), y = Fmedian), 
#   #                                   position = position_dodge(.9))
#   barPlots <- barPlots + facet_grid(species ~ scenario, scales = "free_y")
#   barPlots
# }

# FIRE STATS
outPath <- "~/projects/NWT/outputs/SIMULATIONS"
clim <- qs::qread(file.path(outPath, "burnSummaryTableLandR.CS_fS.qs"))
nonClim <- qs::qread(file.path(outPath, "burnSummaryTableLandR_SCFM.qs"))

# Get the average values by year
clim[, average := mean(val), by = c("year", "var")]
clim[, sd := sd(val), by = c("year", "var")]
clim[, val := NULL]
clim[, repetition := NULL]
climRed <- unique(clim)
climRed <- climRed[year %in% c(2011, 2100), ]
climReddAv <- data.table(dcast(climRed, formula = var ~ year, value.var = "average"))
climReddSD <- data.table(dcast(climRed, formula = var ~ year, value.var = "sd"))
names(climRedd) <- c("variable", "year2011", "year2100")
climRedd[, percChange := round((year2100-year2011)/year2011, 2)]

nonClim[, average := mean(val), by = c("var")]
nonClim[, sd := sd(val), by = c("var")]
nonClim[, val := NULL]
nonClim[, repetition := NULL]
nonClim[, year := NULL]
nonClimRed <- unique(nonClim)
