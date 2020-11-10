library("ggplot2")
library("data.table")
library("raster")
library("tictoc")
library("rasterVis")
library("ggthemes")
library("grid")
library("viridis")
library("gridExtra")
library("pals")
library("gbm")
library("ggrepel")

folderColonization <- "~/projects/NWT/outputs/posthoc/colonization"
Species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
             "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
             "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
             "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
             "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
             "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
             "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
             "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA")
Effect <- c("vegetation", "fire", "climate")
Process <- c("colonization", "extirpation")
noCorner <- raster("~/projects/NWT/inputs/NWT_BCR6/RTM_noCorner.tif") # TODO temporary

############################################# PLOTS
    ##### ~~~~ BIRD MODEL CHECK ~~~~ #####

# speciesOfInterest <- c("NOWA", "FOSP", "HOLA", "OSFL", "PISI", "SWSP", "TEWA", "WEWP", 
#                        "WWCR", "ATSP", "BOCH", "COYE", "CAWA", "REVI", "WCSP")
# speciesExample <- c("COYE", "CAWA", "REVI", "WCSP")

modsPath <- "~/projects/NWT/outputs/SIMULATIONS/birdCovariates.qs"

if (!file.exists(modsPath)){
  mods <- lapply(species, FUN = function(sp){
    MOD <- get(load(paste0("~/projects/NWT/modules/birdsNWT/data/models/", sp,
                           "brt6a.R")))
    MODsum <- data.table(summary(MOD))
    MODsum[, cumSum := cumsum(rel.inf)]
    MODsum[, species := sp]
    MODsum[, Rank := 1:NROW(MODsum)]
    return(MODsum)
  })
  names(mods) <- species
  allMods <- rbindlist(mods)  
  qs::qsave(allMods, modsPath)
} else {
  allMods <- qs::qread(modsPath)
}

# allModsSimp <- allMods[Rank < 4,]
covs <- unique(allMods$var)

speciesVar <- grepMulti(x = covs, patterns = "Species|Structure")
Topo <- c("dev25", "vrug", "wet", "wat", "led25")
Climate <- covs[!covs %in% c(speciesVar, Topo)]

allMods[, group := ifelse(var %in% speciesVar, "vegetation", ifelse(var %in% Topo, "topographic", "climate"))]
allMods[, group := factor(group, levels = c("climate", "vegetation", "topographic"))]
allMods[, sumByGroup := sum(rel.inf), by = c("species", "group")]
allModsSimp <- unique(allMods[, c("species", "group", "sumByGroup")])

library("ggplot2")
setkey(allModsSimp, "sumByGroup")
setkey(allModsSimp, "group")

clim <- allModsSimp[group == "climate", ]
setkey(clim, "sumByGroup")
topo <- allModsSimp[group == "topographic", ]
setkey(topo, "sumByGroup")
veg <- allModsSimp[group == "vegetation", ]
setkey(veg, "sumByGroup")
dt <- rbind(clim, topo, veg)

dt2 <- copy(dt)
dt2 <- dcast(dt2, species ~ group, value.var = "sumByGroup")
dt3 <- dt2[order(rank(vegetation), -rank(climate))]
orderSpecies <- as.character(dt3$species)

dt[, species := factor(species, levels = orderSpecies)]
dt[, group := factor(group, levels = c("climate", "topographic", "vegetation"))]

dt0 <- dt[group != "topographic"]
dt0[, totalSum := sum(sumByGroup), by = "species"]
dt0[, proportion := sumByGroup/totalSum]
speciesOrder <- dt0[group == "climate", c("species", "proportion")]
setkey(speciesOrder, "proportion")
speciesOrder <- as.character(speciesOrder[,species])
dt0[, species := factor(species, levels = speciesOrder)]

p0 <- ggplot(dt0, aes(x = proportion, y = species, 
                      fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("blue", "darkgreen")) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())
p0

# p1 <- ggplot(dt[species %in% speciesOfInterest], aes(x = species, y = sumByGroup, 
#                      fill = group)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_manual(values = c("darkred", "blue", "darkgreen"))
# p1
# p2 <- ggplot(dt[species %in% speciesExample], aes(x = species, y = sumByGroup, 
#                                                      fill = group)) +
#   geom_bar(position = "stack", stat = "identity") +
#   scale_fill_manual(values = c("darkred", "blue", "darkgreen"))
# p2
# 

############################################# PLOT1 
#### ~~~ FULL TABLE ~~~ ####

tic("Full table elapsed time:")
fullTablePath <- file.path(folderColonization, "fullTablePlot.qs")
if (!file.exists(fullTablePath)){
  fullTable <- lapply(Species, function(sp){
    spTable <- rbindlist(lapply(Effect, function(eff){
      effTable <- rbindlist(lapply(Process, function(proc){
        ras <- raster(file.path(folderColonization, paste0("difference_", eff,
                                                           "_", sp, 
                                                           "_", proc, ".tif")))
        # TODO Temporary fixing corner
        ras[is.na(noCorner)] <- NA # <~~~~~REMOVE
        DT <- na.omit(data.table(probability = round(getValues(ras), 3),
                                 species = sp,
                                 process = proc,
                                 effect = eff))
        return(DT)
      }))
      return(effTable)
    }))
    return(spTable)
  }) 
  names(fullTable) <- Species
  qs::qsave(fullTable, fullTablePath)
} else {
  fullTable <- qs::qread(fullTablePath)
}
toc()

tic("Full table netEffect elapsed time:")
fullTableNEPath <- file.path(folderColonization, "fullTableNEPlot.qs")
if (!file.exists(fullTableNEPath)){
  fullTableNE <- lapply(Species, function(sp){
    spTable <- rbindlist(lapply("netEffect", function(eff){
      effTable <- rbindlist(lapply(Process, function(proc){
        ras <- raster(file.path(folderColonization, paste0("difference_", eff,
                                                           "_", sp, 
                                                           "_", proc, ".tif")))
        # TODO Temporary fixing corner
        ras[is.na(noCorner)] <- NA # <~~~~~REMOVE
        DT <- na.omit(data.table(probability = round(getValues(ras), 3),
                                 species = sp,
                                 process = proc,
                                 effect = eff))
        return(DT)
      }))
      return(effTable)
    }))
    return(spTable)
  })
  names(fullTableNE) <- Species
  qs::qsave(fullTableNE, fullTableNEPath)
} else {
  fullTableNE <- qs::qread(fullTableNEPath)
}
toc()

tic("netChangeTable")
netChangeTablePath <- file.path(folderColonization, "netChangeTablePlot.qs")
if (!file.exists(netChangeTablePath)){
  netChangeTable <- rbindlist(lapply(Species, function(sp){
    DT <- fullTable[[sp]]
    DT2 <- fullTableNE[[sp]]
    DT <- rbind(DT, DT2)
    DT[, expectedAreaChange := as.double(6.25*(sum(probability))), by = c("process", "effect")]
    DT <- DT[, probability := NULL]
    DT <- unique(DT)
    DT <- dcast(DT, species + effect ~ process, value.var = "expectedAreaChange")
    DT[, netChange := colonization - extirpation]
    return(DT)
  }))
  netChangeTable <- data.table(netChangeTable)
  netChangeTable[, effect := factor(effect, levels = c("fire", "vegetation", "climate", "netEffect"))]
  speciesOrder <- netChangeTable[effect == "climate", ]
  setkey(speciesOrder, "netChange")
  speciesOrder <- as.character(speciesOrder[["species"]])
  netChangeTable[, species := factor(species, levels = speciesOrder)]
  qs::qsave(netChangeTable, netChangeTablePath)
} else {
  netChangeTable <- qs::qread(netChangeTablePath)
}
toc()

netEffTable <- netChangeTable[effect == "netEffect", c("species", "netChange")]
names(netEffTable) <- c("species", "totalNetEffect")
netChangeTable <- merge(netChangeTable, netEffTable, by = "species")
lapply(X = Species, function(sp){
  DT <- netChangeTable[species == sp & effect != "netEffect",]
  signal <- ifelse(unique(DT[["totalNetEffect"]]) > 0, "> 0", "< 0")
  jit <- ifelse(unique(DT[["totalNetEffect"]]) > 0, 0.13, -0.13)
  S <-  sum(DT[eval(parse(text = paste0("netChange", 
                                        signal))), 
               netChange])
  pos <- S/(10^7)
  pos <- pos + jit
  netChangeTable[species == sp, labelMark := pos]
  return("OK")
})
toc()

newSortOrder <- setkey(unique(netChangeTable[, c("species", "totalNetEffect")]), "totalNetEffect")

newSortOrder <- as.character(newSortOrder[["species"]])
netChangeTable[, species := factor(species, levels = newSortOrder)]

p1 <- ggplot(data = netChangeTable[effect != "netEffect"], aes(x = netChange/(10^7), 
                                               y = species,
                                               group = effect,
                                               color = effect,
                                               fill = effect)) +
  geom_col(aes(fill = effect)) + 
  # facet_wrap(group ~ ., scales = "free_y") + # Only if bird grouping is done!
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        legend.title = element_blank()) +
        # plot.margin = unit(c(5.5,0,5.5,5.5), "pt")) + 
  scale_color_manual(values = c("fire" = "firebrick3", 
                                "vegetation" = "darkgreen", 
                                "climate" = "dodgerblue3"), 
                    labels = c("fire" = "Climate Effect via Fire", 
                               "vegetation" = "Climate Effect via Vegetation", 
                               "climate" = "Direct Climate Effect")) +
  scale_fill_manual(values = c("fire" = "firebrick2", 
                                "vegetation" = "forestgreen", 
                                "climate" = "dodgerblue2"), 
                     labels = c("fire" = "Climate Effect via Fire", 
                                "vegetation" = "Climate Effect via Vegetation", 
                                "climate" = "Direct Climate Effect")) +
  xlab("Expected area in ha colonized and extirpated (x 10\u2077)") +
  geom_vline(xintercept = 0, color = "black") + 
  geom_text(aes(x = labelMark, 
                label = round(totalNetEffect/(10^7), 2)), 
            size = 2.5, color = "grey10", check_overlap = TRUE) + # check_overlap = TRUE
  geom_vline(xintercept = sum(unique(netChangeTable[, totalNetEffect]))/(64*(10^7)), 
             color = "grey40", linetype = "dotdash") +
  geom_text(aes(x = ifelse(species == netChangeTable[["species"]][(NROW(netChangeTable)-7)], -0.8, NA), 
                label = paste0("Expected average area",
                               "\naffected by climate change")),
             color = "grey40", fill = "white", size = 2.8, check_overlap = TRUE)

p1
ggsave(device = "png", filename = file.path(folderColonization, "affectedAreaByClimateChange.png"), 
       width = 8, height = 11)

# Taiga Plains in the NWT = 480,493 km2 --> 48 049 300 ha --> 4,8 x 10^7 ha
# up to 50% of the Taiga Plains within NWT could change with climate change

############################################# PLOT2 
#### ~~~ FULL TABLE PLOT SEPARATING EFFECTS AND DIRECTION ~~~ ####

netChangeTable[, totalNetEffectByEffect := sum(netChange), by = "effect"]

p2 <- ggplot(data = netChangeTable[effect != "netEffect"], aes(y = species,
                                                               group = effect,
                                                               color = effect,
                                                               fill = effect)) +
  geom_col(aes(x = colonization/(10^7)), alpha = 0.5) + # COLONIZATION
  geom_col(aes(x = -(extirpation/(10^7))), alpha = 0.5) + # EXTIRPATION
  geom_col(aes(x = netChange/(10^7))) + # NET CHANGE
  # facet_wrap(group ~ ., scales = "free_y") + # Only if bird grouping is done!
  facet_grid(. ~ effect, scales = "free_x") + # Only if bird grouping is done!
  scale_color_manual(values = c("fire" = "firebrick3", 
                                "vegetation" = "darkgreen", 
                                "climate" = "dodgerblue3"), 
                     labels = c("fire" = "Climate Effect via Fire", 
                                "vegetation" = "Climate Effect via Vegetation", 
                                "climate" = "Direct Climate Effect")) +
  scale_fill_manual(values = c("fire" = "firebrick2", 
                               "vegetation" = "forestgreen", 
                               "climate" = "dodgerblue2"), 
                    labels = c("fire" = "Climate Effect via Fire", 
                               "vegetation" = "Climate Effect via Vegetation", 
                               "climate" = "Direct Climate Effect")) +
  xlab("Expected area in ha colonized and extirpated (x 10\u2077)") +
  geom_vline(xintercept = 0, color = "black") + 
  geom_vline(data = netChangeTable[effect == "climate"], 
             aes(xintercept = totalNetEffectByEffect/(64*(10^7))), 
             colour = "darkblue", 
             linetype = "dotdash") + 
  geom_vline(data = netChangeTable[effect == "fire"], 
             aes(xintercept = totalNetEffectByEffect/(64*(10^7))), 
             colour = "darkred", 
             linetype = "dotdash") + 
  geom_vline(data = netChangeTable[effect == "vegetation"], 
             aes(xintercept = totalNetEffectByEffect/(64*(10^7))), 
             colour = "darkgreen", 
             linetype = "dotdash") +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), 
        strip.text = element_blank(),
        strip.background = element_blank(),
        legend.position = "none",
        legend.title = element_blank())
p2
ggsave(device = "png", filename = file.path(folderColonization, "affectedAreaByClimateChangePerEffect.png"), 
       width = 8, height = 11)

############################################# PLOT3
#### ~~~ SUMMARIZED TABLE FOR PLOT (DENSITY PLOT) ~~~ ####
# We have decided not to use this one
if (FALSE){
  tic("Summary table elapsed time:")
  summTablePath <- file.path(folderColonization, "summTablePlot.qs")
  if (!file.exists(summTablePath)){
    summTable <- rbindlist(lapply(fullTable, function(DT){
      DT[, counts := .N, by = c("probability", "process", "effect")]
      DT <- unique(DT)
      return(DT)
    }))
    summTable[, species := NULL]
    summTable[, fullCounts := sum(counts), by = c("effect", "process", 
                                                  "probability")]
    summTable[, counts := NULL]
    summTable <- unique(summTable)
    qs::qsave(summTable, summTablePath)
  } else {
    summTable <- qs::qread(summTablePath)
  }
  toc()
  
  summTable[, totalArea := probability * fullCounts]
  summTable[, areaByEffect := sum(totalArea), by = c("process", "effect")]
  summTableSim <- copy(summTable)
  summTableSim[, c("probability", "fullCounts", "totalArea") := NULL]
  summTableSim <- unique(summTableSim)
  
  p3 <- ggplot(data = summTableSim, 
               aes(x = totalArea/(64*(10^7)),
                   y = totalArea,
                   group = effect,
                   color = effect,
                   fill = effect)) +
    # geom_area(position = "identity", alpha = 0.4) +
    geom_bar() +
    # geom_density(alpha = 0.4) +
    facet_grid(. ~ process) #+
  xlab("Effect of climate on the probability of boreal songbird presence to change") +
    ylab("Area in ha (x 10^7)")
  
  p3
  
  ggsave(device = "png", filename = file.path(folderColonization, "climateChangeDensityPlot.png"), 
         width = 8, height = 11)
}
#### ~~~ CHANGE IN AREA (COLOZATION/EXTIRPATION PROPORTION PLOT) ~~~ ####

library(raster)
library(tictoc)
library(data.table)
library(reproducible)
source('~/projects/NWT/posthocFunctions/calcProportionPixelsLost.R')

file.exists(file.path(folderColonization, "proportionPixelsChangedTable.qs"))

p4 <- calcProportionPixelsLost(species = Species,
                               newSortOrder = newSortOrder,
                                    outputFolder = folderColonization,
                                    netChangeTable = qs::qread("/home/tmichele/projects/NWT/outputs/posthoc/colonization/netChangeTablePlot.qs"),
                                    # Needs to have the netChange column! This col is in area in ha 6.25*(sum(probabilities))
                                    # netChangeTable comes from plotsPaper_NotFun.R
                                    useFuture = TRUE)

############################################# ALL PLOTS TOGETHER!

# Put all three plots together in one big landscape plot

birdPlots <- gridExtra::grid.arrange(p1, p2, p4, ncol = 3)

ggsave(file.path(folderColonization, "birdPlot.png"), device = "png",
       plot = birdPlots, width = 16, height = 9)


############################################# MAPS -- for each bird -- Appendix
#### ~~~ NET CHANGE PER EFFECT ~~~ ####

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
      # >>>>> HERE, CUT CORNER
      # TODO Temporary fixing corner
      r[is.na(noCorner)] <- NA # <~~~~~REMOVE
      return(r)
    }))
    names(allEffects) <- realEffect
    return(allEffects)
  })
  fileNamePath <- file.path("~/projects/NWT/outputs/posthoc/colonization", 
                            paste0(realEffect, "_netColonization_", species))
  # if (any(!file.exists(paste0(fileNamePath, ".tif")))){
    netColonization <- (allColExt[[1]] - allColExt[[2]])/2
    names(netColonization) <- paste0(realEffect, "_netColonization_", species)
    lapply(1:nlayers(netColonization), function(index){
      writeRaster(netColonization[[index]], 
                  filename = fileNamePath[index], 
                  format = "GTiff", overwrite = TRUE)
    })
  # } else {
    # netColonization <- raster::stack(paste0(fileNamePath, ".tif"))
  # }
  names(netColonization) <- realEffect
  
  ### APPENDIX PLOT ###
  spFilePath <- file.path(folder, paste0("APPENDIX_plot_colonization_", species, ".png"))
  # if (!file.exists(spFilePath)){
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
  # }
  return(netColonization)
})
names(allSpecies) <- sp

############################################# PLOT 5
#### ~~~ CORRELATION RELATIVE INFLUENCE AND NET EFFECT ~~~ ####

netChange <- unique(netChangeTable[, c("species", "totalNetEffect")])

relativeInfluenceClimate <- dt0[group == "climate", c("species", "sumByGroup")]
names(relativeInfluenceClimate) <- c("species", "Climate")

# All together
##### 
corTable <- merge(netChange, relativeInfluenceClimate, by = "species")
tendS <-lm(totalNetEffect ~ Climate, data = corTable)
require(stats)
coeffS <- coefficients(tendS)
Fstats <- summary(tendS)$fstatistic
names(Fstats) <- NULL
pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, 
                  " (significant)", " (non-significant)")
coefXS <- round(coeffS[2],1)
coefYS <- round(coeffS[1],1)

SUB <- paste0("y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
              "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)),
              pValueS)

P5 <- ggplot(data = corTable, aes(x = totalNetEffect/(10^7), y = Climate)) +
  geom_point(color = "darkblue")  +
  geom_label_repel(aes(label = species),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  stat_smooth(method = "lm", color = "blue") + 
  theme(plot.title = element_text(hjust = 0)) +
  ylab("Relative Influence of Climate Covariates") +
  xlab("Total net effect of climate change (x 10\u2077)") + 
  labs(subtitle = paste0(SUB, "\n Total Relative Influence"), 
       title = "Total Net Effect ~ Relative Influence of Climate Covariates")
P5
ggsave(file.path(folderColonization, "relativeInfluencePlot.png"), 
       device = "png",
       plot = P5, width = 12, height = 12)

 #####

# Two plots
#####
corTable <- merge(netChange, relativeInfluenceClimate, by = "species")
corTable1 <- corTable[totalNetEffect < 0,]
corTable2 <- corTable[totalNetEffect > 0,]

tendS1 <-lm(totalNetEffect ~ Climate, data = corTable1)
tendS2 <-lm(totalNetEffect ~ Climate, data = corTable2)

require(stats)
coeffS <- coefficients(tendS1)
Fstats <- summary(tendS)$fstatistic
names(Fstats) <- NULL
pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, 
                  " (significant)", " (non-significant)")
coefXS <- round(coeffS[2],1)
coefYS <- round(coeffS[1],1)
SUB1 <- paste0("y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
              "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)),
              pValueS)

coeffS <- coefficients(tendS2)
Fstats <- summary(tendS)$fstatistic
names(Fstats) <- NULL
pValueS <- ifelse(pf(Fstats[1], Fstats[2], Fstats[3], lower.tail = F) < 0.05, 
                  " (significant)", " (non-significant)")
coefXS <- round(coeffS[2],1)
coefYS <- round(coeffS[1],1)
SUB2 <- paste0("y = ", ifelse(coefXS < 10000, coefXS, formatC(coefXS, format = "e", digits = 2)),
              "x + ", ifelse(coefYS < 10000, coefYS, formatC(coefYS, format = "e", digits = 2)),
              pValueS)


P51 <- ggplot(data = corTable1, aes(x = totalNetEffect/(10^7), y = Climate)) +
  geom_point(color = "darkblue")  +
  geom_label_repel(aes(label = species),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  stat_smooth(method = "lm", color = "blue") + 
  theme(plot.title = element_text(hjust = 0)) +
  ylab("Relative Influence of Climate Covariates") +
  xlab("Total net effect of climate change (x 10\u2077)") + 
  labs(subtitle = paste0(SUB1, "\n Total Relative Influence"), 
       title = "Total Net Effect ~ Relative Influence of Climate Covariates")
P51

P52 <- ggplot(data = corTable2, aes(x = totalNetEffect/(10^7), y = Climate)) +
  geom_point(color = "darkblue")  +
  geom_label_repel(aes(label = species),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  stat_smooth(method = "lm", color = "blue") + 
  theme(plot.title = element_text(hjust = 0)) +
  ylab("Relative Influence of Climate Covariates") +
  xlab("Total net effect of climate change (x 10\u2077)") + 
  labs(subtitle = paste0(SUB2, "\n Total Relative Influence"), 
       title = "Total Net Effect ~ Relative Influence of Climate Covariates")
P52
correlationPlots <- gridExtra::grid.arrange(P51, P52, ncol = 2)

ggsave(file.path(folderColonization, "relativeInfluencePlot2.png"), 
       device = "png",
       plot = correlationPlots, width = 12, height = 12)
#####

# One plot, both veg and clim
#####


relativeInfluenceClimate2 <- dt0[, c("species", "group", "sumByGroup")]
relativeInfluenceClimate2 <- dcast(data = relativeInfluenceClimate2, formula = species ~ group)
corTable3 <- merge(netChange, relativeInfluenceClimate2, by = "species")
corTable3[, totalNetEffect := totalNetEffect/(10^7)]
# names(corTable3) <- c("Species", "Net Climate Effect", 
#                       "Relative Climate Influence", "Relative Vegetation Influence")

corTable3[, Colour := ifelse(totalNetEffect > 0, "darkgreen", "darkred")]
corTable3[, Size := abs(round(totalNetEffect, 2))]

P53 <- ggplot(data = corTable3, aes(x = climate, 
                                    y = vegetation)) +
  geom_point(aes(colour = Colour, 
                 size = Size))  +
  scale_color_manual(name = "Habitat Change",
                     values = c("darkred" = "darkred",
                                "darkgreen" = "darkgreen"),
                     labels = c("Gain of habitat", 
                                "Loss of habitat")) +
  scale_size_binned(name = "Area in ha (x 10\u2077)") + 
  geom_label_repel(aes(label = species),
                   box.padding   = 0.5, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
  # stat_smooth(method = "lm", color = "blue") + 
  theme(plot.title = element_text(hjust = 0)) +
  ylab("Relative Influence of Vegetation Covariates") +
  xlab("Relative Influence of Climate Covariates") +
  theme(legend.position = "bottom")
  # labs(subtitle = paste0(SUB, "\n Total Relative Influence"), 
  #      title = "Total Net Effect ~ Relative Influence of Climate Covariates")
P53

ggsave(file.path(folderColonization, "relativeInfluencePlotComplete.png"), 
       device = "png",
       plot = P53, width = 12, height = 12)

############################################# MAP1

#### ~~~ HOTSPOT OF CHANGE ~~~ ####

# 0. Stack netEffect rasters, and individually each one of the effects.
spReorganized <- lapply(realEffect, function(eff){
  rasStk <- raster::stack(lapply(X = allSpecies, `[[`, eff))
  names(rasStk) <- paste0(names(allSpecies), "_", eff)
  return(rasStk)
})
names(spReorganized) <- realEffect

# MAP 1. Make a map of how many pixels (calc) are != 0 (4 maps)
.countOfNonSpecificValues <- function(x, val) {
  return(sum(x != val))
}
.sumAbsoluteValues <- function(x) {
  return(sum(abs(x)))
}
hotspotChanges <- lapply(names(spReorganized), function(eff){
  tic(paste0("Calculating hotpots of change for ", eff))
  # hot <- calc(x = spReorganized[[eff]], fun = function(x){.countOfNonSpecificValues(x, val = 0)},
  #                                filename = file.path(folderColonization, paste0(eff, "_hotspotsMap")),
  #                                format = "GTiff")
  hot <- calc(x = spReorganized[[eff]], fun = function(x){.sumAbsoluteValues(x)},
              filename = file.path(folderColonization, paste0(eff, "_hotspotsMap")),
              format = "GTiff", overwrite = TRUE)
  toc()
  return(hot)
})
names(hotspotChanges) <- names(spReorganized)

legends <- list("Total number of species expected to change due to direct climate effects",
                "Total number of species expected to change due to climate effects via vegetation",
                "Total number of species expected to change due to climate effects via fire",
                "Total number of species expected to change due to climate change")
names(legends) <- names(hotspotChanges)


nlev <- 200
palBlue <- colorRampPalette(brewer.pal(9, "Blues"))(26)
# RColorBrewer::brewer.pal(n = 26, "Blues")
lapply(names(hotspotChanges), function(eff){
  effectHotspotPath <- file.path(folder, paste0("hotspots3_", eff, ".png"))
  if (!file.exists(effectHotspotPath)){
    ras <- hotspotChanges[[eff]]
    ras[is.na(flammableRTM)] <- NA
    png(filename = effectHotspotPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(ras,
                    sub = legends[[eff]],
                    margin = FALSE,
                    maxpixels = 6e6,
                    colorkey = list(
                      space = 'bottom',
                      labels = list(at = round(seq(from = cellStats(hotspotChanges[[eff]], "min"),
                                             to = cellStats(hotspotChanges[[eff]], "max"),
                                             length.out = nlev + 1),0), font = 4),
                      axis.line = list(col = 'black'),
                      width = 0.75
                    ),
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = palBlue, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")))
    dev.off()
  }
})

############################################# MAP2
#### ~~~ NET CHANGE ~~~ ####
# MAP 2. Sum all probabilities across species: lighter color x brighter color, -1:1 (4 maps)
# Interpretation on MAP 2: Each pixel is expected to lose or win this number of species (-64 to 64)

speciesChanges <- lapply(names(spReorganized), function(eff){
  tic(paste0("Calculating change in species for ", eff))
  chng <- calc(x = spReorganized[[eff]], fun = sum,
              filename = file.path(folderColonization, paste0(eff, "_changeInSpeciesMap")),
              format = "GTiff")
  toc()
  return(chng)
})
names(speciesChanges) <- names(spReorganized)

library(viridis)

legends <- list("Expected number of species to change due to direct climate effects",
                "Expected number of species to change due to climate effects via vegetation",
                "Expected number of species to change due to climate effects via fire",
                "Expected number of species to change due to climate change")
names(legends) <- names(speciesChanges)

pal <- pals::brewer.rdylbu(100)
lapply(names(speciesChanges), function(eff){
  speciesChangesPath <- file.path(folder, paste0("changeInSpecies_", eff, ".png"))
  if (!file.exists(speciesChangesPath)){
    ras <- speciesChanges[[eff]]
    ras[is.na(flammableRTM)] <- NA
    m <- max(abs(cellStats(speciesChanges[[eff]], "max")),
             abs(cellStats(speciesChanges[[eff]], "min")))
    AT <- round(seq(from = -m, to = m, length.out = length(pal) + 1), 0)
    png(filename = speciesChangesPath,
        width = 21, height = 29,
        units = "cm", res = 300)
    print(levelplot(ras,
                    sub = legends[[eff]],
                    margin = FALSE,
                    maxpixels = 6e6,
                    colorkey = list(
                      space = 'bottom',
                      labels = list(at = AT, font = 4),
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
  }
})

############################################# TABLE
#### ~~~ PROPORTION OF THE EFFECT ~~~ ####

# Calculating the proportion of the effects
proportionChangeTable <- copy(netChangeTable)
proportionChangeTable <- proportionChangeTable[effect != "netEffect"]
proportionChangeTable[, absoluteEffect := sum(sum(abs(netChange))), by = "species"]
proportionChangeTable[, proportionalEffect := abs(netChange)/absoluteEffect]

fireEffects <- proportionChangeTable[effect == "fire", ]
vegetationEffects <- proportionChangeTable[effect == "vegetation", ]

fireEffects[fireEffects[, .I[which.max(proportionalEffect)]]]
vegetationEffects[vegetationEffects[, .I[which.max(proportionalEffect)]]]

setkey(fireEffects, "proportionalEffect")
fireEffects # 4 species above 50% (SAVS 91%, WEWP 82%, PIWO 75%, COYE 54%)

setkey(vegetationEffects, "proportionalEffect")
vegetationEffects # 4 species above 10% (BBWA 31%, PUFI 16%, HOLA 13%, WIWR 13%)

# For all these species, expected proportion of habitat area colonized or lost due to climate change was not above
proportionArea <- data.table(p4$data)
proportionAreaChange <- proportionArea[species %in% c("SAVS", "WEWP", "PIWO", "COYE", 
                                                "BBWA", "PUFI", "HOLA", "WIWR"), 
                                 c("species", "proportionOfAreaChanged")]
setkey(proportionAreaChange, "proportionOfAreaChanged")
proportionAreaChange
# In all cases, expected proportion of habitat change was below 20%

# Write table for manuscript
proportionArea
proportionsTable <- copy(proportionChangeTable)
proportionsTable[, labelMark := NULL]
proportionArea[, c("colonization", "labelMark") := NULL]

proportionsTableDCAST <- proportionsTable[, c("species", "effect", "colonization", "extirpation", "netChange")]
proportionsTableMelted = melt(proportionsTableDCAST, id.vars = c("species", "effect"),
             measure.vars = c("colonization", "extirpation", "netChange"))
proportionsTableDCASTed <- dcast(proportionsTableMelted, species ~ effect + variable, value.var = "value")
finalProportionsTable <- merge(proportionsTableDCASTed, proportionArea, by = "species")

# write.csv(finalProportionsTable, file = file.path(getwd(), "proportionsTableSUPMAT.csv"))
# drive_upload(media = file.path(getwd(), "proportionsTableSUPMAT.csv"), as_id("17xCa7ZogxktoaTVuv7s4EIc2DVCuEq68")) # Already uploaded

