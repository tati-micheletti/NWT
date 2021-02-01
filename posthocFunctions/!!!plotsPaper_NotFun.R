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

# folderColonization <- "~/projects/NWT/outputs/posthoc/colonization"
folderColonization <- "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/colonization" # After moving to archive
Species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
             "BCCH", "BHCO", "BHVI", "BLPW", "BOCH",  "BRBL","BRCR", "BTNW", 
             "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
             "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
             "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
             "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
             "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
             "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA")
Effect <- c("vegetation", "fire", "climate")
Process <- c("colonization", "extirpation")
noCorner <- raster("~/projects/NWT/inputs/NWT_BCR6/RTM_noCorner.tif") # TODO temporary

############################################# PLOT0
##### ~~~~ BIRD MODEL CHECK ~~~~ #####

# speciesOfInterest <- c("NOWA", "FOSP", "HOLA", "OSFL", "PISI", "SWSP", "TEWA", "WEWP", 
#                        "WWCR", "ATSP", "BOCH", "COYE", "CAWA", "REVI", "WCSP")
# speciesExample <- c("COYE", "CAWA", "REVI", "WCSP")

modsPath <- "~/projects/NWT/outputs/SIMULATIONS/birdCovariates.qs"

if (!file.exists(modsPath)){
  mods <- lapply(Species, FUN = function(sp){
    MOD <- get(load(paste0("~/projects/NWT/modules/birdsNWT/data/models/", sp,
                           "brt6a.R")))
    MODsum <- data.table(summary(MOD))
    MODsum[, cumSum := cumsum(rel.inf)]
    MODsum[, species := sp]
    MODsum[, Rank := 1:NROW(MODsum)]
    return(MODsum)
  })
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

setkey(allModsSimp, sumByGroup)
allModsSimp[group == "vegetation", ]

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
dt2 <- data.table(dcast(dt2, species ~ group, value.var = "sumByGroup"))
dt3 <- dt2[order(rank(vegetation), -rank(climate))]
orderSpecies <- as.character(dt3$species)

dt[, species := factor(species, levels = orderSpecies)]
dt[, group := factor(group, levels = c("climate", "topographic", "vegetation"))]

dt0 <- dt # This to make a full plot
# dt0 <- dt[group != "topographic"] # or this to make proportional to just climate and vegetation
dt0[, totalSum := sum(sumByGroup), by = "species"]
dt0[, proportion := sumByGroup/totalSum]
speciesOrder <- dt0[group == "climate", c("species", "proportion")]
setkey(speciesOrder, "proportion")
speciesOrder <- as.character(speciesOrder[,species])
dt0[, species := factor(species, levels = speciesOrder)]

p0 <- ggplot(dt0, aes(x = proportion, y = species, 
                      fill = group)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("blue", "darkgoldenrod2", "darkgreen")) +
  theme(axis.title.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank()) +
  coord_cartesian(xlim = c(0, 1), expand = FALSE) +
  xlab("Proportional relative influence of climate sensitive landbird models' covariates")
p0

ggsave(plot = p0, 
       device = "png", 
       filename = file.path(dirname(modsPath),
                            "relativeInfluenceOfCovariates.png"), 
       width = 10, height = 10)

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
#####

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

netEffTable <- netChangeTable[effect == "netEffect", c("species", "netChange")] # <~~~~~~~~~~~~~~~~~~~~~~~ REMOVE BRBL
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
  xlab("(A) Expected area in ha colonized \nand extirpated (x 10\u2077)") +
  geom_vline(xintercept = 0, color = "black") + 
  geom_text(aes(x = labelMark, 
                label = round(totalNetEffect/(10^7), 2)), 
            size = 2.5, color = "grey10", check_overlap = TRUE) + # check_overlap = TRUE
  geom_vline(xintercept = sum(unique(netChangeTable[, totalNetEffect]))/(64*(10^7)), 
             color = "grey40", linetype = "dotdash") +
  geom_text(aes(x = ifelse(species == netChangeTable[["species"]][(NROW(netChangeTable)-7)], -1.1, NA), 
                label = paste0("Expected net area across species",
                               "\naffected by climate change")),
             color = "grey40", fill = "white", size = 2.8, check_overlap = TRUE)

p1
ggsave(device = "png", filename = file.path(folderColonization, 
                                            "affectedAreaByClimateChange.png"), 
       width = 8, height = 11)

# Taiga Plains in the NWT = 480,493 km2 --> 48 049 300 ha --> 4,8 x 10^7 ha
# up to 50% of the Taiga Plains within NWT could change with climate change
#########

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
  xlab("(B) Expected area in ha colonized \nand extirpated (x 10\u2077)") +
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
ggsave(device = "png", filename = file.path(folderColonization, 
                                            "affectedAreaByClimateChangePerEffect.png"), 
       width = 8, height = 11)
##########

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
##########

############################################# PLOT4 
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
##########

############################################# PLOT5 
#### ~~~ CHANGE IN ABUNDANCE ~~~ ####

# 1. Try to compare abundance change. 
# 1.1. abund2011 -> get predicted abundance in 2011, use the colonization 2011 cutoff, sum all
library(data.table)
library(tictoc)
library(future)
library(future.apply)
library(usefulFuns)
library(raster)

folderColonization <- "~/projects/NWT/outputs/posthoc/colonization"

abundanceChangeTable <- file.path(folderColonization, 
          "finalAbundanceMaps",
          "proportionAbundanceChangeTable.qs")

if (!file.exists(abundanceChangeTable)){
  source('~/projects/NWT/posthocFunctions/makeAbundanceTable.R')
  
  folderAbundance <- "~/projects/NWT/outputs/SIMULATIONS"
  abund2011 <- makeAbundanceTable(folderAbundance = folderAbundance, 
                                  folderColonization = folderColonization, 
                                  year = 2011)
  names(abund2011) <- c("species", "year", "totalAbundance2011")
  # 1.2. abund2100 -> get the abundance in 2100, use the colonization 2100 cutoff, sum all
  folderAbundance <- "~/projects/NWT/outputs/SIMULATIONS/LandR.CS_fS"
  abund2100 <- makeAbundanceTable(folderAbundance = folderAbundance, 
                                  folderColonization = folderColonization, 
                                  year = 2100)
  names(abund2100) <- c("species", "year", "totalAbundance2100")
  
  # 1.3. propChangeAbund -> (abund2100 - abund2011)/abund2011
  proportionAbundanceChangeTable <- merge(abund2011[, c("species", "totalAbundance2011")],
                                          abund2100[, c("species", "totalAbundance2100")])
  proportionAbundanceChangeTable[, proportionalChange := (totalAbundance2100 - 
                                                            totalAbundance2011)/totalAbundance2011]
  qs::qsave(proportionAbundanceChangeTable, abundanceChangeTable)
} else {
  proportionAbundanceChangeTable <- qs::qread(abundanceChangeTable)
}

# Make another plot (p5) to add to birdsPlot

proportionAbundanceChangeTable[, species := factor(species, levels = newSortOrder)]
proportionAbundanceChangeTable[, population := ifelse(proportionalChange > 0, 
                                                      "increase", 
                                                      "decrease")]
proportionAbundanceChangeTable[, proportionalChangeFixed := 
                                 ifelse(proportionalChange > 5, 5, 
                                        proportionalChange)]
# Adding label mark
lapply(X = proportionAbundanceChangeTable$species, function(sp){
  DT <- proportionAbundanceChangeTable[species == sp,]
  signal <- ifelse(unique(DT[["proportionalChangeFixed"]]) > 0, "> 0", "< 0")
  jit <- ifelse(unique(DT[["proportionalChangeFixed"]]) > 0, 0.2, -0.2)
  S <-  sum(DT[eval(parse(text = paste0("proportionalChangeFixed", 
                                        signal))), 
               proportionalChangeFixed])
  pos <- S + jit
  proportionAbundanceChangeTable[species == sp, labelMark := pos]
  return("OK")
})

p5 <- ggplot(data = proportionAbundanceChangeTable, 
             mapping = aes(x = proportionalChangeFixed, y = species,
                           fill = population, group = population,
                           color = population)) +
  geom_col() +
  geom_vline(xintercept = 0, color = "black") + 
  xlab("Expected change in boreal songbird abundance from \n2011 to 2100 with climate change") +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        legend.title = element_blank()) + #,
  # plot.margin = unit(c(5.5,5.5,5.5,0), "pt")) +
  scale_color_manual(values = c("increase" = "turquoise3", 
                                "decrease" = "tomato3")) + 
  scale_fill_manual(values = c("increase" = "turquoise1", 
                               "decrease" = "tomato1")) +
  geom_text(aes(x = labelMark, 
                label = round(proportionalChange, 2)), 
            size = 2.5, color = "grey10", check_overlap = TRUE) +
  scale_x_continuous(breaks = seq(-1, 5, by = 0.5)) +
  scale_y_discrete(position = "right")
p5

ggsave(device = "png", filename = file.path(folderColonization, 
                                            "proportionalAbundanceChange.png"), 
       width = 8, height = 11)


#################

############################################# ALL PLOTS TOGETHER!
############################################# 

# Put all three plots together in one big landscape plot

birdPlots <- gridExtra::grid.arrange(p1, p2, p4, ncol = 3)

ggsave(file.path(folderColonization, "birdPlotWithAbundance.png"), device = "png",
       plot = birdPlots, width = 20, height = 9)

######

############################################# PLOT 6
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

#################

############################################# MAPS -- for each bird -- Not Appendix
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
#####

############################################# MAPS 1 -- APPENDIX
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
#####################

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
#####################

############################################### MAP3 -- Paper 2
##### PROBABILITY OF PRESENCE STRALBERG COLOR SCHEME ######

# LAST 2 THINGS TO DO:
# 1. Make the probability of presence maps
# 1.1. Get the colonization 
folderPath <- "~/projects/NWT/outputs/posthoc/colonization"
fileName <- "probabilityPresence_2100_LandR.CS_fS_V6a_"
uploadFolder <- "1f8kqdiTTOtJfJIteFEcFUSaU8FQedCPR"
library(googledrive)
library(rasterVis)
AT <- seq(0, 1, by = 0.2)
pal <- colorRampPalette(c("khaki1", "greenyellow",
                          "green3", "mediumturquoise", "blue"), 
                        space = "Lab", 
                        bias = 0.5)
# Following Stralberg et al., 2015 color scheme`
# palFun <- colorRampPalette(c("#FFFACD", "lemonchiffon","#FFF68F", 
#                           "khaki1","#ADFF2F", "greenyellow", "#00CD00", 
#                           "green3", "#48D1CC", "mediumturquoise", 
#                           "#007FFF", "blue"), 
#                         space = "Lab", bias = 0.5) #Didn't quite work...


# source('~/projects/NWT/temp/checkColorPalette.R')
# checkColorPalette(pal)
# 
probabilityOfPresenceMaps <- lapply(Species, function(sp){
  speciesProbabilityPath <- file.path(folderPath, "probabilityPresenceMapsAPPENDIX", paste0("probabilityPresence_2100_", sp, ".png"))
  ras <- raster::raster(file.path(folderPath, paste0(fileName,sp,".tif")))
  png(filename = speciesProbabilityPath,
      width = 21, height = 29,
      units = "cm", res = 300)
  print(levelplot(ras,
                  sub = paste0("Probability of presence of ", sp," in 2100"),
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
  drive_upload(speciesProbabilityPath, as_id(uploadFolder))
  return(speciesProbabilityPath)
})



#######################

############################################### MAPX -- Paper 2
##### PROBABILITY OF PRESENCE  #######
runName <- "NWT_BCR6"
originalDateAnalysis <- "SIMULATIONS"
source("1_generalSetup.R")
source("2_generatingInputs.R")
stepCacheTag <- c(paste0("cache:6_posthocAnalysis"), 
                  paste0("runName:", runName))

SpaDES.core::setPaths(cachePath = posthocCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       "posthoc"),
                                             create = TRUE))
library(raster)
library(tictoc)
library(data.table)
library(reproducible)
noCorner <- raster("~/projects/NWT/inputs/NWT_BCR6/RTM_noCorner.tif")
Species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO",
             "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW",
             "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP",
             "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL",
             "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI",
             "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP",
             "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP",
             "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA")

vegetationFireModels <- expand.grid(vegetation = c("LandR_", "LandR.CS_"), 
                                    fire = c("fS", "SCFM"))
vegetationFireModels <- paste0(vegetationFireModels$vegetation, vegetationFireModels$fire)


dataFolder <- lapply(vegetationFireModels, function(scenario){
  dataFolderRuns <- lapply(c("V4", "V6a"), function(birdModel){
    dataFolderRuns <- lapply(paste0("run", 1:10), function(run){
      pth <- file.path(dirname(Paths$outputPath),
                       originalDateAnalysis,
                       scenario,
                       run,
                       paste0("birdPredictions", birdModel))
      return(pth)
    })
    names(dataFolderRuns) <- paste0("run", 1:10)
    return(dataFolderRuns)
  })
  names(dataFolderRuns) <- c("V4", "V6a")
  return(dataFolderRuns)
})
names(dataFolder) <- vegetationFireModels

source("modules/posthocBirdsNWT/R/retrieveRasters.R")
listOfRasters <- retrieveRasters(dataFolder = dataFolder,
                                 years = 2100,
                                 patternsToRetrieveRasters = c("predicted", ".tif"),
                                 species = Species)

flammableRTMPath <- file.path(Paths$inputPath, "flammableRTM")
source('~/projects/NWT/temp/makeProbabilityPresenceRaster.R')
flammableRTM <- raster::raster(paste0(flammableRTMPath, ".tif"))
colRas2100 <- makeProbabilityPresenceRaster(species = Species,
                                            yearOfAnalysis = 2100,
                                            flammableRTM = flammableRTM,
                                            listOfRasters = listOfRasters,
                                            noCorner = noCorner,  # <~~~~~~~~ TEMPORARY!!!
                                            outputFolder = "~/projects/NWT/outputs/posthoc/colonization",
                                            useFuture = TRUE)
########

############################################### MAPX -- Paper 2
##### DENSITY CURRENT / FUTURE #############
runName <- "NWT_BCR6"
originalDateAnalysis <- "SIMULATIONS"
source("1_generalSetup.R")
source("2_generatingInputs.R")
stepCacheTag <- c(paste0("cache:6_posthocAnalysis"), 
                  paste0("runName:", runName))

SpaDES.core::setPaths(cachePath = posthocCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       "posthoc"),
                                             create = TRUE))
library(raster)
library(tictoc)
library(data.table)
library(reproducible)

Species <- c("CAWA", "BLPW", "OSFL")

source('~/projects/NWT/posthocFunctions/makeProbabilityPresenceRaster.R')
source('~/projects/NWT/posthocFunctions/calcColonization.R')
source("modules/posthocBirdsNWT/R/retrieveRasters.R")

uploadFolder <- "1f8kqdiTTOtJfJIteFEcFUSaU8FQedCPR"
library(googledrive)
library(rasterVis)

vegetationFireModels <- "LandR.CS_fS"
dataFolder <- lapply(vegetationFireModels, function(scenario){
  dataFolderRuns <- lapply("V6a", function(birdModel){
    dataFolderRuns <- lapply(paste0("run", 1:10), function(run){
      pth <- file.path(dirname(Paths$outputPath),
                       originalDateAnalysis,
                       scenario,
                       run,
                       paste0("birdPredictions", birdModel))
      return(pth)
    })
    names(dataFolderRuns) <- paste0("run", 1:10)
    return(dataFolderRuns)
  })
  names(dataFolderRuns) <- "V6a"
  return(dataFolderRuns)
})
names(dataFolder) <- vegetationFireModels

yearsToGenerateDensityMaps <- c(2011, 2100)
mps <- lapply(yearsToGenerateDensityMaps, function(ys){
  listOfRasters <- retrieveRasters(dataFolder = dataFolder,
                                   years = ys,
                                   patternsToRetrieveRasters = c("predicted", ".tif"),
                                   species = Species)
  
  densityRasters <- makeProbabilityPresenceRaster(species = Species,
                                              yearOfAnalysis = ys,
                                              flammableRTM = flammableRTM,
                                              listOfRasters = listOfRasters,
                                              noCorner = noCorner,  # <~~~~~~~~ TEMPORARY!!!
                                              outputFolder = "~/projects/NWT/outputs/posthoc/colonization",
                                              useFuture = FALSE,
                                              typeOfAnalysis = "density")
  return(densityRasters)
})
names(mps) <- paste0("Year", yearsToGenerateDensityMaps)
# Make the delta maps.  Just need to select these from mps
deltaMaps <- lapply(Species, function(sp){
  message(paste0("Making delta maps for ", sp))
  rasT0 <- mps[[paste0("Year", yearsToGenerateDensityMaps[1])]][[sp]][["LandR.CS_fS"]][["V6a"]][["ras"]]
  rasT1 <- mps[[paste0("Year", yearsToGenerateDensityMaps[length(yearsToGenerateDensityMaps)])]][[sp]][["LandR.CS_fS"]][["V6a"]][["ras"]]
  delta <- rasT1-rasT0
  library(viridis)
  pal <- pals::brewer.rdylbu(100)
  deltaMapPath <- file.path("~/projects/NWT/outputs/posthoc/deltaMaps", paste0("deltaMap_", sp, 
                                                    ".png"))
  # Center the data
  delta[] <- round(delta[], 5)
  deltaDT <- na.omit(data.table::data.table(pixelID = 1:ncell(delta), 
                                                   val = getValues(delta)))
  setkey(deltaDT, val)
  # NEGATIVE
  nD <- deltaDT[val < 0,]
  nD[, CUM := cumsum(val)]
  nD[, CUMstd := CUM/sum(val)]
  nD[, PA := CUMstd > 0.05]
  nD[PA == FALSE, PA0 := "low"]
  delta[nD[PA0 == "low", pixelID]] <- nD[PA0 == "low", mean(val)]
  
  # POSITIVE
  pD <- deltaDT[val > 0,]
  pD[, CUM := cumsum(val)]
  pD[, CUMstd := CUM/sum(val)]
  pD[, PA := CUMstd < 0.95]
  pD[PA == FALSE, PA0 := "high"]
  delta[pD[PA0 == "high", pixelID]] <- pD[PA0 == "high", mean(val)]
  
  library(plyr)
  
  negSeq <- seq(round_any(minValue(delta), 0.001, f = floor), 0, length.out = 5)
  posSeq <- seq(0, round_any(maxValue(delta), 0.001, f = ceiling), length.out = 5)
  AT <- round(c(negSeq[-length(negSeq)], 0, posSeq[-1]), 
              digits = 3)
  ATlabel <- AT
  ATlabel <- round(ATlabel, 2)
  colKey <- list(at = ATlabel, ## where the colors change
                 labels = list(
                   labels = c(paste0("< ", ATlabel[1]),
                              ATlabel[-c(1, length(ATlabel))], 
                              paste0("> ", ATlabel[length(ATlabel)])), ## labels
                   at = ATlabel ## where to print labels
                 ),
                 space = 'bottom',
                 axis.line = list(col = 'black'),
                 width = 0.75,
                 height = 1.3)
    png(filename = deltaMapPath,
        width = 29, height = 29,
        units = "cm", res = 300)
    print(levelplot(delta,
                    sub = paste0("Difference in density from 2011 to 2100 for ", sp),
                    margin = FALSE,
                    maxpixels = 6e6,
                    at = ATlabel,
                    colorkey = colKey,
                    par.settings = list(
                      strip.border = list(col = 'transparent'),
                      strip.background = list(col = 'transparent'),
                      axis.line = list(col = 'transparent')),
                    scales = list(draw = FALSE),
                    col.regions = pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
                    par.strip.text = list(cex = 0.8,
                                          lines = 1,
                                          col = "black")))
    dev.off()
    return(deltaMapPath)
})

########

############################################### MAPX -- Paper 2
##### PROBABILITY OF MOVEMENT  #######

# MAKE ANOTHER FUNCTION TO USE THIS BELOW WITHIN EACH ONE OF THE SPECIES/SCENARIOS I NEED
# speciesForPlot <- c("EAPH", "PISI", "BBWA", "OSFL", "ATSP", "WCSP")
source('~/projects/NWT/posthocFunctions/probabilityOfColonizationWithPresence.R')
speciesForPlot <- c("CAWA", "BLPW")
scenarioForPlot <- "LandR.CS_fS_V6a"

allMaps <- lapply(speciesForPlot, function(sp){
  allScenarios <- lapply(scenarioForPlot, function(scen){
    spScen <- probabilityOfColonizationWithPresence(species = sp,
                                                    flammableRTM = flammableRTM,
                                                    scenarioName = scen,
                                                    folder = "~/projects/NWT/outputs/posthoc/colonization",
                                                    patternsT0 = c(sp, "probabilityPresence_", "2011"),
                                                    patternsT1 = c(sp, scen, "probabilityPresence_", "2100"))
  })
  names(allScenarios) <- scenarioForPlot
  return(allScenarios)
})
names(allMaps) <- speciesForPlot

# to Upload
folderHosting <- "~/projects/NWT/outputs/posthoc/colonization"
folderToUpload <- "1t56m8O3EGfyeJIOrSQwg1bp8EzM7vgsT"

fl <- list.files(folderHosting, pattern = "probabilityMovement2100", full.names = TRUE)
fl <- c(fl, unlist(allMaps, use.names = FALSE))
library(googledrive)
lapply(fl, drive_upload, as_id(folderToUpload))

#####

############################################### PLOTX -- Paper 1
##### VEGETATION PLOTS #############
source('~/projects/NWT/posthocFunctions/vegetationBiomassPlot.R')
posthocFolder <- dirname(folderColonization)
pal <- c("#27408B", "#8B7D6B", "#CD0000", "#EEB422", "#9A32CD", "#006400",
         "#A6BFFF", "#F1E3D1", "#FF7F7F", "#FFDC66", "#FFB1FF", "#7FE37F")

vegPlotsLandRCS_SCFM <- vegetationBiomassPlot(pathData = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS",
                                              # pathOutputs = reproducible::checkPath(
                                              #   path = "~/projects/NWT/outputs/posthoc/vegetationResults", 
                                              #   create = TRUE),
                                              pathOutputs = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults",
                                              typeSim = c("LandR.CS_fS", "LandR.CS_SCFM", "LandR_fS", "LandR_SCFM"),
                                              years = c(2011, 2100),
                                              pal = pal, # Order: all species first, mixed with leading after
                                              flammableRTM = flammableRTM)



# For biomass change in function of climate only
# totBiom_LandR_fS <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR_fS_2100.tif")
# totBiom_LandR.CS_fS <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR.CS_fS_2100.tif")
# totBiom_LandR_SCFM <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR_SCFM_2100.tif")
# totBiom_LandR.CS_SCFM <- raster::raster("~/projects/NWT/outputs/posthoc/vegetationResults/totalBiomass_LandR.CS_SCFM_2100.tif")

totBiom_LandR_fS <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_fS_2100.tif")
totBiom_LandR.CS_fS <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_fS_2100.tif")
totBiom_LandR_SCFM <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR_SCFM_2100.tif")
totBiom_LandR.CS_SCFM <- raster::raster("/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults/totalBiomass_LandR.CS_SCFM_2100.tif")

diffRas <- totBiom_LandR.CS_fS-totBiom_LandR_fS # Vegetation
diffRas2 <- totBiom_LandR.CS_fS-totBiom_LandR_SCFM # Fire and Vegetation
diffRas3 <- totBiom_LandR.CS_fS-totBiom_LandR.CS_SCFM # Fire

# Make a plot of the number of pixel by the values in change of total biomass
directEffect <- diffRas
indirectEffect <- diffRas3
fullEffect <- diffRas2

library(plyr)

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

library("ggplot2")
pVeg <- ggplot(data = unique(plotDT[val != 0,]), aes(x = val, 
                                                     y = Count*6.25,
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
        legend.title = element_blank()) +
  xlim(-5000, 5000) +
  xlab("Change in total biomass due to climate effects") +
  ylab("Area (ha)")

ggsave(plot = pVeg, device = "png", filename = file.path(posthocFolder, "vegetationResults", 
                                                         "biomassAffectedByClimateChange.png"), 
       width = 8, height = 11)
# library(viridis)
# pal <- pals::brewer.rdylbu(100)
# totalBiomassPath <- file.path("~/projects/NWT/outputs/posthoc/vegetationResults", paste0("totalBiomass_CCvsnoCC2.png"))
# diffRas2[is.na(flammableRTM)] <- NA
#   png(filename = totalBiomassPath,
#       width = 21, height = 29,
#       units = "cm", res = 300)
#   print(levelplot(diffRas2,
#                   sub = paste0("Difference in biomass in 2100 due to ",
#                                "climate effect on vegetation succession and fire"),
#                   margin = FALSE,
#                   maxpixels = 6e6,
#                   colorkey = list(
#                     space = 'bottom',
#                     axis.line = list(col = 'black'),
#                     width = 0.75
#                   ),
#                   par.settings = list(
#                     strip.border = list(col = 'transparent'),
#                     strip.background = list(col = 'transparent'),
#                     axis.line = list(col = 'transparent')),
#                   scales = list(draw = FALSE),
#                   col.regions = pal, #pals::kovesi.rainbow(nlev), #viridis_pal(option = "D")(nlev),
#                   par.strip.text = list(cex = 0.8,
#                                         lines = 1,
#                                         col = "black")))
#   dev.off()
#####

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
####################

library("tictoc")
tic("Total time elapsed for biomass plots: ")
source('~/projects/NWT/posthocFunctions/biomassPlots.R')
pl <- biomassPlots(years = c(2011, 2100),
                   pathData = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS/",
                   pathOutputs = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults",
                   Scenarios = c("LandR_SCFM", "LandR.CS_SCFM", 
                                 "LandR_fS", "LandR.CS_fS"),
                   runs = paste0("run", 1:10),
                   flammableRTM = rasterToMatch)
toc()

tic("Total time elapsed for biomass plots: ")
source('~/projects/NWT/posthocFunctions/leadingSpPlots.R')
pl2 <- leadingSpPlots(leadingPercentage = 0.8,
                      years = c(2011, 2100),
                      pathData = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS/",
                      pathOutputs = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults",
                      Scenarios = c("LandR_SCFM", "LandR.CS_SCFM", 
                                    "LandR_fS", "LandR.CS_fS"),
                      runs = paste0("run", 1:10),
                      rasterToMatch = rasterToMatch,
                      flammableRTM = flammableRTM)
toc()

# Proportions ############### for the paper

source('~/projects/NWT/posthocFunctions/biomassPlotDenominator.R')
pl <- biomassPlotDenominator(year = 2011,
                   pathData = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/SIMULATIONS",
                   pathOutputs = "/mnt/SpaDES/data/Micheletti/NWT_Outputs/PAPER_EffectsOfClimateChange/posthoc/vegetationResults",
                   Scenarios = c("LandR_SCFM", "LandR.CS_SCFM",
                                 "LandR_fS", "LandR.CS_fS"),
                   runs = paste0("run", 1:10),
                   flammableRTM = rasterToMatch)

pathways <- c("fire", "vegetation", "netEffect")
percChangesPathways <- rbindlist(lapply(pathways, function(eff){
  EffectRas <- raster(paste0("/mnt/SpaDES/data/Micheletti/NWT_Outputs/",
                             "PAPER_EffectsOfClimateChange/posthoc/vegetationResults/",
                             "vegetationPlots/difference_", eff,"_Biomass.tif"))

  # for each one of the 3 effects and divide by the 
  EffRasDenom <- raster(paste0("/mnt/SpaDES/data/Micheletti/NWT_Outputs/",
                               "PAPER_EffectsOfClimateChange/posthoc/",
                               "vegetationResults/vegetationPlots/",
                               "difference_", eff,"_BiomassDenominator.tif"))
  # And take the mean for the % of change in function of 2011
  # Q <- quantile(EffectRas[], probs=c(.05, .95), na.rm = FALSE)
  # iqr <- IQR(EffectRas[])  
  #   up <-  Q[2]+1.5*iqr # Upper Range  
  #   low<- Q[1]-1.5*iqr # Lower Range
  # EffectNoOuts <- subset(EffectRas[], EffectRas[] > low & EffectRas[] < up)
  meanEff <- mean(EffectRas[], na.rm = TRUE)
  meanDenom <- mean(EffRasDenom[], na.rm = TRUE)
  averagePropChange <- 100*(meanEff/meanDenom)
  return(data.table(effect = eff,
                    meanEffectDifference = meanEff,
                    meanInitialBiomass = meanDenom,
                    changeInPerc = averagePropChange))
}))
percChangesPathways

############### FIRE
############### 
# Fire summaries
source('~/projects/NWT/posthocFunctions/plotAreaBurnReps.R')
# outPath <- Paths$outputPath
library(ggplot2)
library(gridExtra)
outPath <-"~/projects/NWT/outputs/SIMULATIONS"
burnPlot <- plotAreaBurnReps(dataPath = outPath, 
                                         typeSim = c("LandR.CS_fS", "LandR_SCFM"),
                                         lastYear = 2100, 
                                      overwrite = TRUE)
