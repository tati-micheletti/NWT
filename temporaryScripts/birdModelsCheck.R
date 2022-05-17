
species <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
  "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
  "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
  "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
  "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
  "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
  "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
  "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA")

speciesOfInterest <- c("NOWA", "FOSP", "HOLA", "OSFL", "PISI", "SWSP", "TEWA", "WEWP", 
             "WWCR", "ATSP", "BOCH", "COYE", "CAWA", "REVI", "WCSP")

library("gbm")
library("data.table")
library("usefulFuns")
speciesExample <- c("COYE", "CAWA", "REVI", "WCSP")
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

Species <- grepMulti(x = covs, patterns = "Species|Structure")
Topo <- c("dev25", "vrug", "wet", "wat", "led25")
Climate <- covs[!covs %in% c(Species, Topo)]

allMods[, group := ifelse(var %in% Species, "vegetation", ifelse(var %in% Topo, "topographic", "climate"))]
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

