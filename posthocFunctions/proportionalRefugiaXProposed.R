# Check the proportional loss of habitat within BCR6 within NWT 

# All results in: 
resultsHostedFolder <- "https://drive.google.com/drive/folders/1PipfXMz0GAB7nwOq_CRIn_sF259MzUvw"
outFolder <- file.path(getwd(), "outputs/posthoc/refugia")

runName <- "NWT_BCR6"
source("1_generalSetup.R")
source("2_generatingInputs.R")
noCorner <- raster::raster(file.path(getwd(), "inputs/NWT_BCR6/RTM_noCorner.tif"))
flammableRTM[is.na(noCorner)] <- NA

setPaths(outputPath = outFolder)

library(data.table)
library(reproducible)

# 1. Use the table in the paper to figure out which lag time is most appropriate for which species
ourResults <- prepInputs(url = "https://drive.google.com/file/d/1sNWmBXIzCkBKoHq_eYIiVgheQ5UD7Vav/view?usp=sharing",
                         targetFile = "proportionsTableSUPMAT.csv",
                         destinationPath = Paths$outputPath,
                         fun = "data.table::fread")

Species <- prepInputs(url = "https://drive.google.com/file/d/1vfI_diZRiAllVE8juuxaMDOsGlLKas4i/view?usp=sharing",
                      targetFile = "allSpeciesNames.csv",
                      destinationPath = Paths$outputPath,
                      fun = "data.table::fread")
names(Species) <- c("species", "commonName")

# add missing species
Species <- rbind(Species, data.table(species = c("NOFL", 
                                                 "GRAJ"), 
                                     commonName = c("Northern Flicker (Colaptes auratus)",
                                                    "Gray Jay (Perisoreus canadensis)")))

stralberg <- prepInputs(url = "https://drive.google.com/file/d/1Q2otUCK4TQbBe40zCA0AT6etR1z2frSv/view?usp=sharing",
                        targetFile = "StralbergFullTableWithCode.csv",
                        destinationPath = Paths$outputPath,
                        fun = "data.table::fread",
                        userTags = "updatedTable", purge = 7)
stralberg[, Difference := NULL]

# Change names to match folders
names(stralberg)[names(stralberg) == "60yearLag"] <- "60yrlag"
names(stralberg)[names(stralberg) == "30yearLag"] <- "30yrlag"
names(stralberg)[names(stralberg) == "StrictRefugia"] <- "refugia"
names(stralberg)[names(stralberg) == "Unconstrained"] <- "nolag"
names(stralberg)[names(stralberg) == "Species common name (scientific name)"] <- "commonName"

# Identify which scenario belongs to each bird
scen <- names(stralberg)[!names(stralberg) %in% c("commonName",
                                                  "ModifiedRefugia")]
stralberg[, whichScenario := lapply(1:NROW(stralberg), function(rowIndex){
  position <- which(data.frame(stralberg[rowIndex, ..scen]) == as.numeric(stralberg[rowIndex, "ModifiedRefugia"]))
  if (length(position) == 0)
    position <- which(data.frame(stralberg[rowIndex, ..scen]) == abs(as.numeric(stralberg[rowIndex, "ModifiedRefugia"])))
  return(scen[position])
  })]
# Drop common name
stralberg[, commonName := NULL]

fullOurResults <- merge(ourResults, Species, on = "species")
toKeep <- c("species", "proportionOfAreaChanged")
fullResults <- fullOurResults[, ..toKeep]
toKeep <- c("code", "whichScenario")
stralberg <- stralberg[, ..toKeep]
names(stralberg) <- c("species", "whichScenario")

fullResults <- fullResults[stralberg, on = "species"]
# Remove missing species
fullResults <- na.omit(fullResults)

# From Stralberg 11NOV20
# current: current core area (average across models)
# refugia: overlap between current and future core area (i.e., 30-90 yr time lag depending on the time period)
# nolag: future core area , no lag considered (Unconstrained)
# 30yrlag: projections assuming 30-yr lag (30yearLag)
# 60yrlag: projections assuming 60-yr lag (60yearLag)

# Remove species that had StrictRefugia
# stralberg <- stralberg[whichScenario != "StrictRefugia", ]

library("googledrive")

source('~/projects/NWT/posthocFunctions/getCoreAreas.R')
getCoreAreas(fullResults, flammableRTM)
# Doesn't need to be assigned as it modifies fullResults

# Calculate proportion change
source('~/projects/NWT/posthocFunctions/calculateProportionLossRefugia.R')
refugiaDT <- rbindlist(lapply(fullResults$species, function(sp){
  p <- calculateProportionLossRefugia(species = sp,
                                      RTM = flammableRTM,
                                      currentDistribution = raster::raster(as.character(fullResults[species == sp, currentCore])), 
                                      futureDistribution = raster::raster(as.character(fullResults[species == sp, futureCore])))
  return(data.table::data.table(species = sp,
                                proportionOfAreaChangedRefugia = p))
}))

finalRefugiaTable <- merge(fullResults, refugiaDT)
finalRefugiaTable <- finalRefugiaTable[, c("species", "proportionOfAreaChangedRefugia", "proportionOfAreaChanged")]
stralberg <- prepInputs(url = "https://drive.google.com/file/d/1Q2otUCK4TQbBe40zCA0AT6etR1z2frSv/view?usp=sharing",
                        targetFile = "StralbergFullTableWithCode.csv",
                        destinationPath = Paths$outputPath,
                        fun = "data.table::fread",
                        userTags = "updatedTable", purge = 7)
stralberg[, Difference := NULL]

stralberg <- stralberg[, c("code", "ModifiedRefugia")]
names(stralberg) <- c("species", "borealProportionAreaChange")
finalRefugiaTable <- finalRefugiaTable[stralberg, on = "species"]
finalRefugiaTable <- na.omit(finalRefugiaTable)
finalRefugiaTable[, c("proportionOfAreaChangedRefugia", "proportionOfAreaChanged") := list(round(proportionOfAreaChangedRefugia, 2),
                                                                                           round(proportionOfAreaChanged, 2))]
finalRefugiaTablePlot <- melt(finalRefugiaTable, measure.vars = c("proportionOfAreaChangedRefugia", 
                                                                  "proportionOfAreaChanged", 
                                                                  "borealProportionAreaChange"),
                              variable.name = "scenario", value.name = "val")
finalRefugiaTablePlot[val > 5, val := 5]
library("ggplot2")
DT <- finalRefugiaTablePlot[scenario != "borealProportionAreaChange",]
plotRefugia <- ggplot(data = DT, aes(x = species,
                                     y = val,
                                     group = scenario,
                                     color = scenario,
                                     fill = scenario)) +
  geom_bar(stat="identity", width=.7, position = "dodge", aes(fill = scenario)) + 
  theme_linedraw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  scale_color_manual(values = c("proportionOfAreaChangedRefugia" = "midnightblue", #midnightblue
                                # "borealProportionAreaChange" = "dodgerblue3",
                                "proportionOfAreaChanged" = "mediumslateblue"),
                     labels = c("proportionOfAreaChangedRefugia" = "Modified refugia",
                                # "borealProportionAreaChange" = "Modified refugia boreal wide",
                                "proportionOfAreaChanged" = "Simulation")) +
  scale_fill_manual(values = c("proportionOfAreaChangedRefugia" = "midnightblue", #midnightblue
                               # "borealProportionAreaChange" = "dodgerblue2",
                               "proportionOfAreaChanged" = "mediumslateblue"), 
                    labels = c("proportionOfAreaChangedRefugia" = "Modified refugia",
                               # "borealProportionAreaChange" = "Modified refugia boreal wide",
                               "proportionOfAreaChanged" = "Simulation")) +
  ylab("Proportional change in core habitat at the end of the century") +
  ylim(c(-1, 5)) +
  scale_y_continuous(breaks = round(seq(min(DT$val), max(DT$val), by = 0.2), 1))
  
plotRefugia
ggsave(plotRefugia, device = "png", filename = file.path(outFolder, "proportionOfAreaChanged.png"), 
       width = 17, height = 8)  
# Modified refugia: expected vegetation to be unsuitable (did they change tree age with time? -- No, they just forecasted in each climate future and union of the current and future) where climate is suitable?
# Ignores other possible combinations of seral-stage and climate that might work for birds -- maybe not ideal, but would work?

