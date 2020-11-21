LandR.CS <- c("Biomass_borealDataPrep",
              "PSP_Clean", # New parameters
              "Biomass_speciesParameters", # New parameters
              "Biomass_regeneration",
              "Biomass_core",
              # CS
              "gmcsDataPrep")

fS <- c("fireSense_dataPrep",
        "fireSense_SpreadFit", # Being fitted separately
        "fireSense_IgnitionPredict",
        "fireSense_EscapePredict",
        "fireSense_SpreadPredict",
        "LBMR2LCC_DataPrep",
        "fireSense")

others <- c("caribouPopGrowthModel" , "birdsNWT", 
            "caribouRSF", 
            # "caribouCIP",
            "priorityPlaces_DataPrep",
            "priorityPlaces", "posthocBirdsNWT", 
            "posthocLandR")


modules <- c(LandR.CS, fS, others)
library(SpaDES)
library(reproducible)
mm <- na.omit(unique(unlist(lapply(modules, function(m) moduleMetadata(module = m)$inputObjects$objectName))))
names(mm) <- mm
objects <- lapply(mm, function(x) NULL)
sim <- simInit(objects = objects, modules = modules)
saveRDS(sim, file = "forEliot.rds")
