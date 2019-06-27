# SCFM+LandR
SCFM_LandR <- readRDS("/mnt/data/Micheletti/NWT/outputs/08JUN19_noCS/NWT_noCS_09JUN19")

# fireSense+LandR.CS
fS_LandR.CS <- readRDS("/mnt/data/Micheletti/NWT/outputs/08JUN19/NWT_CS_09JUN19")

# SCFM+LandR.CS
SCFM_LandR.CS <- readRDS("/mnt/data/Micheletti/NWT/outputs/11JUN19_CS_SCFM/NWT_CS_SCFM13JUN19")

# fS+LandR
fS_LandR <- readRDS("/mnt/data/Micheletti/NWT/outputs/11JUN19_noCS_fS/NWT_noCS_fS12JUN19")


#########################
#########################

# This is the cummulative burn
flammTable <- function(flammMap, burnSummary, scenario){
  tblFlam <- table(flammMap[])[2]
  totalFRammableHa <- tblFlam * 6.25
  burnedAnnualReg <- burnSummary[, .(burnArea = sum(areaBurned/totalFRammableHa)), by = "year"]
  burnedAnnualReg[, cummulative := cumsum(burnedAnnualReg[, burnArea])]
  burnedAnnualReg$scenario <- scenario
  return(burnedAnnualReg)
}

SCFM_LandR_burn <- flammTable(flammMap = SCFM_LandR$flammableMap, burnSummary = SCFM_LandR$burnSummary, scenario = "SCFM_LandR")
SCFM_LandR.CS_burn <- flammTable(flammMap = SCFM_LandR.CS$flammableMap, burnSummary = SCFM_LandR.CS$burnSummary, scenario = "SCFM_LandR.CS")
fS_LandR_burn <- flammTable(flammMap = fS_LandR$flammableMap, burnSummary = fS_LandR$burnSummary, scenario = "fS_LandR")
fS_LandR.CS_burn <- flammTable(flammMap = fS_LandR.CS$flammableMap, burnSummary = fS_LandR.CS$burnSummary, scenario = "fS_LandR.CS")

burnedArea <- rbind(SCFM_LandR_burn, SCFM_LandR.CS_burn, fS_LandR_burn, fS_LandR.CS_burn)

library("ggplot2")
p <- ggplot(burnedArea, aes(x = year, y = cummulative, colour = scenario)) +
  geom_line()
p


############################
############################

# Checking the Climate 

variationInClimate <- function(rasPath, flammableMap){
  library("raster")
 ras <- raster::raster(rasPath)
 crs(ras) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +units=m +no_defs +datum=WGS84"
 rasPostProcessed <- postProcess(x = ras, rasterToMatch = flammableMap, maskWithRTM = TRUE)
 medianClim <- median(rasPostProcessed[flammableMap == 1], na.rm = TRUE)
 return(medianClim)
}

# PPT
env <- environment()
lapply(c(2025, 2055, 2085), function(year){
  fSFLamm <- fS_LandR.CS$flammableMap # fS_LandR$flammableMap
  dataDir <- paste0("/mnt/data/Micheletti/NWT/modules/climate_NWT_DataPrep/data/", "CanESM2_rcp45_", year,"_Monthly_ASCII/")
  assign(paste0("PPT", year), value = variationInPPT(rasPath = file.path(dataDir, paste0("CanESM2_rcp45_", year, "_PPT06.asc")), flammableMap = fSFLamm), envir = env)
})

# 
env <- environment()
lapply(c(2025, 2055, 2085), function(year){
  fSFLamm <- fS_LandR.CS$flammableMap # fS_LandR$flammableMap
  dataDir <- paste0("/mnt/data/Micheletti/NWT/modules/climate_NWT_DataPrep/data/", "CanESM2_rcp45_", year,"_Monthly_ASCII/")
  assign(paste0("Tave", year), value = variationInClimate(rasPath = file.path(dataDir, paste0("CanESM2_rcp45_", year, "_Tave06.asc")), flammableMap = fSFLamm), envir = env)
})


