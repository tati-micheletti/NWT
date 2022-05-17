# Make LandR and fire posthocs!

runName <- "NWT_BCR6"
originalDateAnalysis <- "SIMULATIONS"

times <- list(start = 1, end = 1)

parameters <- list(
  posthocLandR = list(
    "years" = c(seq(2011, 2091, by = 10), 2100),
    "saveRAS" = TRUE,
    "plotFireStats" = TRUE,
    "sppEquivCol" = runName
  )
)
modules <- list("posthocLandR")
source("1_generalSetup.R")

posthocCache <- checkPath(file.path(generalCacheFolder, "posthoc", runName), create = TRUE)
SpaDES.core::setPaths(outputPath = file.path(getwd(), "outputs", originalDateAnalysis),
                      cachePath = posthocCache)

###################################################
# Prepare sppEquivalencies_CA and sppColorVect
###################################################

data("sppEquivalencies_CA", package = "LandR")
sppEquivCol <- runName

# Make NWT spp equivalencies
sppEquivalencies_CA[, paste0(runName) := c(Betu_Pap = "Betu_Pap", 
                                           Lari_Lar = "Lari_Lar", 
                                           Pice_Gla = "Pice_Gla",
                                           Pice_Mar = "Pice_Mar", 
                                           Pinu_Ban = "Pinu_Ban", 
                                           Popu_Tre = "Popu_Tre")[Boreal]]

sppEquivalencies_CA <- sppEquivalencies_CA[!is.na(get(runName))]
sppEquivalencies_CA$EN_generic_short <- sppEquivalencies_CA[[paste0(runName)]]
sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, 
                                 sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

###################################################

objects <- list(
  resultsFolders = list(
    # LandR.CS_fS_run5 = file.path(Paths$outputPath, "LandR.CS_fS/run5"),
    # LandR.CS_fS_run10 = file.path(Paths$outputPath, "LandR.CS_fS/run10"),
    # LandR_SCFM_run5 = file.path(Paths$outputPath, "LandR_SCFM/run5"),
    # LandR_SCFM_run10 = file.path(Paths$outputPath, "LandR_SCFM/run10"),
    # LandR_fS_run5 = file.path(Paths$outputPath, "LandR_fS/run5"),
    # LandR_fS_run10 = file.path(Paths$outputPath, "LandR_fS/run10"),
    LandR.CS_SCFM_run5 = file.path(Paths$outputPath, "LandR.CS_SCFM/run5"),
    LandR.CS_SCFM_run10 = file.path(Paths$outputPath, "LandR.CS_SCFM/run10")
  ),
  sppColorVect = sppColorVect,
  sppEquivalencies_CA = sppEquivalencies_CA
)
inputs <- list()
outputs <- list()

posthocResultsLandR <- simInitAndSpades(times = times, 
                                        params = parameters, 
                                        modules = modules,
                                        objects = objects, debug = 1)
