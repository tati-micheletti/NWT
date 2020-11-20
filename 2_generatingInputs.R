############################################
############################################
#   S i m u l a t i o n     S e t u p      #  
############################################
############################################

# Prepares the following layers:
# studyArea: based on the runName provided
# rasterToMatch: based on the runName provided. If no RTM is available, it will rasterize the study 
#                area using LCC05 as a template.
# waterRaster: Layer from DUCKS' Unlimited reprojected to 250m (from original 30m resolution) to the 
#              NWT. This layer has water bodies (1), is more precise than LCC05. Used for caribou.
# wetlandsRaster: Layer from DUCKS' Unlimited reprojected to 250m (from original 30m resolution) to the 
#              NWT. This layer has lowlands/wetlands (2) and is more precise than LCC05. not sure its used
# uplandsRaster: Layer from DUCKS' Unlimited reprojected to 250m (from original 30m resolution) to the 
#              NWT. This layer has uplands (3) and is more precise than LCC05. It is used for birds.
# rstLCC: LCC05 (land cover classes 2005) raster layer. This is a 250m resolution layer.
# flammableRTM: rasterToMatch, where Ice/snow (LCC05 39), Water (LCC05 37:38), rocks (LCC05 33) and
#               urban (LCC05 36) become NA's. It is used for fire.
# studyAreaPSP: studyArea for the PSP data (parameterization for LandR.CS). Fore the NWT, it is 
#               NWT + Alberta.
# ecoRegionRAS: Raster of ecoregions to parameterize both LandR.CS and fire models (i.e. SCFM)

stepCacheTag <- c(paste0("cache:2_simulationSetup"), 
                  paste0("runName:", runName))

studyArea <- prepInputs(url = runNamesList()[RunName == runName, studyArea],
                   destinationPath = Paths$inputPath,
                   filename2 = NULL,
                   userTags = c("objectName:studyArea", stepCacheTag), 
                   omitArgs = c("destinationPath", "filename2"))

rasterToMatch <- Cache(prepInputs, url = runNamesList()[RunName == runName, rasterToMatch],
                       studyArea = studyArea,
                       destinationPath = Paths$inputPath,
                       overwrite = TRUE,
                       userTags = c("objectName:rasterToMatch", stepCacheTag,
                                    "outFun:Cache"),
                       omitArgs = c("overwrite", "destinationPath", "filename2"))

####  Prep Layers: Exclude water, rocks and ice from flammableRTM --> NA
watersRaster <- Cache(prepInputs, url = runNamesList()[RunName == runName, watersRaster],
                      destinationPath = Paths$inputPath,
                      studyArea = studyArea,
                      rasterToMatch = rasterToMatch,
                      filename2 = NULL,
                      # cacheId = "b097c68ef07d6978",
                      userTags = c("objectName:watersRaster", stepCacheTag, "outFun:Cache"),
                      omitArgs = c("destinationPath", "filename2"))
watersVals <- raster::getValues(watersRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA

# Fix missing 1's in Mackenzie River + Slave Lake
rtmVals <- getValues(rasterToMatch)
watersVals[is.na(watersVals) & rtmVals == 1] <- 1
#####
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 1] <- NA
waterRaster <- raster::setValues(x = watersRaster, watersValsToChange)
watersValsToChange <- watersVals
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 2] <- NA
watersValsToChange[watersValsToChange == 2] <- 1
wetlandsRaster <- raster::setValues(x = watersRaster, watersValsToChange)
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 3] <- NA
watersValsToChange[watersValsToChange == 3] <- 1
uplandsRaster <- raster::setValues(x = watersRaster, watersValsToChange)

rstLCC <- Cache(prepInputs, url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
                             "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
                targetFile = file.path(Paths$inputPath, "LCC2005_V1_4a.tif"),
                archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
                destinationPath = Paths$inputPath,
                studyArea = studyArea,
                rasterToMatch = rasterToMatch,
                maskWithRTM = TRUE,
                method = "bilinear",
                datatype = "INT2U",
                filename2 = TRUE,
                userTags = c(stepCacheTag,
                             "objectName:rstLCC", "prepInputsrstLCC_rtm",
                             "outFun:Cache"),
                omitArgs = c("destinationPath", "filename2"))

# ===================================================================
# WHEN MOVING TO 2011 LAYERS: we could do that because all data for climate sensitive stuff anyway
# starts in 2011. Will do this for the WB
# Change parameters in Biomass_boreal* == forestedLCCClasses and check LCCClassesToReplaceNN
# Will also need to check the Ice/Snow/Water classes 

# rstLCC <- LandR::prepInputsLCC(year = 2010, # For when we are ready to swapt to 2011 layers
#                                destinationPath = Paths$inputPath,
#                                studyArea = studyArea,
#                                rasterToMatch = rasterToMatch,
#                                maskWithRTM = TRUE,
#                                method = "bilinear",
#                                datatype = "INT2U",
#                                filename2 = TRUE,
#                                userTags = c(stepCacheTag,
#                                             "objectName:rstLCC", "prepInputsrstLCC_rtm"))
# ===================================================================

# Ice/snow = 39
# Water (LCC05) = 37:38
# Rocks = 33
# Urban = 36

flammableRTMPath <- file.path(Paths$inputPath, "flammableRTM")
if (!file.exists(paste0(flammableRTMPath, ".tif"))){
  nonFlammClass <- c(33, 36:39)
  flammableRTM <- rasterToMatch
  # Remove LCC non flammable classes first
  flammableRTM[rstLCC[] %in% nonFlammClass] <- NA
  # Remove more detailed water from DUCKS layer
  flammableRTM[waterRaster[] == 1] <- NA
  writeRaster(flammableRTM, flammableRTMPath, format = "GTiff")
} else {
  flammableRTM <- raster::raster(paste0(flammableRTMPath, ".tif"))
}

# fire ~~~~~~~~~~~~~~~~~~~~~~~~

fireYears <- 1991:2017
firePolys <- Cache(fireSenseUtils::getFirePolygons, years = fireYears,
                   studyArea = aggregate(studyArea),
                   pathInputs = Paths$inputPath, 
                 userTags = paste0("years:", range(fireYears)))

# THere are duplicate NFIREID
firePolys <- Cache(lapply, firePolys, function(x) {
  x <- spTransform(x, crs(studyArea))
  x <- x[!duplicated(x$NFIREID), ]
})

if (!exists("useCentroids")){
  if (fire == "SCFM"){
    useCentroids <- FALSE    
  } else {
    if (fire == "fS"){
      useCentroids <- TRUE    
    } else {
     stop("Fire model has not been specified.") 
    }
  }
} 
if (useCentroids) {
    message("... preparing polyCentroids")
    yr <- min(fireYears)
    firePoints <- Cache(mclapply, X = firePolys, 
                            mc.cores = pemisc::optimalClusterNum(2e3, 
                                                                 maxNumClusters = length(firePolys)),
                            function(X){
                              print(yr)
                              ras <- X
                              ras$ID <- 1:NROW(ras)
                              centCoords <- rgeos::gCentroid(ras, byid = TRUE)
                              cent <- SpatialPointsDataFrame(centCoords, 
                                                             as.data.frame(ras))
                              yr <<- yr + 1
                              return(cent)
                            },
                            userTags = c("what:polyCentroids", "forWhat:fireSense_SpreadFit"),
                            omitArgs = c("userTags", "mc.cores", "useCloud", "cloudFolderID")
                        ) # "cacheId:a6f02820c0ff9fa6"
    names(firePoints) <- names(firePolys)
} else {
  NFDBPath <- checkPath(file.path(Paths$inputPath, "NFDB_pointFolder"), create = TRUE)
  if (!file.exists(file.path(NFDBPath, "CHECKSUMS.txt"))){
    Checksums(NFDBPath)
  }
    firePoints <- Cache(getFirePoints_NFDB_V2,
                            url = paste0("http://cwfis.cfs.nrcan.gc.ca/downloads/nfdb/fire_pnt/",
                                         "current_version/NFDB_point.zip"),
                            studyArea = studyArea,
                            rasterToMatch = rasterToMatch,
                            NFDB_pointPath = NFDBPath,
                        years = fireYears,
                        userTags = c("what:firePoints",
                        "forWhat:SCFM"),
                        omitArgs = c("useCache", "purge")
                        )
  }
  
studyAreaPSP <- Cache(prepInputs, url = runNamesList()[RunName == runName, studyAreaPSP],
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      userTags = c("objectName:studyAreaPSP", "extension:BCR6",
                                   stepCacheTag, "outFun:Cache"))

ecoRegionRAS <- Cache(prepInputs, targetFile = "ecoRegionRAS.rds",
                      url = runNamesList()[RunName == runName, ecoRegionRaster],
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      studyArea = studyArea,
                      fun = "readRDS",
                      userTags = c(stepCacheTag,
                                   "step:prepInputsEcoRegion"),
                      omitArgs = c("destinationPath", "filename2", "outFun:Cache"))

anthropogenicLayer <- Cache(prepInputs, targetFile = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.grd",
                                 archive = "bufferMap_v0.1.0_m_r500_t0_anthrDisturb.zip",
                                 alsoExtract = "similar",
                                 url = "https://drive.google.com/open?id=1GhnIjmKsZ3JoxTjefeeBUb02iiEcV_qD",
                                 destinationPath = Paths$inputPath, 
                                 studyArea = studyArea,
                                 rasterToMatch = rasterToMatch,
                                 userTags = c(stepCacheTag,
                                              "step:prepAnthropogenicLayer", "outFun:Cache"))

roadDensity <- Cache(prepInputs, targetFile = "roadDensity_BCR6_NWT_t0.tif",
                          url = "https://drive.google.com/open?id=1C0Y0z1cgQKwa3_-X2qWrhNIzEHIl9m5e",
                          destinationPath = Paths$inputPath, 
                          studyArea = studyArea,
                          rasterToMatch = rasterToMatch,
                          userTags = c(stepCacheTag,
                                       "step:prepRoadDensity",
                                       "objectName:roadDensity", 
                                       "outFun:Cache"))

caribouArea1 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO",
                           targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                           destinationPath = Paths$inputPath,
                           userTags = c(stepCacheTag,
                                        "step:prepCaribouArea1", 
                                        "outFun:Cache"))

caribouArea2 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV",
                      targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                      destinationPath = Paths$inputPath,
                      userTags = c(stepCacheTag,
                                   "step:prepCaribouArea2", 
                                   "outFun:Cache"))

Edehzhie <- Cache(prepInputs, targetFile = "Edehzhie.shp",
                  archive = "Edehzhie.zip",
                  alsoExtract = "similar",
                  url = "https://drive.google.com/open?id=1VP91AyIeGCFwJS9oPSEno4_SbtJQJMh7",
                  studyArea = studyArea,
                  destinationPath = Paths$inputPath,
                  filename2 = NULL,
                  rasterToMatch = rasterToMatch,
                  userTags = c(stepCacheTag, 
                               "outFun:Cache",
                               "step:prepEdehzhie"))
Edehzhie$Name <- Edehzhie$NAME_1

listSACaribou <- list(caribouArea1, caribouArea2, Edehzhie)
names(listSACaribou) <- c("caribouArea1", "caribouArea2", "Edehzhie")

if (!exists("climateModel")) climateModel <- "CCSM4_85" # Default if not provided
if (!climateModel %in% c("CCSM4_85", "CCSM4_45")) 
  stop("Other climate scenarios are still not implemented.")

# TODO Still need to implement this for other provinces. Also need to implement other models! 
# These have been done ONLY with NWT shapefile, I believe!
# 
RCP <- ifelse(climateModel == "CCSM4_85", "85", "45")
climateModelType = ifelse(climateModel == "CCSM4_85", "CCSM4", "CanESM2")# CanESM2 is NOT implemented yet. Here just figurative
ensemble <- ifelse(climateModel == "CCSM4_85", "", "r11i1p1")
climateResolution <- "3ArcMin" # Only available for now, matches the created layers for all modules
climateFilePath <- ifelse(climateModel == "CCSM4_85", 
                          "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8", 
                          "https://drive.google.com/open?id=1U0TuYNMC75sQCkZs7c4EcBRLcjqeVO6N")

# Equivalency table for tree species
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
# Fix EN_generic_short for plotting. Needs to have all names. Don't have time now.
sppColorVect <- LandR::sppColors(sppEquiv = sppEquivalencies_CA, 
                                 sppEquivCol = sppEquivCol,
                                 palette = "Set3")
mixed <- structure("#D0FB84", names = "Mixed")
sppColorVect[length(sppColorVect)+1] <- mixed
attributes(sppColorVect)$names[length(sppColorVect)] <- "Mixed"

if (!exists("Times"))
  Times <- list(start = 2011, end = 2100)

#SCFM
defaultInterval <- 1.0
defaultPlotInterval <- NA
defaultInitialSaveTime <- NA 

# FireSense SpreadFit
#  lower asymptote, upper asymptote, (inflection point), slope at inflection pt, asymmetry
lowerParams <- c(-16, -16, -16, -16, -16, -16)
upperParams <- c(32, 32, 32, 32, 32, 32)
# Spread log function bounds

# for logistic3p
#lower <- c(0.22, 0.001, 0.001, lowerParams)
#upper <- c(0.29, 10, 10, upperParams)

# for logistic2p
lower <- c(0.22, 0.001, lowerParams)
upper <- c(0.29, 10, upperParams)

# Setting up IP's for paralelizing
  cores <- makeIpsForClusters(module = "fireSense",
                              availableCores = c(9,   9,   9,   9,  19,   9,   9,   9,   8))

# TODO For the other areas I will need to reparameterize these.
# I should put these in a table file somehow...
parameters <- list(
  #SCFM
  # ".progress" = list(type = "text", interval = 1),
  Biomass_speciesParameters = list(
    ".useCache" = c(".inputObjects", "init"),
    "sppEquivCol"  = sppEquivCol,
    "GAMMiterations" = 2, 
    "GAMMknots" = list( # This needs to be put into a table of sorts for the specific region. So all species.
      # "Pinu_Con" = 4,
      # "Abie_Bal" = 4,
      # "Popu_Bal" = 4,
      "Betu_Pap" = 3,
      "Lari_Lar" = 4,
      "Pice_Gla" = 3,
      "Pice_Mar" = 4,
      "Pinu_Ban" = 3,
      "Popu_Tre" = 4),
    "minimumPlotsPerGamm" = 40,
    "constrainMortalityShape" = list(
      # "Pinu_Con" = c(15,25),
      # "Abie_Bal" = c(15,25),
      # "Popu_Bal" = c(15,25),
      "Betu_Pap" = c(15,25),
      "Lari_Lar" = c(20,25),
      "Pice_Gla" = c(15,25),
      "Pice_Mar" = c(15,25),
      "Pinu_Ban" = c(15,25),
      "Popu_Tre" = c(15,25)
    ),
    "quantileAgeSubset" = list(
      # "Pinu_Con" = 97,
      # "Abie_Bal" = 95,
      # "Popu_Bal" = 95,
      "Betu_Pap" = 95,
      "Lari_Lar" = 95,
      "Pice_Gla" = 95,
      "Pice_Mar" = 95,
      "Pinu_Ban" = 95,
      "Popu_Tre" = 99
    )
  ),
  scfmLandcoverInit = list(
    ".plotInitialTime" = NA
  ),
  scfmSpread = list(
    "pSpread" = 0.235,
    "returnInterval" = defaultInterval,
    "startTime" = Times$start,
    ".plotInitialTime" = NA,
    ".plotInterval" = defaultPlotInterval,
    ".saveInitialTime" = defaultInitialSaveTime,
    ".saveInterval" = defaultInterval),
  scfmRegime = list(
    "fireCause" = "L",
    "fireEpoch" = c(1970, 2017)),
  scfmDriver = list(
    targetN = 1000),
  # LandR_Biomass
  PSP_Clean = list(
    ".useCache" = c(".inputObjects", "init")
  ),
  Biomass_core = list(
    "successionTimestep" = 10,
    ".plotInitialTime" = NA,
    ".saveInitialTime" = NA,
    ".useCache" = c(".inputObjects", "init"),
    "seedingAlgorithm" = "wardDispersal",
    "initialBiomassSource" = "cohortData",
    "growthAndMortalityDrivers" = definedRun$growthAndMortalityDrivers,
    "keepClimateCols" = FALSE,
    ".useParallel" = 2),
  Biomass_borealDataPrep = list(
    "speciesUpdateFunction" = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable, 
                                      sim$sppEquiv, P(sim)$sppEquivCol))
    ),
    "useCloudCacheForStats" = TRUE,
    "sppEquivCol" = sppEquivCol,
    "successionTimestep" = 10,
    "pixelGroupAgeClass" = 20,
    ".useCache" = c(".inputObjects", "init"),
    # ".useCache" = FALSE,
    "subsetDataBiomassModel" = 50,
    "exportModels" = "all"
  ),
  Biomass_regeneration = list(
    "fireTimestep" = 1,
    "fireInitialTime" = Times$start,
    ".useCache" = c(".inputObjects", "init")
  ),
  gmcsDataPrep = list(
    "GCM" = "CCSM4_RCP8.5"),
  # ".useCache" = c(".inputObjects"),
  fireSense_IgnitionPredict = list(
    ".useCache" = c(".inputObjects", "init"),
    "data" = c("MDC06", "LCC"),
    "modelObjName" = "fireSense_IgnitionFitted_year2011"),
  fireSense_EscapePredict = list(
    ".useCache" = c(".inputObjects", "init"),
  "data" = c("MDC06", "LCC"),
  "modelObjName" = "fireSense_EscapeFitted_year2011"),
  fireSense_SpreadPredict = list(
    ".useCache" = c(".inputObjects", "init"),
    "modelObjName" = "fireSense_SpreadFitted_year2011"#,
  # "coefToUse" = "meanCoef" # Alternatively, we can use the bestCoef from DEOptim
  ),
  fireSense = list(
    "mapping" = list(
      "spreadProbRaster" = "fireSense_SpreadPredicted"
      )),
  # Caribou Population Growth
  caribouPopGrowthModel = list(
    ".plotInitialTime" = NULL,
    "recoveryTime" = 40,
    ".useCache" = c(".inputObjects"),
    ".useDummyData" = FALSE,
    ".growthInterval" = 5),
  # fireSense
  fireSense_dataPrep = list(
    "train" = FALSE,
    "whichModulesToPrepare" = c("fireSense_SpreadPredict",
                                "fireSense_IgnitionPredict"),
    "RCP" = RCP,
    "climateModel" = climateModelType,
    "ensemble" = ensemble,
    "climateResolution" = climateResolution,
    "climateFilePath" = climateFilePath
  ),
  fireSense_SpreadFit = list(
    "formula_fire" = formula(~ 0 + weather + class1 + class2 + class3 + class4 + class5), # Formula of the statistical model
    "fireAttributesFireSense_SpreadFit" = "fireAttributesFireSense_SpreadFit", # Default
    "data" = c("weather", "class1", "class2", "class3", "class4", "class5"),
    # Here are the bounds for: 5 parameters for log fun + n parameters for the model (i.e. n terms of a formula)
    "lower" = lower,
    "upper" = upper,
    "fireYears" = fireYears,
    "cores" = if (isRstudioServer()) NULL else cores, #rep("localhost", 40), #cores,
    "iterDEoptim" = 150,
    "iterStep" = 150,
    "minBufferSize" = 2000,
    "debugMode" = FALSE, # DEoptim may spawn many machines via PSOCK --> may be better from cmd line
    "rescaleAll" = TRUE,
    "NP" = length(cores),
    "objFunCoresInternal" = 3L,
    "maxFireSpread" = 0.3,
    "objfunFireReps" = 100,
    "verbose" = TRUE,
    "trace" = 1,
    "visualizeDEoptim" = TRUE,
    #initialpop = if (exists("aa")) aa$member$pop else NULL#[sample(seq_len(NROW(aa$member$pop)), length(cores)),]
    # "40927e9ca42d33b3", "56769e2b2edfe8ab",  "c3af84b504e99a5d", # This is NWT DEoptim Cache, newer to older
    "cacheId_DE" = runNamesList()[RunName == runName, 
                                  DEoptimCache], # This is NWT DEoptim Cache
    "cloudFolderID_DE" = "1kUZczPyArGIIkbl-4_IbtJWBhVDveZFZ",
    "useCloud_DE" = TRUE
  )
)

succTS <- c(seq(Times$start, Times$end, 
                by = parameters$Biomass_core$successionTimestep), Times$end)
outputsLandR <- data.frame(
  objectName = rep(c("burnMap",
                     "cohortData",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap",
                     "MDC06"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 8))
)
lastYears <- data.frame(objectName = c("predictedCaribou", "plotCaribou", 
                                       "fireRegimeRas", "speciesEcoregion", 
                                       "species", "gcsModel", "mcsModel", 
                                       "spreadPredictedProbability"),
                        saveTime = Times$end)
if (length(usefulFuns::grepMulti(x = definedRun$modules, "Biomass_core")) != 0){
  clim <- data.frame(objectName = rep(c("fireSense_IgnitionPredicted", 
                                        "fireSense_EscapePredicted", "burnSummary", 
                                        "successionLayers", "activePixelIndex"), 
                                      each = 3),
                     saveTime = rep(c(Times$start, round((Times$start + Times$end)/2, 0), 
                                      Times$end), 
                                    Times = 1))
} else {
  clim <- NULL
}

outputsLandR <- unique(rbind(outputsLandR, lastYears, clim))

objects <- list(
  "studyAreaPSP" = studyAreaPSP,
  "rasterToMatchLarge" = rasterToMatch,
  "rasterToMatch" = rasterToMatch,
  "studyAreaLarge" = studyArea,
  "studyArea" = studyArea,
  "sppEquiv" = sppEquivalencies_CA,
  "sppEquivCol" = sppEquivCol,
  "sppColorVect" = sppColorVect,
  "omitNonVegPixels" = TRUE,
  "cloudFolderID" = cloudFolderID,
  "studyArea" = studyArea,
  "waterRaster" = waterRaster,
  "fireRegimePolys" = studyArea,
  "ecoregionRst" = ecoRegionRAS,
  "ecoregionLayer" = ecoRegionRAS,
  "flammableRTM" = flammableRTM,
  "uplandsRaster" = uplandsRaster,
  "wetLCC" = watersRaster,
  "rstLCC" = rstLCC,
  "anthropogenicLayer" = anthropogenicLayer,
  "caribouArea1" = caribouArea1,
  "caribouArea2" = caribouArea2,
  "Edehzhie" = Edehzhie,
  "roadDensity" = roadDensity,
  "firePolys" = firePolys,
  "firePoints" = firePoints,
  "listSACaribou" = listSACaribou
)

data.table::setDTthreads(2) # Data.table has all threads by default, 
# which is inconveninent and unecessary. Will try setting it for only 2 cores.  

if (runOnlySimInit){
  spadesFun <- "simInit"
} else {
  spadesFun <- "simInitAndSpades"
}

