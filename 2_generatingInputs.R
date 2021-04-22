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

if (!exists("Times"))
  Times <- list(start = 2011, end = 2100)

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

# ~~~~~~~~~~~~~~~~~~~~ FOR CARIBOU

studyAreaCaribou <- Cache(prepInputs, targetFile = "NT1_BCR6.shp",
                          archive = "NT1_BCR6.zip",
                          alsoExtract = "similar",
                          url = "https://drive.google.com/file/d/1RPfDeHujm-rUHGjmVs6oYjLKOKDF0x09/view?usp=sharing",
                          destinationPath = Paths$inputPath,
                          rasterToMatch = rasterToMatch,
                          filename2 = "NT1_BCR6",
                          fun = "raster::shapefile",
                          userTags = c(stepCacheTag,
                                       "outFun:Cache", "step:prepCaribouSA"))

caribouLCC <- Cache(prepInputs, targetFile = "EOSD_NT1_BCR6_JAN21.tif",
                    archive = "EOSD_NT1_BCR6_JAN21.zip",
                    alsoExtract = "similar",
                    url = "https://drive.google.com/file/d/11VEo4dhI0uFcWTlmm41bMFGkiE8wKpKT",
                    studyArea = studyAreaCaribou,
                    rasterToMatch = rasterToMatch,
                    destinationPath = Paths$inputPath,
                    filename2 = "EOSD_NT1_BCR6_250m_caribou",
                    fun = "raster::raster",
                    userTags = c(stepCacheTag,
                                 "outFun:Cache", "step:prepEOSDcaribou"))

# As the caribou RSF doesn't have broadleaf sparse (223) and mixedwood sparse (233), we should reclassify
# these to to broadleaf open (222) and mixedwood open (232), respectively. This was done in the same
# way as GNWT (J. Hodson) did to update RSF maps. 
caribouLCC[caribouLCC == 223] <- 222 # originally 667 pixels
caribouLCC[caribouLCC == 233] <- 232 # originally 2517 pixels
caribouLCC[caribouLCC == 255] <- NA # originally 2 pixels. It is not even in the metadata. Go figure!

anthropogenicLayers <- Cache(prepInputs, targetFile = "anthropogenicDisturbanceLayers_NT1_BCR6.grd",
                             archive = "anthropogenicDisturbanceLayers_NT1_BCR6.zip",
                             alsoExtract = "anthropogenicDisturbanceLayers_NT1_BCR6.gri",
                             url = "https://drive.google.com/file/d/1npwXsabARoLeGKNKhdC_7j-OJSKCOJdC/view?usp=sharing",
                             destinationPath = Paths$inputPath,
                             studyArea = studyAreaCaribou,
                             rasterToMatch = caribouLCC,
                             filename2 = "anthropogenicDisturbanceLayers_NT1_BCR6",
                             fun = "raster::stack",
                             userTags = c("FUN:Cache",
                                          "object:anthropogenicLayersNT1BCR6"))
# [UPDATE 12JAN21]: One of the layers got named wrongly, so I will fix here:
names(anthropogenicLayers)[names(anthropogenicLayers) == "lineDen1000"] <- "lden1000_2015"
names(anthropogenicLayers)[names(anthropogenicLayers) == "exp_sett"] <- "exp_settle"

# ~~~~~~~~~~~~~~~~~~~~ 

rasterToMatch <- raster(caribouLCC)
rasterToMatch[caribouLCC != 0] <- 1
rasterToMatch[rasterToMatch == 0] <- NA
caribouLCC[is.na(rasterToMatch)] <- NA
studyArea <- studyAreaCaribou

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ WATER ~~~~~~~~~~~~~~~~~~~~~~~~

#  THE PROBLEM WITH THE SIMULATIONS IS HERE!!! IT IS THE WATER LAYER!
#  WE ONLY HAVE THE WATER FOR NWT_BCR6, and it is setting whatever 
#  outside of this area to water!! 
#  SOLUTION: EXTRACT THE WATER LAYER FROM THE LCC (EOSD). Won't change much, but will
#  keep consistency!

# New EOSD Water, Uplands and Lowlands derived
UplandClass <- c(32:34, 40, 51:52, 211:213, 221:223, 231:233)
WaterClass <- c(20, 31)
WetlandClass <- c(81:83, 100) #so for water, for example, anything other than c(20, 31) is set to NA

watersVals <- raster::getValues(caribouLCC)
# Fix missing 1's in Mackenzie River + Slave Lake
rtmVals <- getValues(rasterToMatch)
watersVals[is.na(watersVals) & rtmVals == 1] <- 20

# Water
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & !(watersValsToChange %in% WaterClass)] <- NA
watersValsToChange[!is.na(watersValsToChange)] <- 1
waterRaster <- raster::setValues(x = raster(caribouLCC), watersValsToChange)
# Lowlands
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & !(watersValsToChange %in% WetlandClass)] <- NA
watersValsToChange[!is.na(watersValsToChange)] <- 1
wetlandsRaster <- raster::setValues(x = raster(caribouLCC), watersValsToChange)
# Uplands
watersValsToChange <- watersVals 
watersValsToChange[!is.na(watersValsToChange) & !(watersValsToChange %in% UplandClass)] <- NA
watersValsToChange[!is.na(watersValsToChange)] <- 1
uplandsRaster <- raster::setValues(x = raster(caribouLCC), watersValsToChange)

# Making wetLCC
watersRaster <- raster(caribouLCC)
watersRaster[waterRaster == 1] <- 1
watersRaster[wetlandsRaster == 1] <- 2
watersRaster[uplandsRaster == 1] <- 3

############ ORIGINAL WATER PREP WITH DUCKS LAYER #########
####  Prep Layers: Exclude water, rocks and ice from flammableRTM --> NA
# watersRaster <- Cache(prepInputs, url = runNamesList()[RunName == runName, watersRaster],
#                       destinationPath = Paths$inputPath,
#                       studyArea = studyArea,
#                       rasterToMatch = rasterToMatch,
#                       filename2 = NULL,
#                       # cacheId = "b097c68ef07d6978",
#                       userTags = c("objectName:watersRaster", stepCacheTag, "outFun:Cache"),
#                       omitArgs = c("destinationPath", "filename2"))
# watersVals <- raster::getValues(watersRaster) # Uplands = 3, Water = 1, Wetlands = 2, so 2 and 3 to NA

# # Fix missing 1's in Mackenzie River + Slave Lake
# rtmVals <- getValues(rasterToMatch)
# watersVals[is.na(watersVals) & rtmVals == 1] <- 1

# watersValsToChange <- watersVals 
# watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 1] <- NA
# waterRaster <- raster::setValues(x = watersRaster, watersValsToChange)
# watersValsToChange <- watersVals
# watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 2] <- NA
# watersValsToChange[watersValsToChange == 2] <- 1
# wetlandsRaster <- raster::setValues(x = watersRaster, watersValsToChange)
# watersValsToChange <- watersVals 
# watersValsToChange[!is.na(watersValsToChange) & watersValsToChange != 3] <- NA
# watersValsToChange[watersValsToChange == 3] <- 1
# uplandsRaster <- raster::setValues(x = watersRaster, watersValsToChange)

############ END OF ORIGINAL WATER PREP WITH DUCKS LAYER #########



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ LAND COVER ~~~~~~~~~~~~~~~~~~~~~~~~


# Original LCC05 landcover product
# rstLCC <- Cache(prepInputs, url = paste0("ftp://ftp.ccrs.nrcan.gc.ca/ad/NLCCLandCover/",
#                              "LandcoverCanada2005_250m/LandCoverOfCanada2005_V1_4.zip"),
#                 targetFile = file.path(Paths$inputPath, "LCC2005_V1_4a.tif"),
#                 archive = asPath("LandCoverOfCanada2005_V1_4.zip"),
#                 destinationPath = Paths$inputPath,
#                 studyArea = studyArea,
#                 rasterToMatch = rasterToMatch,
#                 maskWithRTM = TRUE,
#                 method = "bilinear",
#                 datatype = "INT2U",
#                 filename2 = TRUE,
#                 userTags = c(stepCacheTag,
#                              "objectName:rstLCC", "prepInputsrstLCC_rtm",
#                              "outFun:Cache"),
#                 omitArgs = c("destinationPath", "filename2"))


# The original file "EOSD_2000_2007_combined.zip/EOSD_Mosaic.gdb" was manually converted to 
# TIFF in arcMap and uploaded GoogleDrive
# rstLCC <- Cache(prepInputs, targetFile = "EOSD_NT1_BCR6_250m_caribou.tif",
#                 archive = "EOSD_NT1_BCR6_250m_caribou.zip", # <== Check this layer
#                 alsoExtract = "similar",
#                 url = "https://drive.google.com/file/d/1VMIpeUb2ZTcC9NpQ83YKaWq2sDwwKI8d/view?usp=sharing",
#                 studyArea = studyArea,
#                 rasterToMatch = rasterToMatch,
#                 destinationPath = Paths$inputPath,
#                 filename2 = "EOSD_NT1_BCR6_250m_caribou",
#                 fun = "raster::raster",
#                 userTags = c(stepCacheTag,
#                              "outFun:Cache", "step:prepEOSD"))
# NECESSARY DUE TO A BUG IN postProcess. Should be fine with
# the next version of it
# rstLCC <- Cache(postProcess, 
#                 x = rstLCC, 
#                 destinationPath = Paths$inputPath,
#                 rasterToMatch = rasterToMatch,
#                 userTags = c(stepCacheTag,
#                              "outFun:Cache", "step:reampleEOSD"))
# LCC05
# Ice/snow = 39
# Water (LCC05) = 37:38
# Rocks = 33
# Urban = 36

rstLCC <- caribouLCC

# EOSD
caribouRSFTable <- Cache(prepInputs, url = paste0("https://drive.google.com",
                                                      "/file/d/19ex5N5Z0Ow8fXPW",
                                                      "TXJuSU8A3zvFd69vK/view?u",
                                                      "sp=sharing"),
                             destinationPath = Paths$inputPath,
                             fun = "data.table::fread", 
                             userTags = "tableRSF")

# Reclassify EOSD with LCC05 code
reclassTB <- Cache(prepInputs, url = paste0("https://drive.google.com/file/d/",
                                            "1YUXcx8Gc6dI4vy76l2k_P6tUm6X2m7M",
                                            "G/view?usp=sharing"),
                   targetFile = "EOSD_LCC05_ConversionTable.csv",
                   destinationPath = Paths$inputPath,
                   fun = "data.table::fread", 
                   userTags = c("version:noSparse", 
                                "conversionEOSD_LCC05"))

reclassMatrix <- usefulFuns::makeReclassifyMatrix(table = reclassTB, 
                                                  originalCol = "EOSD_Class", 
                                                  reclassifiedTo = "LCC05_Class")
rstLCC <- raster::reclassify(x = rstLCC, rcl = reclassMatrix[,-1])

# Improve water classification of the rstLCC with the waterRaster from DUCKS
rstLCC[waterRaster == 1] <- 37 # 37 is LCC05 classification for water

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIRE ~~~~~~~~~~~~~~~~~~~~~~~~

  nonFlammClass <- c(33, 36:39)
  flammableRTM <- rasterToMatch
  # Remove LCC non flammable classes first
  flammableRTM[rstLCC[] %in% nonFlammClass] <- NA
  # Remove more detailed water from Water layer
  flammableRTM[waterRaster[] == 1] <- NA

fireYears <- 1991:2017
firePolys <- Cache(getFirePolys, years = fireYears,
                   studyArea = aggregate(studyArea),
                   version = c(20200921, 20200703, 20191129, 20190919),
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
  
# For Caribou --> Need fires older than what we have for fireSense
historicalFires <- Cache(prepInputs, url = "https://drive.google.com/file/d/1WPfNrB-nOejOnIMcHFImvnbouNFAHFv7",
                         alsoExtract = "similar",
                         destinationPath = Paths$inputPath,
                         studyArea = studyArea,
                         userTags = c("objectName:historicalFires", 
                                      "extension:BCR6_NWT",
                                      stepCacheTag, "outFun:Cache"))
# simplifying
historicalFiresS <- historicalFires[, names(historicalFires) %in% c("YEAR", "DECADE")]
historicalFiresDT <- data.table(historicalFiresS@data)
historicalFiresDT[, decadeYear := 5+(as.numeric(unlist(lapply(strsplit(historicalFiresDT$DECADE, split = "-"), `[[`, 1))))]
historicalFiresDT[, fireYear := ifelse(YEAR == -9999, decadeYear, YEAR)]
historicalFiresS$fireYear <- historicalFiresDT$fireYear
historicalFires <- historicalFiresS[, "fireYear"]
historicalFiresReproj <- projectInputs(historicalFires, targetCRS = as.character(crs(studyArea)))

# Discard fires with more than 60 from starting time
olderstFireYear <- Times$start-60
historicalFires <- historicalFiresReproj[historicalFiresReproj$fireYear >= olderstFireYear,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OTHER LAYERS ~~~~~~~~~~~~~~~~~~~~~~~~

studyAreaPSP <- Cache(prepInputs, url = runNamesList()[RunName == runName, studyAreaPSP],
                      alsoExtract = "similar",
                      destinationPath = Paths$inputPath,
                      userTags = c("objectName:studyAreaPSP", "extension:BCR6",
                                   stepCacheTag, "outFun:Cache"))



ecoRegionSHP <- Cache(prepInputs, targetFile = "ecoregions.shp",
                                            url = "https://drive.google.com/file/d/1qr9roO6lCMomSfS5tfBdMMJsYk5eB6K7",
                                            alsoExtract = "similar",
                                            destinationPath = Paths$inputPath,
                                            studyArea = studyAreaCaribou,
                                            userTags = c(stepCacheTag,
                                                         "step:prepInputsEcoRegion"),
                                            omitArgs = c("destinationPath"))
ecoRegionSHP <- projectInputs(ecoRegionSHP,
                              destinationPath = Paths$inputPath,
                              targetCRS = crs(rasterToMatch))
ecoRegionSF <- sf::st_as_sf(ecoRegionSHP)
ecoRegionRAS <- fasterize::fasterize(sf = ecoRegionSF, 
                                     raster = caribouLCC, 
                                     field = "ECOREGION")
  
# NWT/BCR6 ecoregions raster: older version
# ecoRegionRAS <- Cache(prepInputs, targetFile = "ecoRegionRAS.rds",
#                       url = runNamesList()[RunName == runName, ecoRegionRaster],
#                       alsoExtract = "similar",
#                       destinationPath = Paths$inputPath,
#                       studyArea = studyArea,
#                       fun = "readRDS",
#                       userTags = c(stepCacheTag,
#                                    "step:prepInputsEcoRegion"),
#                       omitArgs = c("destinationPath", "filename2", "outFun:Cache"))

bufferedAnthropogenicDisturbance500m <- Cache(prepInputs, targetFile = "buffered500mDisturbancesUnified_NT1_BCR6.shp",
                                 archive = "buffered500mDisturbancesUnified_NT1_BCR6.zip",
                                 alsoExtract = "similar",
                                 url = "https://drive.google.com/file/d/1yz39dGW4XMJk5ox6TuVUOMrU4q3mhfhU/view?usp=sharing",
                                 destinationPath = Paths$inputPath, 
                                 studyArea = studyArea,
                                 rasterToMatch = rasterToMatch,
                                 userTags = c(stepCacheTag,
                                              "step:prepAnthropogenicDistLayer", "outFun:Cache"))
bufferedAnthropogenicDisturbance500mSF <- sf::st_as_sf(bufferedAnthropogenicDisturbance500m)
bufferedAnthropogenicDisturbance500mSF$fieldSF <- 1
bufferedAnthropogenicDisturbance500m <- fasterize::fasterize(sf = bufferedAnthropogenicDisturbance500mSF,
                                                             raster = rasterToMatch, field = "fieldSF", 
                                                             background = 0)
buffAnthroDist500m <- Cache(postProcess, x = bufferedAnthropogenicDisturbance500m,
                                  destinationPath = Paths$inputPath, 
                                  studyArea = studyArea,
                                  rasterToMatch = rasterToMatch,
                                  userTags = c(stepCacheTag,
                                               "step:maskAnthropogenicDistLayer", "outFun:Cache"))
 
# Older version of road density for ECCC 2011 RSF. To update it, a 10km buffered layer still needs to be
# created. The current one used for the DeMars et al., 2019 model for NWT is 1km density.
# roadDensity <- Cache(prepInputs, targetFile = "roadDensity_BCR6_NWT_t0.tif",
#                           url = "https://drive.google.com/open?id=1C0Y0z1cgQKwa3_-X2qWrhNIzEHIl9m5e",
#                           destinationPath = Paths$inputPath,
#                           studyArea = studyArea,
#                           rasterToMatch = rasterToMatch,
#                           userTags = c(stepCacheTag,
#                                        "step:prepRoadDensity",
#                                        "objectName:roadDensity",
#                                        "outFun:Cache"))

caribouArea1 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Qbt2pOvC8lGg25zhfMWcc3p6q3fZtBtO",
                           targetFile = "NWT_Regions_2015_LCs_DC_SS_combined_NT1_clip_inc_Yukon.shp",
                           destinationPath = Paths$inputPath,
                           alsoExtract = "similar", .useCache = "overwrite",
                           rasterToMatch = rasterToMatch,
                           userTags = c(stepCacheTag,
                                        "step:prepCaribouArea1", 
                                        "outFun:Cache"))

caribouArea2 <- Cache(prepInputs, url = "https://drive.google.com/open?id=1Vqny_ZMoksAjji4upnr3OiJl2laGeBGV",
                      targetFile = "NT1_BOCA_spatial_units_for_landscape_projections.shp",
                      destinationPath = Paths$inputPath,
                      alsoExtract = "similar",
                      rasterToMatch = rasterToMatch,
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


metaHeards <- Cache(prepInputs, 
                       targetFile = "Enhanced_MetaHerds_20191029.shp",
                       archive = "Johnsonetal2020_studyareas.zip",
                       alsoExtract = "similar",
                       url = "https://drive.google.com/file/d/1lH_Oy2pEHv9dtSsAXn-UrDHrW1aJOOCd",
                       studyArea = studyArea,
                       destinationPath = Paths$inputPath,
                       filename2 = NULL,
                       rasterToMatch = rasterToMatch,
                       userTags = c(stepCacheTag, 
                                    "outFun:Cache",
                                    "step:prepHeardsPoly"))
  
listSACaribou <- list(caribouArea1, caribouArea2, Edehzhie, metaHeards)
names(listSACaribou) <- c("caribouArea1", "caribouArea2", "Edehzhie", "metaHeards")

# Protected areas
protectedAreas <- Cache(prepInputs, 
                    targetFile = "protAreas_NT1_BCR6.shp",
                    archive = "protAreas_NT1_BCR6.zip",
                    alsoExtract = "similar",
                    url = "https://drive.google.com/file/d/1byUcmQXvxbgTT3Q-3DwVKTVRmnqwsRVO",
                    studyArea = studyArea,
                    destinationPath = Paths$inputPath,
                    filename2 = NULL,
                    rasterToMatch = rasterToMatch,
                    userTags = c(stepCacheTag, 
                                 "outFun:Cache",
                                 "step:protectedAreas_NT1_BCR6"))


landscapeUnits <- Cache(prepInputs, 
                        archive = "ca_all_slc_v3r2.zip",
                        alsoExtract = "similar",
                        url = "https://sis.agr.gc.ca/nsdb/ca/cac003/cac003.20110308.v3.2/ca_all_slc_v3r2.zip",
                        studyArea = studyArea,
                        destinationPath = Paths$inputPath,
                        filename2 = NULL,
                        rasterToMatch = rasterToMatch,
                        userTags = c(stepCacheTag,
                                     "outFun:Cache",
                                     "step:landscapeUnits_NT1_BCR6"))

landscapeUnitSF <- sf::st_as_sf(landscapeUnits)
landscapeUnitsRAS <- fasterize::fasterize(sf = landscapeUnitSF, 
                                     raster = rstLCC, 
                                     field = "POLY_ID")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ CLIMATE ~~~~~~~~~~~~~~~~~~~~~~~~

if (!exists("climateModel")) climateModel <- "CCSM4_RCP85" # Default if not provided
if (!climateModel %in% c("CCSM4_85",
                         "CCSM4_RCP85",
                         "ACCESS1-0_RCP85", 
                         "CanESM2_RCP85",
                         "CSIRO-Mk3-6-0_RCP85", 
                         "INM-CM4_RCP85", 
                         "CNRM-CM5_RCP85")) 
  stop("Other climate scenarios are still not implemented.")

RCP <- strsplit(x = climateModel, split = "_")[[1]][2]
climateModelType <- strsplit(x = climateModel, split = "_")[[1]][1]
ensemble <- ifelse(climateModelType %in% c("CCSM4_85",
                                           "CCSM4",
                                           "INM-CM4", 
                                           "CNRM-CM5", 
                                           "ACCESS1-0"), "", "r51i1p1")
climateResolution <- "3ArcMin" # Only available for now, matches the created layers for all modules
climateFilePath <- getAnnualClimateZipURL(scenario = climateModel)

# For any climate scenarios that not CCSM4_85, we need to supply CMI and ATA stacks
# These are annual projected mean annual temperature anomalies, units stored as tenth of a degree (ATA) and annual projected mean climate moisture deficit

CMIATA <- makeCMIandATA(pathToNormalRasters = file.path(Paths$inputPath, 
                                                        "Canada3ArcMinute_Normals"),
                     pathToFutureRasters = file.path(Paths$inputPath, climateModel), 
                     rasterPrefix = climateResolution,
                     climateModel = climateModel,
                     outputDir = Paths$inputPath,
                     studyArea = studyArea,
                     rasterToMatch = rasterToMatch,
                     years = 2011:2100)

CMInormal <- CMIATA$normalsCMI
CMIstack <- CMIATA$futureCMI
ATAstack <- CMIATA$futureATA

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TREE SPECIES ~~~~~~~~~~~~~~~~~~~~~~~~

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ MODULES' PARAMETERS ~~~~~~~~~~~~~~~~~~~~~~~~

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
    "subsetDataBiomassModel" = 50,
    "exportModels" = "all" #, [01FEB21: TM -- This below was in 213 but not here. It might be needed 
    #                         when running directly from EOSD layer instead of converting to LCC05 as
    #                         I am doing now.]
    # "LCCClassesToReplaceNN" = 11:12, #Cloud/Shadow
    # "forestedLCCClasses" = forestedClasses
  ),
  Biomass_regeneration = list(
    "fireTimestep" = 1,
    "fireInitialTime" = Times$start,
    ".useCache" = c(".inputObjects", "init")
  ),
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
  )#, [01FEB21: TM -- This below was in 213 but not here. It is needed 
  #                         when running directly from EOSD layer instead of converting to LCC05 as
  #                         I am doing now. But might (most likele will) be useless when we actually have the 
  #                         whole fireSense scheme working.]
  # LBMR2LCC_DataPrep = list(
  # "trainingAndMappingFuns" = c("trainXGBModel_EOSD", "MapBiomassToLCC_EOSD"))
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OUTPUTS ~~~~~~~~~~~~~~~~~~~~~~~~


succTS <- c(seq(Times$start, Times$end, 
                by = parameters$Biomass_core$successionTimestep), Times$end)
if (Times$end > 2017)
  succTS <- sort(c(2017, succTS))
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
                                       "spreadPredictedProbability", 
                                       "rstCurrentBurnList",
                                       "caribouPredictions", "disturbances"),
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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ OBJECTS ~~~~~~~~~~~~~~~~~~~~~~~~


objects <- list(
  "CMInormal" = CMInormal,
  "CMIstack" = CMIstack,
  "ATAstack" = ATAstack,
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
  "rstLCC" = rstLCC,
  "bufferedAnthropogenicDisturbance500m" = buffAnthroDist500m, # Buffered disturbances 500m
  "anthropogenicLayers" = raster::stack(anthropogenicLayers), # New RSF anthropogenic layers for NT1+BCR6 (exp_dist_sett, exp_maj_rod, etc)
  "caribouLCC" = caribouLCC,
  # "roadDensity" = roadDensity, # Used only in the older caribouRSF module
  "firePolys" = firePolys,
  "firePoints" = firePoints,
  "listSACaribou" = listSACaribou,
  "historicalFires" = historicalFires,
  "NT1shapefile" = caribouArea2,
  ".studyAreaName" = runName
)

data.table::setDTthreads(2) # Data.table has all threads by default, 
                            # which is inconveninent and unecessary. 
                            # Will try setting it for only 2 cores.  
if (runOnlySimInit){
  spadesFun <- "simInit"
} else {
  spadesFun <- "simInitAndSpades"
}

