
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~> ATTENTION: <~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# NEED TO FIX THE biomassModel in Boreal_DataPrep. Ideas for that:
# 1. Not subsetting by 50 (a parameter) to 100
# 2. Use Ecoregion map instead of Ecodistrict
# 3. Create a way of merging LCC classes (c/o Ceres: merge levels of some covariates so that tehre are more points for each combination of factor levels
# ie. use `sub('_.*', '', ecoregion)` as a variable in the model -- not sure it would work) 
# 4. Use a mergeSliver type of function from @Ian to merge small ecodistricts.

mainFold <- "/mnt/data/Micheletti/NWT/outputs/09OCT19/LandR.CS_fS/run1"
filesToUpload <- c("CAWA", "OSFL", "RUBL", "BBWA", "WEWP")
fl <- lapply(filesToUpload, function(fl){
  ras <- usefun::grepMulti(x = list.files(mainFold, recursive = TRUE,
                                   full.names = TRUE), patterns = fl)
  ras <- ras[-length(ras)]
  lapply(X = ras, FUN = function(eachRas){
    googledrive::drive_upload(eachRas,
                              path = googledrive::as_id("16IT3t0Gaokh-kCwhSjcGWRz9EQ9cFMUD"))
  })
})

# fl <- usefun::grepMulti(list.files(file.path(getwd(), "outputs/23OCT19/LandR_SCFM/run1"), recursive = TRUE, full.names = TRUE), 
#                         patterns = c("RAS_", "2100"))
# lapply(X = fl, FUN = function(eachRas){
#   googledrive::drive_upload(eachRas,
#                             path = googledrive::as_id(LandR_SCFM))
# })
# 
pLandR.CS_fS <- "1ooFQ4IbkVtVL3topEvz_3vAMu-6h-hyB"
pLandR.CS_SCFM <- "1Nuo091FmyFnrfLKVvQXjGoTN8fRUfXIJ"
pLandR_fS <- "1PmQR0SkNOrfi5fq_a16PTmsYoLw8zSJJ"
pLandR_SCFM <- "1A6BslpcPi4D_9bZ_2DC7sgEXz447cuRv"

dataFolder <- list(
  LandR.CS_fS = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "caribouPredictions"),
  LandR_fS = file.path(getPaths()$inputPath, "LandR_fS", Run, "caribouPredictions"),
  LandR.CS_SCFM = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "caribouPredictions"),
  LandR_SCFM = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "caribouPredictions")
  )

lapply(X = names(dataFolder), FUN = function(eachComb){
  fls <- list.files(dataFolder[[eachComb]], full.names = TRUE)
  lapply(X = fls, FUN = function(eachRas){
    googledrive::drive_upload(eachRas,
                              path = googledrive::as_id(get(paste0("p", eachComb))))
  })
})

fl <- usefun::grepMulti(x = list.files(path = "/mnt/data/Micheletti/NWT/outputs/23OCT19/birdResults/", full.names = TRUE),
                        patterns = c("Selection", "tif"))   
lapply(X = fl, FUN = function(eachRas){
  googledrive::drive_upload(eachRas,
                            path = googledrive::as_id("16IT3t0Gaokh-kCwhSjcGWRz9EQ9cFMUD"))
})

ras <- grepMulti(list.files("/mnt/data/Micheletti/NWT/outputs/23OCT19/effectsRasters", full.names = TRUE),
                 patterns = c("rds"))
lapply(ras, function(r){
  googledrive::drive_upload(r, googledrive::as_id("1jgGqwGcOpxSjUwxJw8_bee-iQtcHSRTG"))
})


# fireSense:
# The only things I need to run are:
# 1. Include in fireSense_NWT_DataPrep the function to create sim$MDC06 these. This needs to be redone every year! LCC "too" but cached [ DONE ]
# 2. Run every year: fireSense_NWT_DataPrep, "fireSense_FrequencyPredict", "fireSense_EscapePredict", "LBMR2LCC_DataPrep", "fireSense_NWT" [ REDEFINE definedRun, Push prepareClimateLayers to usefun]

# line 661
if (saveInitialConditions){
  saveRDS(sim$activePixelIndex, file = file.path(outputPath(sim), "pixelsWithDataAtInitialization.rds"))
}


# FEB 13th 2020: This is the latest working version of the modules

#tmichele@ubuntu1tb:/mnt/data/Micheletti/NWT$ git submodule
# d248111e8dbf75495be431c0e8f0726ddba88e07 modules/Biomass_regeneration (heads/development)
# 7cc032c13c82abb731dbb67d91749a99c39ee6d3 modules/Boreal_LBMRDataPrep (heads/development)
# cd0e267cf87b5729d84fd980b111f7be189906bd modules/LBMR (v1.3.2-3-gcd0e267)
# d2533f11b45886dba7540131d3c6311ea02845ea modules/LBMR2LCC_DataPrep (heads/master)
# 0d674d3a264e59148aa03fa4f3d004a007d115d2 modules/LandR_BiomassGMOrig (heads/development)
# 9f351b9dc18ba76e1a94abaf479686c12b872a89 modules/LandR_speciesParameters (heads/master)
# 881957951841d667734556b84e39789622518343 modules/MDC_NWT_DataPrep (heads/master)
# 1517fae323ab5f3b2259cb92a7082586812cbfbf modules/PSP_Clean (heads/master)
# 196d8d06103a674cb2dd97400865381ccb4562ff modules/birdsNWT (heads/master)
# 1274a19a2850ae53c89f22c422bb57f151bcda31 modules/caribouPopGrowthModel (heads/master)
# 31c69d99bcfe22cc7e34c6537dd19a2a19559619 modules/caribouRSF (heads/master)
# deb602530b6244adc231cfa59cccac9d3d26319c modules/climate_NWT_DataPrep (heads/master)
# d05bdb786fc39ace130ca0f663e92168958367ca modules/comm_metricsNWT (heads/master)
# c83efa780fa5b3aa92fa7d23853269f4ef36cc8c modules/fireSense_EscapePredict (heads/master)
# 16df9d4283d9800c3eaacc59fce44a0d90ae2a8b modules/fireSense_IgnitionPredict (heads/master)
# 267c35529bdad97a7f373706d7bf8282fc4be5e2 modules/fireSense_NWT (heads/master)
# 99b21c4883e520eefb7f091a41c12828fc9bc9f0 modules/fireSense_NWT_DataPrep (heads/master)
#+c42700c09423d2289558bda353a2dfc8f8e35013 modules/gmcsDataPrep (heads/development)
# 98f04b4ae6779dfcee89f3f19058302d8ed499ca modules/scfm (heads/development)

# And this is the version of the packages
#> remotes:::local_sha("reproducible")
#[1] "b177d646e2438681d1b7cf9da1a5a596f01296c6"
#> remotes:::local_sha("SpaDES.core")
#[1] "e500e1c5473ac28ff2110d4e45667bbc67a032c8"
#> remotes:::local_sha("SpaDES.tools")
#[1] "6f22c2c3d5b521d2771e5349a2da4fdbeb3faeaa"
#> remotes:::local_sha("quickPlot")
#[1] "464de9dc22467a3c071bf257f83cc14e566829e1"
#> remotes:::local_sha("amc")
#[1] "153a67b69d5e26ffa18530f5b028b5cda0afeb6f"
#> remotes:::local_sha("usefun")
#[1] "417892e74d5aaba074cd7261a4359b1352e05362"
#> remotes:::local_sha("pemisc")
#[1] "54ea365a885889215e05072084df1a17b0cbbe56"
#> remotes:::local_sha("data.table")
#[1] "1.12.6"
#> remotes:::local_sha("LandR")
#[1] "dce51666b4539919c51491735747697f6eb31954"
#> remotes:::local_sha("LandR.CS")
#[1] "0.0.0.9001"

