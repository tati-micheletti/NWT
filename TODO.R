# I have decided I will run 5 times each: 
#  - SCFM+LandR [  ]
#  - SCFM+LandR.CS [  ]
#  - fireSense+LandR [  ]
#  - fireSense+LandR.CS [  ]
#  - run bird models: (terrain + ...) 
#  - V4 (...vegetation) # NOT DOING IT FOR THE BIRDS MANUSCRIPT
#  - V5 (...climate) # NOT DOING IT FOR THE BIRDS MANUSCRIPT
#  - V6 (...vegetation + climate) # ==> This is the model to be used.

# 2. Make all plots that relate 2 or more types of run (SCFM vs fS vs LandR vs LandR.CS) [ ]
# 3. Make some stats on all replicates per type [ LATER ]
# 4. Organize !runMe function defineRun: add birds, caribou + commMet + hotspots to it! (complete run) [ ]

# VERSION3 folder: "1s4rtgI7N5iw5_WytZvhF2gi0Xbi9sNx8"
# V3 RSF: "1U3WJuNDtPWygzDJdxBgvUBWzVwiJoBch"
# V3 Birds: "1KHiBB-WgJeok_T4aEEar2aOtrjjz0FLY"  -- only CAWA BOCH BBWA OSFL ALFL BPWA [ OK ] 
# V3 CoOcc: "187B0wqjokdPB9PDPfF5icZ6SwF5cHZH2"
# V3 LandR: "1TMPcrqE2b9prq2hRe8jcXdLPXMWv_Enh"
# NoCC: "1MtRh2c55G1UOj8EZkaoFhPl7tJibc2Py"
# HERE --> Still need to manually save each plot in LandR folder! Then upload

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

# fireSense:
# The only things I need to run are:
# 1. Include in fireSense_NWT_DataPrep the function to create sim$MDC06 these. This needs to be redone every year! LCC "too" but cached [ DONE ]
# 2. Run every year: fireSense_NWT_DataPrep, "fireSense_FrequencyPredict", "fireSense_EscapePredict", "LBMR2LCC_DataPrep", "fireSense_NWT" [ REDEFINE definedRun, Push prepareClimateLayers to usefun]

