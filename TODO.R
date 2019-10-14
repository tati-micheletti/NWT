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

mainFold <- "/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/"
fl <- usefun::grepMulti(x = list.files(mainFold, recursive = TRUE,
                                       full.names = TRUE), patterns = c("BOCH", "2001"))
lapply(fl, function(ras){
  googledrive::drive_upload(ras,
                            path = googledrive::as_id("1KHiBB-WgJeok_T4aEEar2aOtrjjz0FLY"))
})






# fireSense:
# The only things I need to run are:
# 1. Include in fireSense_NWT_DataPrep the function to create sim$MDC06 these. This needs to be redone every year! LCC "too" but cached [ DONE ]
# 2. Run every year: fireSense_NWT_DataPrep, "fireSense_FrequencyPredict", "fireSense_EscapePredict", "LBMR2LCC_DataPrep", "fireSense_NWT" [ REDEFINE definedRun, Push prepareClimateLayers to usefun]

# For the RIA:
# 1. We need to understand the fitting better i.e. `train`
# 
# 