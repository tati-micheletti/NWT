# 1. Make Edehzhie Raster for prepInputs [ OK ]
# 2. Make RSF ~ richness   [ OK  ] INSIDE AND OUT OF EDEHZHIE
# 3. Make Sam's table -- how many bird species change? And by how much? [  ] INSIDE AND OUT OF EDEHZHIE


# 9. Re-run for now 1x LandR+SCFM, LandR.CS+SCFM, LandR+fS
# 10. Make all plots that relate 2 or more types of run (SCFM vs fS vs LandR vs LandR.CS) [ ]
# 11. Make some stats on all replicates per type [ LATER ] 
# 12. Organize !runMe function defineRun: add birds, caribou + commMet + hotspots to it! (complete run) [  ]
 
# My e.mail: Concerns about the climate scenarios (more scenarios?) and fires (SCFM vs fireSense) birds
# 2 climates (GCM) and fireSense/SCFM
# I have decided I will run: 
#  - 8x SCFM+LandR
#  - 8x SCFM+LandR.CS
#  - 8x fireSense+LandR
#  - 8x fireSense+LandR.CS [ OK ]
# VERSION3 folder: "1s4rtgI7N5iw5_WytZvhF2gi0Xbi9sNx8"
# V3 RSF: "1U3WJuNDtPWygzDJdxBgvUBWzVwiJoBch"
# V3 Birds: "1KHiBB-WgJeok_T4aEEar2aOtrjjz0FLY"  -- only CAWA BOCH BBWA OSFL ALFL BPWA [ OK ]
# V3 CoOcc: "187B0wqjokdPB9PDPfF5icZ6SwF5cHZH2"
# V3 LandR: "1TMPcrqE2b9prq2hRe8jcXdLPXMWv_Enh"
# NoCC: "1MtRh2c55G1UOj8EZkaoFhPl7tJibc2Py"
# HERE --> Still need to manually save each plot in LandR folder! Then upload

mainFold <- "/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/"
fl <- usefun::grepMulti(x = list.files(mainFold, recursive = TRUE,
                                       full.names = TRUE), patterns = c("RUBL"))
lapply(fl, function(ras){
  googledrive::drive_upload(ras,
                            path = googledrive::as_id("1KHiBB-WgJeok_T4aEEar2aOtrjjz0FLY"))
})
