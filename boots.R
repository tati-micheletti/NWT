source('/mnt/data/Micheletti/NWT/posthocFunctions/bootstrapPercentChanges.R') # INTERNAL FUNCTIONS TO BE PUT IN USEFUN!!! [ FIX ]
shp <- "https://drive.google.com/open?id=1GA7hGslGEE1DGIMsD4Ou9duesS-eGbyZ"
cacheFolder <- "/mnt/data/Micheletti/NWT/cache/"
SpaDES.core::setPaths(cachePath = cacheFolder)
boot <- bootstrapPercentChanges(folder = "/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/", 
                                years = c(2001, 2100), sampleSize = 50, n = 80, shp = shp) # FIGURE OUT WHY IT TAKES SO LONG TO SIMULATE!
saveRDS(object = boot, file = file.path("/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/speciesChangeTables.rds"))
