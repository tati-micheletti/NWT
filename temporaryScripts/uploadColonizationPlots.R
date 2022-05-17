# UPLOAD BIRDS
library(googledrive)
colonizationCS <- usefulFuns::grepMulti(x = list.files(path = "outputs/posthoc/colonization", full.names = TRUE),
                                      patterns = "climateSensitive")
colonizationNCS <- usefulFuns::grepMulti(x = list.files(path = "outputs/posthoc/colonization", full.names = TRUE),
                                        patterns = "nonclimateSensitive")
colonization <- c(colonizationCS, colonizationNCS)
lapply(X = colonization, drive_upload, path = as_id("1BqKDbLz4K7gpxJWQ68dQNqV_PaHp6EpX"))
averageTimePlot_year1 <- readRDS("~/projects/NWT/outputs/posthoc/averageTimePlot_year1.rds")
sp <- names(averageTimePlot_year1)
timePlot <- unlist(lapply(averageTimePlot_year1, `[[`, 1))
averageTP <- unlist(lapply(sp, function(SP){
  list.files(path = "~/projects/NWT/outputs/posthoc/effectsRasters", pattern = paste0(SP, "PerPolygon.png"), 
                       full.names = TRUE)
  }))
TP <- c(timePlot, averageTP)
lapply(X = TP, drive_upload, path = as_id("1BOk6fNM9UYPKYd-0za-sjZPdoeEIWKcj"))
summaryTable <- "outputs/posthoc/summaryRasters/rastersSummaryTable.qs"
drive_upload(media = summaryTable, path = as_id("1q7vXFH3aZLAI-yJP-FeJaCMp6wGWL4uc"))
# deltaRas <- list.files(path = "outputs/posthoc/", pattern = "delta.tif", full.names = TRUE) # DO IT LATER!
# lapply(X = deltaRas, drive_upload, path = as_id("1S4gbWFaHnDCY73QyhM_MJwEey2MjZMqs"))
# effects <- list.files(path = "outputs/posthoc/effectsRasters/", pattern = "mean", full.names = TRUE) # DO IT LATER!
# effects2 <- list.files(path = "outputs/posthoc/effectsRasters/", pattern = "sd", full.names = TRUE) # DO IT LATER!
# lapply(X = c(effects, effects2), drive_upload, path = as_id("1H6Z-OIpBV3BO7al9LanLTuQkGVT4TS01"))

