rasFolder <- "C:/Users/Tati/Google Drive/Postdoc PFC-UBC/NWT 2019/G&C_Contract/priorityPlacesPreliminaryResults/rawNoWeight"

# Planning Unit :: cost biodiversity
planningUnit_noW <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/planningUnit_year2100.rds"))
planningUnit_W <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/planningUnit_year2100.rds"))
writeRaster(planningUnit_noW[["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/planningUnit_noW_2011.tif"), format = "GTiff")
writeRaster(planningUnit_noW[["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/planningUnit_noW_2100.tif"), format = "GTiff")

writeRaster(planningUnit_W[["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/planningUnit_W_2011.tif"), format = "GTiff")
writeRaster(planningUnit_W[["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/planningUnit_W_2100.tif"), format = "GTiff")


# Results :: Not working as outputs is acting up
# p_15_noW <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/target15_noWeight/priorityAreas_year2100.rds"))
# names(p_15_noW[["Year2011"]][[1]]) <-"p_15_noW_2011"
# names(p_15_noW[["Year2100"]][[1]]) <-"p_15_noW_2100"
# p_15_W <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/target15_weights15105/priorityAreas_year2100.rds"))
# names(p_15_noW) <-"p_15_noW"
# 
# p_30_noW <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/target30_noWeight/priorityAreas_year2100.rds"))
# names(p_30_noW) <-"p_30_noW"
# p_30_W <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/target30_weights15105/priorityAreas_year2100.rds"))
# names(p_30_W) <-"p_30_W"
# 
# p_60_noW <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/target60_noWeight/priorityAreas_year2100.rds"))
# names(p_60_noW) <-"p_60_noW"
# p_60_W <- readRDS(file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/target60_weights15105/priorityAreas_year2100.rds"))
# names(p_60_W) <-"p_60_W"

writeRaster(p_15_noW[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_15_noW_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_15_noW[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_15_noW_2100.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_15_W[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_15_W_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_15_W[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_15_W_2100.tif"), format = "GTiff", 
            overwrite = TRUE)

writeRaster(p_30_noW[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_30_noW_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_30_noW[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_30_noW_2100.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_30_W[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_30_W_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_30_W[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_30_W_2100.tif"), format = "GTiff", 
            overwrite = TRUE)

writeRaster(p_60_noW[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_60_noW_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_60_noW[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/noWeights/p_60_noW_2100.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_60_W[["priorityAreas"]][["Year2011"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_60_W_2011.tif"), format = "GTiff", 
            overwrite = TRUE)
writeRaster(p_60_W[["priorityAreas"]][["Year2100"]][[1]], 
            file.path(getwd(), "outputs/GnCReport_VERSION3/weights15105/p_60_W_2100.tif"), format = "GTiff", 
            overwrite = TRUE)
library(usefun)
fl <- grepMulti(list.files("outputs/GnCReport_VERSION3/", full.names = TRUE, recursive = TRUE), 
                        patterns = c("p", ".tif"))
lapply(fl, googledrive::drive_upload, path = as_id("1rad2FzocV_-4kN1nRrs9C4TGlyoZ_9-m"))






