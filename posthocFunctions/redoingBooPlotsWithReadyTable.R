## Redoing caribou plots

library("Require")
Require("reproducible")
Require("data.table")

urlFinalTable <- "https://drive.google.com/file/d/1ClbbSIeos2n4LugtBaSa9B2tqt4x_MiY/view?usp=sharing"
pathOutputs <- "caribouPlots/"
booTable <- prepInputs(url = urlFinalTable,
                       destinationPath = pathOutputs,
                       fun = "data.table::fread")

source("posthocFunctions/plotCaribouPopGrowthMSSD.R")
plotCaribou <- plotCaribouPopGrowthMSSD(whichPolys = c("Bistcho", "Maxhamish", "Yates"),
                                      outputFolder = pathOutputs,
                                      caribouPopulationGrowthTable = booTable)
library("googledrive")
fl <- list.files(path = pathOutputs, pattern = "caribou_", full.names = TRUE)
lapply(fl, drive_upload, path = as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))


