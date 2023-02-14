## Redoing caribou plots

library("Require")
Require("reproducible")
Require("data.table")
Require("ggplot2")

urlFinalTable <- "https://drive.google.com/file/d/1ClbbSIeos2n4LugtBaSa9B2tqt4x_MiY/view?usp=sharing"
pathOutputs <- "caribouPlots/"
booTable <- prepInputs(url = urlFinalTable,
                       destinationPath = pathOutputs,
                       fun = "data.table::fread")
# Clean up table
booTable <- booTable[, V1 := NULL, ]
booTable <- unique(booTable)

# Checking normality of the results
histPlots <- ggplot2::ggplot(data = booTable[femSurvMod_recrMod == "Johnson_M1_National::Johnson_M4_National" &
                                               Year != 2011], 
                             mapping = aes(x = annualLambda)) +
  geom_histogram(binwidth = 0.001) +
  # geom_density(alpha = .3, fill = "#FF6666") +
  facet_grid(Polygon ~ Year, scales = 'free_x')
histPlots


source("posthocFunctions/plotCaribouPopGrowthMSSD.R")
plotCaribou <- plotCaribouPopGrowthMSSD(whichPolys = c("Bistcho", "Maxhamish", "Yates"),
                                      outputFolder = pathOutputs,
                                      caribouPopulationGrowthTable = booTable)
library("googledrive")
fl <- list.files(path = pathOutputs, pattern = "caribou_", full.names = TRUE)
lapply(fl, drive_upload, path = as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4"))
