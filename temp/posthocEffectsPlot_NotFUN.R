outputsFolder <- "~/projects/NWT/outputs/posthoc/summaryRasters"

# BOCH = file.path(outputsFolder, "BOCH_rastersSummaryTable.qs"),
# NOWA = file.path(outputsFolder, "NOWA_rastersSummaryTable.qs")

library(tictoc)
# source("~/projects/NWT/modules/posthocBirdsNWT/R/plotAbundanceThroughTime.R")
# abundTable <- plotAbundanceThroughTime(pixelsSummaries = list(COYE = file.path(outputsFolder, "COYE_rastersSummaryTable.qs")),
#                                        useFuture = FALSE,
#                                        overwrite = FALSE,
#                                        comparisons = list(climate = c("V6a", "V4"),
#                                                           vegetation = c("LandR.CS_", "LandR_"),
#                                                           fire = c("fS", "SCFM"),
#                                                           netEffect = c("LandR.CS_fS_V6a", "LandR_SCFM_V4")),
#                                        locations = 1:5,
#                                        years = c(seq(2011, 2091, by = 20), 2100),
#                                        pathOutputs = outputsFolder)

abundTableSep <- list(COYE = list(climate = list(individualPlotTable = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotTable_climate_COYE.qs",
                                              statsEachComparison = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotStats_climate_COYE.qs"),
                               vegetation = list(individualPlotTable = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotTable_vegetation_COYE.qs",
                                              statsEachComparison = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotStats_vegetation_COYE.qs"),
                               fire = list(individualPlotTable = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotTable_fire_COYE.qs",
                                              statsEachComparison = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotStats_fire_COYE.qs")))

abundTableNet <- list(COYE = list(netEffect = list(individualPlotTable = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotTable_netEffect_COYE.qs",
                                                statsEachComparison = "~/projects/NWT/outputs/posthoc/summaryRasters/timePlotStats_netEffect_COYE.qs")))
library(ggplot2)
source("~/projects/NWT/modules/posthocBirdsNWT/R/effectThroughTimePlot.R")
indvPlots <- effectThroughTimePlot(averageTimePlot = abundTableSep, 
                                   plotFilePath = "~/projects/NWT/outputs/posthoc/summaryRasters",
                                   typePlot = "individualEffects")
netPlots <- effectThroughTimePlot(averageTimePlot = abundTableNet,
                                  plotFilePath = "~/projects/NWT/outputs/posthoc/summaryRasters",
                                  typePlot = "netEffect")
