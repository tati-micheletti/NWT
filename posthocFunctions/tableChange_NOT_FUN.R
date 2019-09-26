tableChange <- readRDS(file.path("/mnt/data/Micheletti/NWT/outputs/18JUL19/birdPredictionsV3_Fixed/speciesChangeTables.rds"))
tablePlot <- tableChange$tableIC
tablePlot$lower95 <- round(tablePlot$lower95, 0)
tablePlot$upper95 <- round(tablePlot$upper95, 0)
