# Investigating bird models

load("/mnt/data/Micheletti/NWT/modules/birdsNWT/data/models/OSFLbrt2.R")
library("gbm")
library("data.table")
library("dismo")
# dt <- data.table(brt1$contributions)
# order(dt, na.last = TRUE, decreasing = TRUE)
# top3 <- as.character(dt[, var])
dismo::gbm.plot(brt1, n.plots = 6)
gbm::plot.gbm(brt1, i = "Structure_Stand_Age_v1")

# library("ggplot2")
# p <- ggplot(data = dt) +
#   geom_bar(aes(x = var, y = rel.inf))

