# Partial effects plot

# lapply(c("bayestestR", "effectsize", "ggeffects", "parameters", "performance", "sjstats"), install.packages)
# install.packages("sjPlot")
# install.packages("gamlss")
mcsModel <- readRDS("/mnt/data/Micheletti/NWT/outputs/PAPER/LandR.CS_SCFM/run1/mcsModel_year2100.rds")
gcsModel <- readRDS("/mnt/data/Micheletti/NWT/outputs/PAPER/LandR.CS_SCFM/run1/gcsModel_year2100.rds")
cohortData <- readRDS("/mnt/data/Micheletti/NWT/outputs/PAPER/LandR.CS_SCFM/run1/cohortData_year2100.rds")

mcsModel <- mySimOut$mcsModel
gcsModel <- mySimOut$gcsModel
library(gamlss)
reproducible::Require(sjPlot)
sjPlot::plot_model(gcsModel, type = 'pred', term = "CMI")
sjPlot::plot_model(gcsModel, type = 'pred', term = 'ATA')
PSPmodelData <- gcsModel$data
gamlss::getPEF(mcsModel, term = "ATA", plot = TRUE)
gamlss::getPEF(mcsModel, term = "CMI", plot = TRUE)
gamlss::getPEF(mcsModel, term = "logAge", plot = TRUE)
cd <- mySimOut$cohortData[, .(age, B, mortality, aNPPAct, growthPred, mortPred)]
summary(cd[age > 1])

