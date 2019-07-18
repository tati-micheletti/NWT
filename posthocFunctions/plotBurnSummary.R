plotBurnSummary <- function(folderData, typeSim){
  
parSetup <- par()
invisible(on.exit(par(parSetup)))
par(mfrow=c(2, 1))

folder <- folderData #"04JUL19" #18JUN19_CS_SCFM" #"08JUN19" #"29JUN19" 12JUL19 --> NoCS  12JUL19 --> CS
simul <- typeSim
folderPath <- paste0("/mnt/data/Micheletti/NWT/outputs/", folder)

# FIRE
burnSumm <- readRDS(file.path(folderPath, "burnSummary_year2100.rds"))
areaB <- burnSumm[, sumAB := sum(areaBurned), by = year]
areaB <- data.table(YEAR = areaB$year, AREABURNED = areaB$sumAB)
areaB <- unique(areaB)
tend <-lm(AREABURNED ~ YEAR, data = areaB)
require(stats)
coeff <- coefficients(tend)

# equation of the line : 
eq <- paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
# plot
plot(areaB, main = paste0(eq, " - ", simul))
abline(tend, col="blue")

# N fires
nFires <- burnSumm[, Nfires := length(N), by = year]
nFires <- data.table(YEAR = nFires$year, NUMBFIRES = nFires$Nfires)
nFires <- unique(nFires)
tendF <-lm(NUMBFIRES ~ YEAR, data = nFires)
require(stats)
coeffF <- coefficients(tendF)

# equation of the line : 
eqF <- paste0("y = ", round(coeffF[2],1), "*x ", round(coeffF[1],1))
# plot
plot(nFires, main = paste0(eqF, " - ", simul))
abline(tendF, col="red")

p <- recordPlot()

return(p)
}