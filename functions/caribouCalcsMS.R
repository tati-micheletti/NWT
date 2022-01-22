library("googledrive")
foldID <- as_id("11H-Chg-EyO6zQ-KwggiLzN6D54sjnRp4")
filePath <- "~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/caribouPlots/populationGrowthTable.csv"
drive_upload(media = filePath, path = foldID)

library("data.table")
dt <- fread(filePath)

sDT <- unique(dt[Year == 2011 & 
                   femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National" & 
                   Replicate == "run1", 
                 c("Polygon", "average_femaleSurvival", "stdErr_femaleSurvival", "climateModel")])

sDT[, average_femaleSurvival := round(average_femaleSurvival, 2)] 
sDT[, stdErr_femaleSurvival := round(stdErr_femaleSurvival, 2)] 
setkey(sDT, "average_femaleSurvival", "Polygon")
sDT

sDT2 <- unique(dt[Year == 2011 & 
                   femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National" & 
                   Replicate == "run1", 
                 c("Polygon", "average_recruitment", "stdErr_recruitment", "climateModel")])

sDT2[, average_recruitment := round(average_recruitment, 2)] 
sDT2[, stdErr_recruitment := round(stdErr_recruitment, 2)] 
setkey(sDT2, "average_recruitment", "Polygon")
sDT2

sDT3 <- unique(dt[Year == 2100 & 
                   femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National" & 
                   Replicate == "run1", 
                 c("Polygon", "average_femaleSurvival", "stdErr_femaleSurvival", "climateModel")])

sDT3[, average_femaleSurvival := round(average_femaleSurvival, 2)] 
sDT3[, stdErr_femaleSurvival := round(stdErr_femaleSurvival, 2)] 
setkey(sDT3, "average_femaleSurvival", "Polygon")
sDT3

sDT4 <- unique(dt[Year == 2100 & 
                    femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National" & 
                    Replicate == "run1", 
                  c("Polygon", "average_recruitment", "stdErr_recruitment", "climateModel")])

sDT4[, average_recruitment := round(average_recruitment, 2)] 
sDT4[, stdErr_recruitment := round(stdErr_recruitment, 2)] 
setkey(sDT4, "average_recruitment", "Polygon")
sDT4

lDT <- unique(dt[Year %in% c(2011, 2100) & 
                   femSurvMod_recrMod == "Johnson_M4_National::Johnson_M4_National",
                 c("Polygon", "averageannualLambda", "climateModel", "Year")])
setkey(lDT, "Polygon", "climateModel")
lDT <- unique(lDT)
lDT <- lDT[, c("Polygon", "averageannualLambda", "Year")]

lDTmax <- dcast(data = lDT, formula = Polygon ~ Year, fun.aggregate = max, value.var = "averageannualLambda")
lDTmin <- dcast(data = lDT, formula = Polygon ~ Year, fun.aggregate = min, value.var = "averageannualLambda")
names(lDTmax) <- names(lDTmin) <- c("Polygon", "Y2011", "Y2100")

lDTmax[, diff := Y2100-Y2011]
lDTmin[, diff := Y2100-Y2011]


