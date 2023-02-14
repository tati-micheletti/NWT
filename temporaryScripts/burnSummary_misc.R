wd <- "C:/Users/Tati/Google Drive/Postdoc PFC-UBC/WBI/fireSense Ignition and Escape Parameters"

# Escape
fls <- list.files(wd, full.names = TRUE, pattern = "escape")
allfs <- lapply(fls, FUN = qs::qread)
nms <- usefulFuns::substrBoth(strng = tools::file_path_sans_ext(fls), howManyCharacters = 2, fromEnd = TRUE)
names(allfs) <- nms
# class2 = deciduous
# class3 = conifer
tb <- rbindlist(lapply(nms, function(areaName){
  coeffs <- data.table(t(allfs[[areaName]][["fireSense_EscapeFitted"]][["coefficients"]]))
  names(coeffs) <- names(allfs[[areaName]][["fireSense_EscapeFitted"]][["coefficients"]])
  coeffs[, province := areaName]
  return(coeffs)
}))
names(tb)[names(tb) == "class2"] <- "deciduous"
names(tb)[names(tb) == "class3"] <- "conifer"

library("googledrive")
write.csv(tb, file = "C:/Users/Tati/Downloads/fireSense_EscapeParamTable.csv")
drive_upload("C:/Users/Tati/Downloads/fireSense_EscapeParamTable.csv", path = as_id("1xnVV65KOBTGdE4y0TP49Q1sfiGFMHVUF"))

# Ignition
fls <- list.files(wd, full.names = TRUE, pattern = "ignition")
allfs <- lapply(fls, FUN = qs::qread)
nms <- usefulFuns::substrBoth(strng = tools::file_path_sans_ext(fls), howManyCharacters = 2, fromEnd = TRUE)
names(allfs) <- nms
# class2 = deciduous
# class3 = conifer
tb <- rbindlist(lapply(nms, function(areaName){
  coeffs <- data.table(t(allfs[[areaName]][["fireSense_IgnitionFitted"]][["coef"]]))
  names(coeffs) <- names(allfs[[areaName]][["fireSense_IgnitionFitted"]][["coef"]])
  coeffs[, province := areaName]
  return(coeffs)
}), fill=TRUE)
names(tb)[names(tb) == "class2"] <- "deciduous"
names(tb)[names(tb) == "class3"] <- "conifer"

library("googledrive")
write.csv(tb, file = "C:/Users/Tati/Downloads/fireSense_IgnitionParamTable.csv")
drive_upload("C:/Users/Tati/Downloads/fireSense_IgnitionParamTable.csv", path = as_id("1xnVV65KOBTGdE4y0TP49Q1sfiGFMHVUF"))


# CARIBOU
library("Require")
Require("reproducible")
Require("qs")

distTable <- prepInputs(targetFile = "disturbanceTable_3Scenarios.qs",
                                  url = "https://drive.google.com/file/d/1yBdZnHR_EoRoJ5TTY76Kq63y2oeJ_Fkm/view?usp=sharing",
                                  destinationPath = tempdir(),
                                  fun = "qs::qread")

library("Require")
Require("reproducible")
Require("qs")
meanRSFpol <- prepInputs(targetFile = "meanRSFperPolygon.qs",
                        url = "https://drive.google.com/file/d/1t_ZFHFjXpqXLaUye7FAmBal3iy4GT7GC/view?usp=sharing",
                        destinationPath = tempdir(),
                        fun = "qs::qread")

library("Require")
Require("reproducible")
Require("qs")

burnSummaryTable <- prepInputs(targetFile = "burnSummaryTableCanESM2_INM-CM4_CCSM4.qs",
                         url = "https://drive.google.com/file/d/1mpq75Rcxnj1B1FUEdt_ogY1klOMyDLiG/view?usp=sharing",
                         destinationPath = tempdir(),
                         fun = "qs::qread")
