table <- readRDS("/mnt/data/Micheletti/NWT/outputs/18JUN19_CS_SCFM/caribouPopGrowth.rds")

# Delete columns minModelParam maxModelParam sdModelParam modelParam
colsDel <- c("minModelParam", "maxModelParam", "sdModelParam", "modelParam")
table[, (colsDel) := NULL]
table[, mortF := (1-adultFemaleSurv)*100]

res <- data.table::rbindlist(apply(X = table, MARGIN = 1, FUN = function(row){
  meanLambda <- round((100-as.numeric(row["mortF"]))/(100-(as.numeric(row["Rec"])/2)), 2)
  minLambda <- round((100-as.numeric(row["mortF"]))/(100-(as.numeric(row["minRec"])/2)), 2)
  maxLambda <- round((100-as.numeric(row["mortF"]))/(100-(as.numeric(row["maxRec"])/2)), 2)
  df <- data.table(modelParam = meanLambda,
                   minModelParam = minLambda,
                   maxModelParam = maxLambda)
  return(df)
})
)

tableNew <- cbind(table, res)

plotCaribou <- plotCaribou2(startTime = 2011,
                            currentTime = 2100,
                            endTime = 2100,
                            predictedCaribou = tableNew,
                            yearSimulationStarts = 2001)


pth <- file.path(reproducible::checkPath(file.path(getwd(), "outputs"), create = TRUE), 
              paste0("cariboucaribouArea1_", 
                     toupper(format(Sys.time(), "%d%b%y")),".png"))
googledrive::drive_upload(pth,
                          path = googledrive::as_id("130r_99kfhLIJE0Mz1FjyhiZnfM7s7-v6")) 
