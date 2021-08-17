# Getting bird and covariates' data information

###### BIRDS
birdsList <- c("ALFL", "AMCR", "AMRE", "AMRO", "ATSP", "BAWW", "BBWA", "BBWO", 
  "BCCH", "BHCO", "BHVI", "BLPW", "BOCH", "BRBL", "BRCR", "BTNW", 
  "CAWA", "CHSP", "CORA", "COYE", "DEJU", "EAKI", "EAPH", "FOSP", 
  "GRAJ", "HETH", "HOLA", "LCSP", "LEFL", "LISP", "MAWA", "NOFL", 
  "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PISI", "PIWO", "PUFI", 
  "RBGR", "RBNU", "RCKI", "REVI", "RUGR", "RWBL", "SAVS", "SOSP", 
  "SWSP", "SWTH", "TEWA", "TRES", "WAVI", "WCSP", "WETA", "WEWP", 
  "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA", "YEWA", "YRWA"
)
source('~/projects/NWT/modules/birdsNWT/R/loadBirdModels.R')
birdModels <- loadBirdModels(birdsList = birdsList,
                             folderUrl = "https://drive.google.com/open?id=1DD2lfSsVEOfHoob3fKaTvqOjwVG0ZByQ",
                             pathData = "~/projects/NWT/modules/birdsNWT/data",
                             version = "6a")

birdModels4 <- loadBirdModels(birdsList = birdsList,
                             folderUrl = "https://drive.google.com/drive/u/0/folders/17RhA0KkmAJPpf4qss65I0F1wC77XmhzE",
                             pathData = "~/projects/NWT/modules/birdsNWT/data",
                             version = "4")

# Get all covariate values for each bird for each model
birdCovariatesTable <- rbindlist(lapply(birdsList, function(BIRD){
  # Birds CS
  V6a <- birdModels[[BIRD]]
  DT6 <- data.table(V6a[["contributions"]])
  DT6[, birdModel := "CS"]
  DT6[, species := BIRD]
  # Birds non-CS
  V4 <- birdModels4[[BIRD]]
  DT4 <- data.table(V4[["contributions"]])
  DT4[, birdModel := "non-CS"]
  DT4[, species := BIRD]
  DT <- rbind(DT6, DT4)
  return(DT)
}))
write.csv(birdCovariatesTable, file = file.path(getwd(), "birdCovariates.csv"))
library("googledrive")
drive_upload(file.path(getwd(), "birdCovariates.csv"), path = as_id("1wxQ6xtIg3A6OgVivuqCi4vbvr5uEQppd"))

# Table with information for the paper
birdInfo <- rbindlist(lapply(birdsList, function(sp){
  tb <- data.table(birdModels[[sp]][["gbm.call"]][["dataframe"]])
  DT <- data.table(species = sp,
                   birdRecords = tb[, sum(ABUND)])
}))

write.csv(birdInfo, file = file.path(getwd(), "birdRecords.csv"))
# drive_upload(media = file.path(getwd(), "birdRecords.csv"), as_id("17xCa7ZogxktoaTVuv7s4EIc2DVCuEq68")) # Already uploaded

DT <- data.table(birdModels[[1]][["gbm.call"]][["dataframe"]]) # It is the same dataframe for all species!
colsToKeep <- names(DT)[7:length(names(DT))]
DT <- DT[, ..colsToKeep]
otherInfo <- rbindlist(lapply(names(DT), function(variable){
  DF <- DT[, ..variable]
  DF <- DF[[1]]
  newDT <- data.table(variable = variable,
                      Min = round(min(as.numeric(DF), na.rm = TRUE), 2),
                      Max = round(max(as.numeric(DF), na.rm = TRUE), 2)
                      )
}))
write.csv(otherInfo, file = file.path(getwd(), "variableMinMax.csv"))
# drive_upload(media = file.path(getwd(), "variableMinMax.csv"), as_id("17xCa7ZogxktoaTVuv7s4EIc2DVCuEq68")) # Already uploaded
  