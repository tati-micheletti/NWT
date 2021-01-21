getBirdsAvailable <- function(birdModelVersion = "8"){
  Require::Require("data.table")
  Require::Require("googledrive")
  Require::Require("usefulFuns")
  urlTable <- data.table(version = c("8"),
                         id = c("1AoScxKtKrVbStk9LldXGGjna9f9iBbfd"))
  allFiles <- drive_ls(path = as_id(urlTable[version == birdModelVersion, id]))
  # Get only the models:
  modFiles <- allFiles[["name"]][grep(allFiles[["name"]], pattern = paste0("brt", 
                                                                           birdModelVersion))]
  allBirds <- usefulFuns::substrBoth(modFiles, howManyCharacters = 4, fromEnd = FALSE)
  return(sort(allBirds))
}
