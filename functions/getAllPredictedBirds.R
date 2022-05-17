getAllPredictedBirds <- function(pathToBirdsRasters){
  allFls <- tools::file_path_sans_ext(list.files(pathToBirdsRasters, pattern = ".tif"))
  fls <- substrBoth(strng = unlist(strsplit(x = allFls, split = "predicted")), 
             howManyCharacters = 12, fromEnd = TRUE)
  fls <- grepMulti(x = fls, patterns = "Year")
  birds <- substrBoth(strng = fls, 
                      howManyCharacters = 4, fromEnd = FALSE)
  years <- substrBoth(strng = fls, 
                      howManyCharacters = 4, fromEnd = TRUE)
  return(list(birdSpecies = sort(unique(birds)),
              years = sort(unique(years))))
}
