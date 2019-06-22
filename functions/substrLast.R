substrLast <- function(string, howManyCharacters){
  substr(string, nchar(string)-howManyCharacters+1, nchar(string))
}
