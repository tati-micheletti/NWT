showAvailableBirdSpecies <- function(){
  reproducible::Require("data.table")
  birdies <- c("REVI", "HETH", "RCKI", "HAFL", "WIWR", "GRCA", "RBNU", "WIWA", 
    "GRAJ", "RBGR", "WEWP", "GCKI", "PUFI", "WETA", "FOSP", "PISI", 
    "WCSP", "EVGR", "WBNU", "PIGR", "BTNW", "EAPH", "PHVI", "WAVI", 
    "BRTH", "EAKI", "BRCR", "PAWA", "VESP", "DEJU", "BRBL", "OVEN", 
    "VEER", "CSWA", "BOCH", "VATH", "OSFL", "BLPW", "COYE", "TRES", 
    "BLJA", "OCWA", "TOWA", "TEWA", "BLBW", "CORA", "NOWA", "SWTH", 
    "BHVI", "CONW", "MOWA", "SWSP", "BHCO", "COGR", "MAWA", "CMWA", 
    "SOSP", "BCCH", "LISP", "YRWA", "CHSP", "SEWR", "BBWA", "LEFL", 
    "YBFL", "CEDW", "SAVS", "BAWW", "LCSP", "WWCR", "CCSP", "RWBL", 
    "BAOR", "HOWR", "WTSP", "CAWA", "RUBL", "AMRO", "HOLA", "AMRE", 
    "AMGO", "AMCR", "ALFL")
  return(data.table(BirdSpecies = birdies))
}