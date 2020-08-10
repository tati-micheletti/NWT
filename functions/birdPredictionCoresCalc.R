birdPredictionCoresCalc <- function(birdSpecies = NULL,
                                    ipEnd = c(97, 189, 213, 220, 58, 68),
                                    availableCores = c(10, rep(52, times = 5)),
                                    availableRAM = c(150, rep(500, times = 4), 920),
                                    sizeGbEachProcess = 31,
                                    localHostEndIp = 68,
                                    returnNumber = TRUE){ # if TRUE, return number of cores, FALSE
  # returns IP of workers
  if (is.null(birdSpecies))
    birdSpecies <- c("ALFL", "AMCR", "AMGO", "AMRE", "AMRO", "ATSP", "BAOR", "BAWW",
                     "BBWA", "BBWO", "BCCH", "BHCO", "BHVI", "BLBW", "BLJA", "BLPW",
                     "BOCH", "BRBL", "BRCR", "BRTH", "BTNW", "CAWA", "CCSP", "CEDW",
                     "CHSP", "CMWA", "COGR", "CONW", "CORA", "COYE", "CSWA", "DEJU",
                     "EAKI", "EAPH", "EVGR", "FOSP", "GCKI", "GCTH", "GRAJ", "GRCA",
                     "HAFL", "HETH", "HOLA", "HOWR", "LCSP", "LEFL", "LISP", "MAWA",
                     "MOWA", "NOFL", "NOWA", "OCWA", "OSFL", "OVEN", "PAWA", "PHVI",
                     "PISI", "PIWO", "PUFI", "RBGR", "RBNU", "RCKI", "REVI", "RUBL",
                     "RUGR", "RWBL", "SAVS", "SEWR", "SOSP", "SWSP", "SWTH", "TEWA",
                     "TOWA", "TRES", "VATH", "VEER", "VESP", "WAVI", "WBNU", "WCSP",
                     "WETA", "WEWP", "WIWA", "WIWR", "WTSP", "WWCR", "YBFL", "YBSA",
                     "YEWA", "YRWA")
  
tryCatch({
  nGroups <- 1
      cores <- makeIpsForClusters(ipEnd = ipEnd,
                                  availableCores = availableCores,
                                  availableRAM = availableRAM,
                                  nProcess = length(birdSpecies), # Species or parameters
                                  baseProcess = "RAM", # can also be "RAM" when that is the limiting factor!
                                  sizeGbEachProcess = sizeGbEachProcess,
                                  module = "birdsNWT",
                                  localHostEndIp = localHostEndIp)
      return(list(cores = ifelse(returnNumber, length(cores), cores),
                            birdSpecies = list("Group1" = birdSpecies)))
    }, error = function(e) {
      divideBirdsSp <- TRUE
      while (divideBirdsSp){
        print(paste0("Not enough RAM for ", nGroups, 
                     " group(s). Trying ", nGroups+1, " groups..."))
        nGroups <- nGroups + 1
          # Divide the groups based on nGroups
          birdSpeciesG <- chunk(toDivide = birdSpecies, nGroups = nGroups)
          names(birdSpeciesG) <- paste0("Group", 1:nGroups)
          largest <- max(sapply(birdSpeciesG, length))
          tryCatch({
            cores <- makeIpsForClusters(ipEnd = ipEnd,
                                        availableCores = availableCores,
                                        availableRAM = availableRAM,
                                        nProcess = largest, # Species or parameters
                                        baseProcess = "RAM", # can also be "RAM" when that is the limiting factor!
                                        sizeGbEachProcess = sizeGbEachProcess,
                                        module = "birdsNWT",
                                        localHostEndIp = localHostEndIp)
            return(list(cores = ifelse(returnNumber, length(cores), cores),  
                        birdSpecies = birdSpeciesG))
        }, error = function(e){
          return(NULL)
        })
      }
      # return(list(cores = cores,
      #             birdSpecies = birdSpecies))
    }
)
}

chunk <- function(toDivide, nGroups){
  dd <- split(toDivide, factor(sort(rank(toDivide)%%nGroups)))
  return(dd)
}
