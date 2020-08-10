makeIpsForClusters <- function(ipStart = "10.20.0.",
                              ipEnd = c(68, 97, 189, 213, 220, 58, 106, 184, 217),
                              availableCores = c(50, 50, 50, 50, 50, 50, 23, 23, 23),
                              availableRAM = c(950, 500, 500, 500, 500, 500, 245, 245, 245),
                              nProcess = 8,
                              baseProcess = "cores", # can also be "RAM" when that is the limiting factor!
                              internalProcesses = 10,
                              sizeGbEachProcess = 35,
                              module = NULL,
                              localHostEndIp = 68){ # lower=8 is for fireSense 2p, fireSense 3p needs lower=9
  
  machines <- data.frame(
    ipEnd = ipEnd,
    availableCores = availableCores,
    availableRAM = availableRAM
  )
  
  if (any(is.null(module),
          !module %in% c("fireSense","birdsNWT"))){
    NP <- ifelse(baseProcess == "cores",
           nProcess * internalProcesses,
           nProcess * sizeGbEachProcess)
    warning(paste0("module is NULL", 
                   "Defaulting needed process to ",
                   NP, " ", baseProcess),
            immediate. = TRUE)
  } else {
    if (module == "fireSense"){
      baseProcess <- "cores"
      NP <- ifelse(baseProcess == "cores",
                   nProcess * internalProcesses,
                   nProcess * sizeGbEachProcess)
    } else {
      if (module == "birdsNWT")
      baseProcess <- "RAM"
      NP <- ifelse(baseProcess == "cores",
                   nProcess * internalProcesses,
                   nProcess * sizeGbEachProcess)
    }
  }

  IPs <- makeIps(machines = machines, 
                 ipStart = ipStart, 
                 NP = NP,
                 proc = baseProcess,
                 nProcess = nProcess,
                 sizeGbEachProcess = sizeGbEachProcess)
  IPs[grep(localHostEndIp, IPs)] <- "localhost"
  
  (table(IPs))
  (length(IPs))
  
  return(IPs)
}

  makeIps <- function(machines, 
                      ipStart, 
                      NP,
                      proc, 
                      nProcess,
                      sizeGbEachProcess) {
    if (proc == "cores"){
      availableResource <- "availableCores"
      if (sum(machines$availableCores) < NP)
        stop("Not enough cores")
    } else {
      availableResource <- "availableRAM"
      if (sum(machines$availableRAM) < NP)
        stop("Not enough RAM")
    }
  
    if (proc == "RAM"){
      # find the number of cores for the amount of 
      # RAM needed
      ncoresVector <- numeric()
      ncoresVector <- unlist(lapply(seq_along(machines$ipEnd), function(machineIndex){
        RAM <- machines$availableRAM[machineIndex]
        CORES <- machines$availableCores[machineIndex]
        newCore <- optimalClusterNumGeneralized(memRequiredMB = sizeGbEachProcess*1000,
                                                maxNumClusters = CORES,
                                                NumCoresAvailable = CORES,
                                                availMem = RAM*1000)
        ncoresVector <- c(ncoresVector, newCore)
      }))
      machines$coresByRAM <- ncoresVector
      availableResource <- "coresByRAM"
      NP <- min(sum(machines$coresByRAM), nProcess)
    }

    ipsEnd <- rep(machines$ipEnd,
                  pmax(machines[[availableResource]], 
                       ceiling(machines[[availableResource]]/(sum(machines[[availableResource]])/NP))))

    ips <- paste0(ipStart, ipsEnd)
    i <- 0
    while(length(ips) > NP) {
      i <- i + 1
      i <- (i - 1) %% NROW(machines) + 1
      j <- i+1
      ips <- ips[-which(endsWith(ips, suffix = as.character(machines$ipEnd[i])))[1]]
    }
    return(sort(ips))
  }
  
  optimalClusterNumGeneralized <- function(memRequiredMB = 500, 
                                           maxNumClusters = parallel::detectCores(),
                                           NumCoresAvailable = parallel::detectCores(),
                                           availMem = pemisc::availableMemory()/1e+06){
    if (maxNumClusters > 0) {
      if (!is.null(availMem)) {
        numClusters <- floor(min(NumCoresAvailable, 
                                 availMem/memRequiredMB))
      }
      else {
        message("Unable to estimate available memory. Returning 1 cluster.")
        numClusters <- 1
      }
      numClusters <- min(maxNumClusters, numClusters, NumCoresAvailable)
    }
    else {
      numClusters <- 1
    }
    return(numClusters)
  }
  