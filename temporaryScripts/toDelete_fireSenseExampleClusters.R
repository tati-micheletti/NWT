(logPath, cores, )

  objsNeeded <- list("landscape", 
                     "annualDTx1000", 
                     "nonAnnualDTx1000", 
                     "fireBufferedListDT", 
                     "historicalFires")
  
  if (!is.null(cores)) {
    message("Starting ", paste(paste(names(table(cores))), 
                               "x", table(cores), collapse = ", "), " clusters")
    logPath <- file.path(logPath, paste0("birdPrediction_log", 
                                         Sys.getpid()))
    message(crayon::blurred(paste0("Starting parallel prediction for ", 
                                   "boreal birds. Log: ", logPath)))
    if (is.numeric(cores))
      cores <- rep("localhost", cores)
    revtunnel <- if (all(cores == "localhost")) FALSE else TRUE
    st <- system.time(cl <- future::makeClusterPSOCK(unique(cores), 
                                                     revtunnel = revtunnel))
    clusterExport(cl, list("logPath"), envir = environment())
    parallel::clusterEvalQ(cl, {
      reproducible::checkPath(dirname(logPath), create = TRUE)
      Pkg <- c("PredictiveEcology/Require@development",
               "PredictiveEcology/reproducible@development",
               "PredictiveEcology/quickPlot@development",
               "PredictiveEcology/SpaDES.addins@development",
               "PredictiveEcology/SpaDES.tools@development",
               "PredictiveEcology/SpaDES.core@development",
               "PredictiveEcology/pemisc@development",
               "achubaty/amc@development",
               "PredictiveEcology/map@development",
               "PredictiveEcology/LandR@master",
               "PredictiveEcology/usefulFuns@development",
               "ianmseddy/LandR.CS@master",
               "PredictiveEcology/fireSenseUtils@iterative",
               "PredictiveEcology/SpaDES@development")
      pkg <- lapply(Pkg, function(p){
        capture.output(devtools::install_github(p))
      })
      if (sum(sapply(pkg, length)) != 0){
        message(crayon::bgWhite(paste0("At least one new package was installed. ",
                                       "Restarting R. Please re-run your code")))
        .rs.restartR()
      } else {
        message(crayon::green(paste0("No new packages were installed. ",
                                     "Your setup will continue.")))
      }
    })
    stopCluster(cl)
    st <- system.time(cl <- future::makeClusterPSOCK(cores, 
                                                     revtunnel = revtunnel, 
                                                     outfile = logPath))
    on.exit(stopCluster(cl))
    message("it took ", round(st[3], 2), "s to start ", 
            paste(paste(names(table(cores))), "x", table(cores), collapse = ", "), 
            " threads")
    clusterExport(cl, objsNeeded, envir = environment())
    parallel::clusterEvalQ(cl, {
      for (i in c("googledrive", 
                  "data.table", 
                  "raster", 
                  "gbm", 
                  "crayon", 
                  "plyr", 
                  "dplyr", 
                  "tati-micheletti/usefulFuns",
                  "future", 
                  "future.callr", 
                  "future.apply"))
        library(i, character.only = TRUE)
    })
  }
  else {
    list2env(mget(unlist(objsNeeded), envir = environment()), 
             envir = .GlobalEnv)
  }
  # Function Call for prediction
  DE <- Cache(DEoptimIterative, itermax = itermax, lower = lower, 
              upper = upper, control = do.call("DEoptim.control", control), 
              formula = formula, covMinMax = covMinMax, tests = c("SNLL_FS"), 
              maxFireSpread = maxFireSpread, objFunCoresInternal = objFunCoresInternal, 
              Nreps = Nreps, .verbose = .verbose, visualizeDEoptim = visualizeDEoptim, 
              cachePath = cachePath, iterStep = iterStep, omitArgs = c("verbose"))
  return(DE)
}