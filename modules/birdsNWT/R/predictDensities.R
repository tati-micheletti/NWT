predictDensities <- function(birdSpecies = sim$birdsList,
                             uplandsRaster = sim$uplandsRaster,
                             successionLayers = sim$successionLayers,
                             staticLayers = sim$staticLayers,
                             currentTime = time(sim),
                             modelList = sim$birdModels,
                             overwritePredictions = P(sim)$overwritePredictions,
                             pathData = dataPath(sim),
                             useParallel = FALSE,
                             nCores = 1) {

  if (useParallel == FALSE){
    
    predictionPerSpecies <- lapply(X = birdSpecies, FUN = corePrediction,
                                    uplandsRaster = uplandsRaster,
                                    successionLayers = successionLayers,
                                    staticLayers = staticLayers,
                                    currentTime = currentTime,
                                    modelList = modelList,
                                    overwritePredictions = overwritePredictions,
                                    pathData = pathData)
    
  } else {
    message(crayon::red(paste0("Paralellizing for:\n", paste(birdSpecies, collapse = "\n"), 
                               "\nMessages will be suppressed until done.")))
    if (nCores == "auto") {
      nCores <- data.table::getDTthreads()*0.9
    }
    cl <- if (.Platform$OS.type != "windows") {
      parallel::makeForkCluster(nCores, outfile = file.path(pathData, "logParallelBirdPrediction"))
      on.exit(try(parallel::stopCluster(cl), silent = TRUE))
    } else {
      NULL
    }
    predictionPerSpecies <-  pemisc::Map2(cl = cl, #parallel::clusterApplyLB(cl = cl, x = birdSpecies, fun = corePrediction,
                                          corePrediction, x = birdSpecies,
                                          MoreArgs = list(
                                                      uplandsRaster = uplandsRaster,
                                                      successionLayers = successionLayers,
                                                      staticLayers = staticLayers,
                                                      currentTime = currentTime,
                                                      modelList = modelList,
                                                      overwritePredictions = overwritePredictions,
                                                      pathData = pathData))
    
  }

  names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}
