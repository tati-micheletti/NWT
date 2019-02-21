predictDensities <- function(birdSpecies = sim$birdsList,
                             successionLayers = sim$successionLayers,
                             staticLayers = sim$staticLayers,
                             currentTime = time(sim),
                             modelList = sim$birdModels,
                             overwritePredictions = FALSE,
                             pathData = dataPath(sim),
                             useParallel = FALSE,
                             nCores = 1) {

  if (useParallel == FALSE){
    
    predictionPerSpecies <- lapply(X = birdSpecies, FUN = corePrediction, 
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
    cl <- parallel::makeForkCluster(nCores, outfile = file.path(pathData, "logParallelBirdPrediction"))
    predictionPerSpecies <-  parallel::clusterApplyLB(cl = cl, x = birdSpecies, fun = corePrediction,
                                                      successionLayers = successionLayers,
                                                      staticLayers = staticLayers,
                                                      currentTime = currentTime,
                                                      modelList = modelList,
                                                      overwritePredictions = overwritePredictions,
                                                      pathData = pathData)
    parallel::stopCluster(cl)
  }

  names(predictionPerSpecies) <- birdSpecies
  return(predictionPerSpecies)
}
