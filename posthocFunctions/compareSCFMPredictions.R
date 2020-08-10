compareSCFMPredictions <- function(polyList, simList, scenario, colNameFireSize = "SIZE_HA") {
  out <- lapply(polyList, FUN = function(x, sim = simList) {
    regime <- sim$scfmRegimePars[[x]]
    driver <- sim$scfmDriverPars[[x]]
    landscapeAttr <- sim$landscapeAttr[[x]]
    frpl <- sim$fireRegimePolys$PolyID
    sim$firePoints$PolyID <- sp::over(sim$firePoints, sim$fireRegimePolys) #gives studyArea row name to point
    sim$firePoints$PolyID <- sim$firePoints$PolyID$PolyID
    firePoints <- sim$firePoints[sim$firePoints$PolyID == x,]
    hist_MAAB <- sum(firePoints[[colNameFireSize]][firePoints[[colNameFireSize]] > landscapeAttr$cellSize])/
      (landscapeAttr$burnyArea*(sim@params$scfmRegime$fireEpoch[2] - sim@params$scfmRegime$fireEpoch[1] + 1)) * 100
    #This is a long way of saying, sum of fires/ (flammable landscape * fire epoch )
    #hist_mfs will be NaN if there were no fires larger than one pixel
    pSpread <- driver$pSpread 
    burnSum <- sim$burnSummary[sim$burnSummary$polyID == x,]
    burnSum$N <- as.numeric(burnSum$N)
    burnSum$areaBurned <- as.numeric(burnSum$areaBurned)
    burnSum <- burnSum[burnSum$N > 1]
    mod_MAAB <- sum(burnSum$areaBurned)/(landscapeAttr$burnyArea * (SpaDES.core::times(sim)$end - SpaDES.core::times(sim)$start)) * 100
    
    pred <- data.frame("PolyId" = x, #Polygon ID
                       "prdMeanSize" = regime$xBar, #The predicted (empirical) mean size of fires
                       "modMeanSize" = mean(burnSum$areaBurned), #The modeled mean size of fires
                       "pSpread" = pSpread, # The spread probability estimated from the SCAM model
                       "hist_MAAB" = hist_MAAB,#The empirical mean annual area burned (from NFDB 1970-2000)
                       "mod_MAAB" = mod_MAAB,
                       "scenario" = scenario) #The modelled mean annual area burned
    return(pred)
  })
  return(data.table::rbindlist(out))
}