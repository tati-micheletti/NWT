classifyWetlands <- function(LCC = P(sim)$baseLayer,
                             wetLayerInput = sim$rasterDUCKS,
                             pathData = dataPath(sim),
                             studyArea = sim$studyArea){
  
  Require("LandR")

  listLCC <- lapply(X = LCC, FUN = function(yearLCC){
  if (!any(yearLCC %in% c("LCC10", "LCC05"))) message(paste0("Currently, only LCC10 and LCC05 are available.\n",
                                                        "LCC05 will be returned unless LCC10 was specified."))
    year <- if (grepl(x = yearLCC, pattern = "10")) 2010 else 2005

    rasLCC <- LandR::prepInputsLCC(year = year, destinationPath = pathData, 
                         studyArea = studyArea, filename2 = yearLCC)
    return(rasLCC)
  })
  browser()
  
  # DUCS Layer has 1 = Water, 2 = Wetland, 3+ = Upland
  
}