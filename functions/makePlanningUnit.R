makePlanningUnit <- function(pU, years){
  # In the case of lack of a cost layer, we 
  # can derive the penalty by calculating the median of the
  # pU cost range using rasterToMatch (all values = 1).
  # The suggestion is that in the lack of a cost layer, 
  # the pU can be a raster with the size of planning units 
  # in hectares as the cost of each planning unit, resulting 
  # in roughly the same cost for all planning units. All
  # else being equal, a solution that includes more planning units will have a
  # higher total cost
  # DECIDED NOT TO remove water. SMALL WATER BODIES MIGHT BE IMPORTANT!

  resol <- res(pU)
pUInm2 <- prod(resol)
pUinHa <- 0.0001*pUInm2 # m2 to ha
newPU <- pU*pUinHa
pUList <- lapply(years, function(Y){
  stkY <- newPU
  return(stkY)
})
names(pUList) <- paste0("Year", years)
return(pUList)
}
