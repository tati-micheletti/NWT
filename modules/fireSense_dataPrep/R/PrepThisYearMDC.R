PrepThisYearMDC <- function(sim)
{
  # WILL BE USED ONCE WE ARE FITTING. HOWEVER, NEEDS REVISION!!
browser()
  mod[["MDC"]] <- 
    raster::stack(
      Cache(
        lapply, 
        raster::unstack(sim[["MDC_BCR6_NWT_250m"]]),
        postProcess,
        rasterToMatch = mod$RTM,
        destinationPath = tempdir(),
        omitArgs = "destinationPath",
        maskWithRTM = TRUE,
        method = "bilinear",
        datatype = "FLT4S",
        filename2 = NULL
      )
    )
  
  return(invisible(sim))
}
