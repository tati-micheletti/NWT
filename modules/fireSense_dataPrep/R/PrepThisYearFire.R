PrepThisYearFire <- function(sim)
{
  # WILL BE USED ONCE WE ARE FITTING. HOWEVER, NEEDS REVISION!!
  
  browser()
  
  currentYear <- time(sim, "year")
  
  NFDB_PT <- sim[["NFDB_PT"]] %>%
    
    # Filter fire data for the current year
    dplyr::filter(YEAR == currentYear) %>%
    
    # Drop columns containing info we don't need
    dplyr::select(LATITUDE, LONGITUDE, YEAR, SIZE_HA, CAUSE) %>%
    
    # Keep only lightning fires
    dplyr::filter(CAUSE == "L")
  
  mod[["fires"]] <- st_set_geometry(
    mutate(
      filter(
        st_join(mod[["RTM_VT"]], NFDB_PT),
        !is.na(YEAR)
      ),
      YEAR = currentYear
    ), 
    NULL
  )
  
  invisible(sim)
}
