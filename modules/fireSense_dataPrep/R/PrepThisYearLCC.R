PrepThisYearLCC <- function(currentTime,
                            firePolys,
                            rstLCC,
                            train,
                            res,
                            pp_lcc = NULL){

  # LCC05 with incremental disturbances
  fireYearsToSelect <- paste0("Year", (currentTime-15):currentTime)
  subsetFires <- firePolys[fireYearsToSelect]
  fires_this_year <- do.call(rbind, subsetFires)

  if (nrow(fires_this_year) > 0) {
    # Setting the burned pixels of LCC05 to category 34 (recent burns)
    fires_this_yearSF <- st_as_sf(fires_this_year)
    rasfires_this_year <- fasterize::fasterize(sf = fires_this_yearSF, raster = rstLCC)
    rstLCC[rasfires_this_year == 1] <- 34  # LCC05 code for recent burns
  }
  
  if (train){
    n_lcc <- 39
    pp_lcc <- lapply(1:n_lcc, function(cl_i){
          calc_prop_lcc <- function(x, cl = cl_i, na.rm = TRUE){
            if (anyNA(x)) return(NA)
            sum(x == cl, na.rm = na.rm) / (agg_fact ** 2)
          }
          
          col_name <- paste0("cl", cl_i)
          agg_fact <- res / xres(rstLCC)
          
          tibble(
            !!col_name := aggregate(
              rstLCC,
              fact = agg_fact,
              fun = calc_prop_lcc
            )[]
          )
        }
      ) %>% bind_cols %>% filter_at(2, all_vars(!is.na(.)))
  }
  
 return(list(rstLCC = rstLCC, pp_lcc = pp_lcc))
}
