
# Checking climate layers and calculating MDC
period <- c(2025, 2055, 2085)
# climateLayers <- lapply(period, function(y){
#   dtPath <- "/mnt/data/Micheletti/NWT/modules/climate_NWT_DataPrep/data/"
#   climateLayers <- setNames(
#     raster::stack(
#       lapply(
#         dir(dtPath, pattern = paste0("CanESM2_rcp45_", y, ".*[.]asc"), 
#             full.names = TRUE, recursive = TRUE),
#         function(file){
#           r <- raster(file)
#           crs(r) <- "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +units=m +no_defs +datum=WGS84"
#           
#           postProcess(
#             x = r,
#             rasterToMatch = rasterToMatch,
#             maskWithRTM = TRUE,
#             filename2 = NULL,
#             method = "bilinear",
#             destinationPath = tempdir()
#           )
#         })
#     ),
#     nm = c(
#       paste0("PPT", stringr::str_pad(1:12, width = 2, pad = 0)),
#       paste0("Tave", stringr::str_pad(1:12, width = 2, pad = 0)),
#       paste0("Tmax", stringr::str_pad(1:12, width = 2, pad = 0)),
#       paste0("Tmin", stringr::str_pad(1:12, width = 2, pad = 0))
#     )
#   )
# })

climateLayers <- readRDS("/mnt/data/Micheletti/NWT/outputs/08JUN19/climateLayers.rds")
names(climateLayers) <- period

MDC <- lapply(period, function(y){
  library("raster")
  raster::calc(climateLayers[[as.character(y)]], fun = function(x){
    L_f <- function(month)
    {
      c(
        '4' = 0.9, 
        '5' = 3.8,
        '6' = 5.8
      )[[as.character(month)]]
    }
              MDC_0 <- 0
              for (i in 4:6){
                ni <- c(
                  '4' = 30, 
                  '5' = 31,
                  '6' = 30
                )[[as.character(i)]]
                
                PPT <- x[[paste0("PPT0", i)]]
                Tmax <- x[[paste0("Tmax0", i)]]
                
                MDC_m <- pmax(
                  MDC_0 + .25 * ni * (.36 * Tmax + L_f(i)) -
                    400 * log(1 + 3.937 * .83 * PPT / (800 * exp(-MDC_0/400))) +
                    .25 * ni * (.36 * Tmax + L_f(i)), 
                  0
                )
                
                MDC_0 <- pmax((MDC_0 + MDC_m) / 2, 0)
              }
              return(MDC_0)
            }
)
})
names(MDC) <- period









