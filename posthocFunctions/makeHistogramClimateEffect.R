makeHistogramClimateEffect <- function(fireDifferenceRaster, 
                                       vegetationDifferenceRaster){

  vegetation <- na.omit(getValues(vegetationDifferenceRaster))
  fire <- na.omit(getValues(fireDifferenceRaster))
  DT <- data.table::data.table(type = c(rep("vegetation", times = length(vegetation)),
                                        rep("fire", times = length(fire))), 
                               val = c(vegetation, fire))
  roundUpNice <- function(x, nice = c(1,2,4,5,6,8,10)) {
    if(length(x) != 1) stop("'x' must be of length 1")
    10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
    # Credit to Tommy (https://stackoverflow.com/users/662787/tommy)
  }
  minVal <- -roundUpNice(abs(min(DT$val)))/2
  maxVal <- roundUpNice(abs(max(DT$val)))/2
  
  p <- ggplot(data = DT, aes(x = val)) +
    geom_histogram(data = DT[type == "vegetation", ],
                   fill = "darkgreen", 
                   alpha = .5, binwidth = 600) + 
    geom_histogram(data = DT[type == "fire", ],
                   fill = "darkred", 
                   alpha = .5, binwidth = 600) + 
    geom_vline(xintercept = mean(DT[type == "vegetation", val]), 
               colour = "darkgreen") +
    geom_vline(xintercept = mean(DT[type == "fire", val]), 
               colour = "darkred") +
    labs(title = "Climate Effects on Biomass", 
         sub = paste0("vegetation average: ", mean(DT[type == "vegetation", val]), 
                      "fire average: ", mean(DT[type == "fire", val]))) + 
    xlim(c(minVal, maxVal))
  return(p)
}