speciesMaxBHistogram <- function(speciesEcoregion, maxB){
  library("ggplot2")
  p1 <- ggplot(data = speciesEcoregion, aes(x = maxB)) +
    geom_histogram() +
    facet_grid(speciesCode ~ .)
}

