plotCaribou2 <- function(startTime = start(sim),
                        currentTime = time(sim),
                        endTime = end(sim),
                        predictedCaribou = sim$predictedCaribou,
                        yearSimulationStarts = P(sim)$yearSimulationStarts){
  library("reproducible")
  reproducible::Require(ggplot2)
  # Year -> Shapefile -> Polygon -> Model -> results
if (is(predictedCaribou, "list")){
  tableAll <- data.table::rbindlist(lapply(X = 1:length(predictedCaribou), FUN = function(yr){ # here I extract the info for all locations and models, make a big table
    yrReady <- data.table::rbindlist(lapply(X = 1:length(predictedCaribou[[yr]]), FUN = function(shp){
      shpReady <- data.table::rbindlist(lapply(X = 1:length(predictedCaribou[[yr]][[shp]]), function(ply){
        polyReady <- data.table::rbindlist(lapply(X = 1:length(predictedCaribou[[yr]][[shp]][[ply]]), function(mod){
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$Polygon <- names(predictedCaribou[[yr]][[shp]])[[ply]]
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$CaribouArea <- names(predictedCaribou[[yr]])[[shp]]
          predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]]$Year <- names(predictedCaribou)[[yr]]
          return(predictedCaribou[[yr]][[shp]][[ply]][[mod]][["results"]])
        }))
        return(polyReady)
      }))
      return(shpReady)
    }))
    return(yrReady)
  }))

  if ((length(unique(tableAll$Year)) != length(startTime:currentTime)) & (startTime == 0)){
    time <- as.integer(startTime + 1:currentTime) 
  } else {
    time <- as.integer(startTime:currentTime)
  }
  
  timeStr <- as.numeric(sapply(strsplit(unique(tableAll$Year), split = "Year"), "[[", 2))
  yearTime <- data.table::data.table(Year = unique(tableAll$Year), 
                                     Time = timeStr)

  tableAll <- merge(tableAll, yearTime)
} else {
  tableAll <- predictedCaribou
}
  tableAll2 <- tableAll[Time >= startTime]
  tryCatch(quickPlot::clearPlot(), error = function(e){message(crayon::red("quickPlot::clearPlot() failed"))})
  # plts <- lapply(X = unique(tableAll$CaribouArea), function(shp){ #
shp <- unique(tableAll$CaribouArea)[1] # 
    yaxis <- if (unique(tableAll[["populationModel"]]) == "annualLambda") "lambda" else unique(tableAll[["populationModel"]])
    
    popModelPlot <- ggplot2::ggplot(data = tableAll2[CaribouArea == shp], aes(x = Time,
                                                                             y = modelParam, 
                                                                             colour = Polygon)) +
      facet_grid(rows = vars(Polygon)) +
      geom_hline(yintercept = 1, linetype = "dotted", color = "grey73", size = 1) +
      geom_line(size = 1.2) +
      geom_ribbon(aes(ymin = minModelParam, ymax = maxModelParam, 
                      fill = Polygon), alpha = 0.3, colour = NA) +
      ggtitle(paste0("Caribou population dynamics")) +
      theme(legend.position = "bottom",
            strip.text.y = element_blank()) +
      ylab(yaxis)
    
    if(currentTime == endTime){
      
      tryCatch(quickPlot::clearPlot(), error = function(e){message(crayon::red("quickPlot::clearPlot() failed"))})
      pngPath <- reproducible::checkPath(file.path(getwd(), "outputs"), create = TRUE)
      png(file.path(pngPath, 
                    paste0("caribou", shp, "_", 
                           toupper(format(Sys.time(), "%d%b%y")),".png")), 
          width = 700, height = 480)
      print(popModelPlot)
      dev.off()
    }
    return(popModelPlot)
  # }) #
  
  # return(plts) #
}