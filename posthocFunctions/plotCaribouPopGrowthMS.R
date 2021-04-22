plotCaribouPopGrowthMS <- function(startTime,
                                 currentTime,
                                 endTime,
                                 resultsMainFolder = NULL, # Pass this if outside of module
                                 climateModel = NULL,
                                 predictedCaribou = NULL,
                                 yearSimulationStarts,
                                 reps = paste0("run", 1:5),
                                 outputFolder,
                                 whichPolys = NULL, # Optional to ensure only specific polygons to be plotted
                                 timeSpan = "annual") # Optional = "timeStep" (normally every 10y)
  {

  library("Require")
  Require("data.table")
  Require("ggplot2")
  if (any(all(is.null(resultsMainFolder), 
          is.null(predictedCaribou)),
          all(!is.null(resultsMainFolder), 
              !is.null(predictedCaribou))))
    stop("Please provide either predictedCaribou or resultsMainFolder")
  
  if (is.null(climateModel)){
    message(crayon::red("climateModel is NULL, default is 'CCSM4'"))
    climateModel <- "CCSM4"
  }
  if (!is.null(resultsMainFolder)){
    allcombs <- data.table(expand.grid(climateModel, reps))
    allcombs[, comb := paste0(Var1, "_",Var2)]
    pth <- file.path(resultsMainFolder, allcombs[["comb"]])
    
    predictedCaribou <- rbindlist(lapply(seq_along(pth), function(filePathIndex){
      tb <- readRDS(list.files(path = pth[filePathIndex], 
                               pattern = paste0("predictedCaribou_year", currentTime), 
                               full.names = TRUE, recursive = TRUE))
      addedTB <- rbindlist(lapply(names(tb), function(years){
        TB <- tb[[years]]
        climMod <- strsplit(basename(pth[filePathIndex]), "_")[[1]][1]
        replic <- strsplit(basename(pth[filePathIndex]), "_")[[1]][2]
        TB[, c("climateModel", "Replicate", "Year") := list(climMod, 
                                                            replic, 
                                                            usefulFuns::substrBoth(years, 4, T))]
        return(TB)
      }))
      return(addedTB)
    }))
    
    if (!is.null(whichPolys)){
      predictedCaribou <- predictedCaribou[polygon %in% whichPolys, ]
    }
    tableAll <- predictedCaribou
  }
  
  yaxis <- if (timeSpan == "annual") "annualLambda" else "growth"
  yaxisName <- yaxis
  
  names(tableAll)[names(tableAll) == "polygon"] <- "Polygon"
  tableAll[, minRib := min(get(paste0(yaxis, "Min"))), by = c("Year", "Polygon", 
                                                              "climateModel", "femSurvMod_recrMod")]
  tableAll[, maxRib := max(get(paste0(yaxis, "Max"))), by = c("Year", "Polygon", 
                                                              "climateModel", "femSurvMod_recrMod")]
  tableAll[, paste0("average", yaxis) := mean(get(yaxis)), by = c("Year", "Polygon", "climateModel", 
                                                                  "femSurvMod_recrMod")]
  
  yrReady <- lapply(X = unique(tableAll[["area"]]), 
                    FUN = function(shp){
                      polyReady <- lapply(X = unique(tableAll[area == shp, femSurvMod_recrMod]), 
                                          FUN = function(mod){
                                            message(paste0("Plotting caribou population growth for ", shp, 
                                                           " for ", mod))
                                            DT <- tableAll[area == shp & femSurvMod_recrMod == mod, ]
                                            survMod <- strsplit(strsplit(mod, "::")[[1]][1], "_National")[[1]][1]
                                            recMod <- strsplit(strsplit(mod, "::")[[1]][2], "_National")[[1]][1]
                                            
                                            tryCatch(quickPlot::clearPlot(), error = function(e){
                                              message(crayon::red("quickPlot::clearPlot() failed"))
                                            })
                                            
                                            DT[Polygon == "Dehcho North_v2", Polygon := "Dehcho North"]
                                            DT[Polygon == "Dehcho South_v2", Polygon := "Dehcho South"]
                                            
                                            # a) increase the resolution (300 dpi tiff file), [DONE]
                                            # b) remove the grey background so that the 1-dashed lambda line stands out more [OK]
                                            # c) remove the title and save what is being presented in the image name. [OK]
                                            # d) Each GCM needs to be a different linetype
                                            # e) Each ribbon belongs to a climate scenario and is overplotted
                                            # f) Add the table with values coming from the other function -- NULL default!
                                            # g) data.table(Area, climateModel, averageRSF) --> needs to match in the strip!
                                            # h) label the y axis with the lambda symbol as ("Mean lambda") [OK]
                                            # i) label the x axis as "year"  [OK]
                                            # j) Change "Dehcho North_v2" to just "Dehcho North". Same with Dehcho South. [OK]
                                            
                                            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           colour = Polygon, 
                                                                                           group = climateModel)) +
                                              geom_line(size = 0.9, aes(y = get(paste0("average", yaxis)),
                                                                        group = climateModel,
                                                                        linetype = climateModel)) +
                                              facet_grid(rows = vars(Polygon)) +
                                              geom_hline(yintercept = 1, linetype = "dotted", 
                                                         color = "grey73", size = 1) +
                                              geom_ribbon(aes(ymin = minRib, 
                                                              ymax = maxRib,
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.1, colour = NA) +
                                              theme_linedraw() +
                                              # ggtitle(label = paste0("Caribou population dynamics: ", climateModel),
                                              #         subtitle = paste0("Female Survival Model: ", survMod,
                                              #                           "\nRecruitment Model: ", recMod)) +
                                              theme(legend.position = "bottom",
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial")) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year")
                                            
                                            if ("Replicate" %in% names(DT)){
                                              popModelPlot <- popModelPlot + geom_jitter(data = DT, aes(x = Year,
                                                                                                        y = get(yaxis)),
                                                                                         size = 1, colour = "grey40",
                                                                                         width = 0.2)
                                            }
                                            
                                            if(currentTime == endTime){
                                              tryCatch(quickPlot::clearPlot(), 
                                                       error = function(e){
                                                         message(crayon::red("quickPlot::clearPlot() failed"))
                                                       })
                                              png(file.path(outputFolder, 
                                                            paste0("caribou_", shp, "_allCM",
                                                                   "_", recMod,"_", survMod,
                                                                   ifelse(!is.null(resultsMainFolder), "_reps", ""),
                                                                   ".png")),
                                                  units = "cm", res = 300,
                                                  width = 29, height = 21)
                                              print(popModelPlot)
                                              dev.off()
                                            }
                                            return(popModelPlot)
                                          })
                      names(polyReady) <- unique(tableAll[area == shp, femSurvMod_recrMod])
                      return(polyReady)
                    })
  names(yrReady) <- unique(tableAll[["area"]])
  return(yrReady)
}
