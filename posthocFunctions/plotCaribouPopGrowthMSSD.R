plotCaribouPopGrowthMSSD <- function(caribouPopulationGrowthTable, # Pass this if outside of module
                                   outputFolder,
                                   whichPolysToIgnore = NULL,
                                   addSD = FALSE,
                                   makeZoomed = FALSE)
{

  library("Require")
  Require("data.table")
  Require("ggplot2")
  
  tableAll <- caribouPopulationGrowthTable
    
  if (!is.null(whichPolysToIgnore)){
    tableAll <- tableAll[!Polygon %in% whichPolysToIgnore, ]
  }
  tableAll <- tableAll[, V1 := NULL, ]
  tableAll <- unique(tableAll)

if (addSD){
    tableAll[, annualLambdaSD := sd(annualLambda),
           by = c("Polygon", "area", "femSurvMod_recrMod", 
                  "Year")] # Here we are doing the SD across climate models and replicates
  tableAll[, annualLambdaSDmin := min(annualLambdaSD),
           by = c("Polygon", "area", "femSurvMod_recrMod", 
                  "Year")] # Here we are capturing the minimum (lower ribbon) SD across climate models and replicates
  tableAll[, annualLambdaSDmax := max(annualLambdaSD),
           by = c("Polygon", "area", "femSurvMod_recrMod", 
                  "Year")]# Here we are capturing the maximum (upper ribbon) SD across climate models and replicates
  tableAll[, annualLambdaLDV := min(annualLambda)-annualLambdaSDmin,
           by = c("Polygon", "area", "femSurvMod_recrMod", 
                  "Year")] # Here we combine the minimum annual lambda across climate models and replicates with the minimum SD across climate models and replicates 
  tableAll[, annualLambdaUDV := max(annualLambda)+annualLambdaSDmax,
           by = c("Polygon", "area", "femSurvMod_recrMod", 
                  "Year")] # Here we combine the maximum annual lambda across climate models and replicates with the maximum SD across climate models and replicates
  
  # We then use for the darker ribbon the annualLambdaLDV (lower ribbon) and annualLambdaUDV (upper ribbon)
  yaxis <- "annualLambda"
  yrReady <- lapply(X = unique(tableAll[["area"]]), 
                    FUN = function(shp){
                      polyReady <- lapply(X = unique(tableAll[area == shp, femSurvMod_recrMod]), 
                                          FUN = function(mod){
                                            message(paste0("Plotting caribou population growth for ", shp, 
                                                           " for ", mod))
                                            # Polygon == "Hay River Lowlands" & # TO TEST
                                            DT <- tableAll[area == shp & femSurvMod_recrMod == mod, ]
                                            survMod <- strsplit(strsplit(mod, "::")[[1]][1], "_National")[[1]][1]
                                            recMod <- strsplit(strsplit(mod, "::")[[1]][2], "_National")[[1]][1]
                                            
                                            tryCatch(quickPlot::clearPlot(), error = function(e){
                                              message(crayon::red("quickPlot::clearPlot() failed"))
                                            })
                                            if (unique(DT[["area"]]) == "metaHeards"){
                                              DT[Polygon == "Dehcho North_v2", Polygon := "Dehcho North"]
                                              DT[Polygon == "Dehcho South_v2", Polygon := "Dehcho South"]
                                              DT[Polygon == "GSA North", Polygon := "Gwich'in Settlement Area North"]
                                              DT[Polygon == "GSA South", Polygon := "Gwich'in Settlement Area South"]
                                              
                                              DT[, Polygon := factor(Polygon, 
                                                                     levels = c("Gwich'in Settlement Area North", 
                                                                                "Gwich'in Settlement Area South", 
                                                                                "Dehcho North", "Dehcho South", 
                                                                                "Hay River Lowlands"))]
                                            }
                                            
                                            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           group = climateModel)) +
                                              facet_grid(rows = vars(Polygon)) +
                                              geom_line(size = 0.9, aes(y = get(paste0("average", yaxis)),
                                                                        group = climateModel,
                                                                        linetype = climateModel),                                                                        color = "black") +
                                              geom_hline(yintercept = 1, linetype = "dotted", 
                                                         color = "grey20", size = 0.8) +
                                              geom_ribbon(aes(ymin = minRib, 
                                                              ymax = maxRib,
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.1, colour = NA) +
                                              geom_ribbon(aes(ymin = annualLambdaLDV, 
                                                              ymax = annualLambdaUDV,
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.4, colour = NA) +
                                              theme_linedraw() +
                                              theme(legend.position = "bottom",
                                                    legend.box = "vertical", legend.margin = margin(),
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial")) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year") +
                                              geom_jitter(data = DT, aes(x = Year,
                                                                         y = get(yaxis)),
                                                          size = 0.5, colour = "grey45",
                                                          width = 1) +
                                              scale_x_continuous(breaks = sort(as.numeric(unique(DT[["Year"]]))))
                                            tryCatch(quickPlot::clearPlot(), 
                                                     error = function(e){
                                                       message(crayon::red("quickPlot::clearPlot() failed"))
                                                     })
                                            png(file.path(outputFolder, 
                                                          paste0("caribou_", shp, "_allCM",
                                                                 "_", recMod,"_", survMod,
                                                                 ".png")),
                                                units = "cm", res = 300,
                                                width = 29, height = 21)
                                            print(popModelPlot)
                                            dev.off()
                                            
                                            # PLOT ZOOMED
                                            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           # colour = Polygon, 
                                                                                           group = climateModel)) +
                                              facet_grid(rows = vars(Polygon)) +
                                              geom_line(size = 0.9, aes(y = get(paste0("average", yaxis)),
                                                                        group = climateModel,
                                                                        linetype = climateModel),                                                                        color = "black") +
                                              geom_hline(yintercept = 1, linetype = "dotted", 
                                                         color = "grey20", size = 0.8) +
                                              geom_ribbon(aes(ymin = min(minRib, min(DT[["annualLambdaLDV"]])), 
                                                              ymax = min(maxRib, max(DT[["annualLambdaUDV"]])),
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.1, colour = NA) +
                                              geom_ribbon(aes(ymin = annualLambdaLDV, 
                                                              ymax = annualLambdaUDV,
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.4, colour = NA) +
                                              theme_linedraw() +
                                              theme(legend.position = "bottom",
                                                    legend.box = "vertical", legend.margin = margin(),
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial")) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year") +
                                              geom_jitter(data = DT, aes(x = Year,
                                                                         y = get(yaxis)),
                                                          size = 0.5, colour = "grey45",
                                                          width = 1) +
                                              scale_x_continuous(breaks = sort(as.numeric(unique(DT[["Year"]])))) +
                                              coord_cartesian(ylim = c(min(DT[["annualLambdaLDV"]]), max(DT[["annualLambdaUDV"]])))
                                            
                                            tryCatch(quickPlot::clearPlot(), 
                                                     error = function(e){
                                                       message(crayon::red("quickPlot::clearPlot() failed"))
                                                     })
                                            png(file.path(outputFolder, 
                                                          paste0("caribou_", shp, "_allCM",
                                                                 "_", recMod,"_", survMod,
                                                                 "ZOOM.png")),
                                                units = "cm", res = 300,
                                                width = 29, height = 21)
                                            print(popModelPlot)
                                            dev.off()
                                            
                                            return(popModelPlot)
                                          })
                      names(polyReady) <- unique(tableAll[area == shp, femSurvMod_recrMod])
                      return(polyReady)
                    })
  names(yrReady) <- unique(tableAll[["area"]])
  return(yrReady)
} else {
  # Derive the SD from the mean and quantiles. We assume that the Bootstrap 
  # estimates are normally distributed, so we can rebuild the distribution of 
  # values at each point in time using assumption of normality. Thus, if we know 
  # the 0.025 and 0.0975 quantiles, we can derive the Standard Deviation of the 
  # normal distribution.
  tableAll[, SD := (abs(annualLambda - annualLambdaMax) +
                      abs(annualLambda - annualLambdaMin))/1.96/2]
  valueOfInterest = seq(5, 95, by = 10)
  for (i in valueOfInterest){
    diffi <- i/100 # For naming convention, multiplied the 
                                # percentage by 100. Now need to divide again 
    # 1. Divide the quantile of interest by 2 
    diffi <- diffi/2
    # 2. UPPER of value of interest: 0.5 (which is the middle) + resulting value of 1 
    upperQ <- 0.5+diffi
    # 3. LOWER of value of interest: 0.5 (which is the middle) - resulting value of 1
    lowerQ <- 0.5-diffi
    
    tableAll[, c(paste0("lower", i),
                   paste0("upper", i)) := list(qnorm(p = lowerQ,
                                                     mean = annualLambda,
                                                     sd = SD),
                                               qnorm(p = upperQ,
                                                     mean = annualLambda,
                                                     sd = SD))]
    tableAll[, c(paste0("lowerMin", i),
                 paste0("upperMax", i)) := list(min(get(paste0("lower", i))),
                                                              max(get(paste0("upper", i)))),
             by = c("Polygon", "area", "femSurvMod_recrMod", "Year", "climateModel")]
    
  }
  alphy <- 0.075
  # Add several quantiles, and make the transparency the same to each, so naturally
  # we will have the center more likely, darker
  yaxis <- "annualLambda"
  yrReady <- lapply(X = unique(tableAll[["area"]]), 
                    FUN = function(shp){
                      polyReady <- lapply(X = unique(tableAll[area == shp, femSurvMod_recrMod]), 
                                          FUN = function(mod){
                                            message(paste0("Plotting caribou population growth for ", shp, 
                                                           " for ", mod))
                                            # Polygon == "Hay River Lowlands"& # TO TEST
                                            DT <- tableAll[area == shp & femSurvMod_recrMod == mod, ]
                                            survMod <- strsplit(strsplit(mod, "::")[[1]][1], "_National")[[1]][1]
                                            recMod <- strsplit(strsplit(mod, "::")[[1]][2], "_National")[[1]][1]
                                            
                                            tryCatch(quickPlot::clearPlot(), error = function(e){
                                              message(crayon::red("quickPlot::clearPlot() failed"))
                                            })
                                            if (unique(DT[["area"]]) == "metaHeards"){
                                              DT[Polygon == "Dehcho North_v2", Polygon := "Dehcho North"]
                                              DT[Polygon == "Dehcho South_v2", Polygon := "Dehcho South"]
                                              DT[Polygon == "GSA North", Polygon := "Gwich'in Settlement Area North"]
                                              DT[Polygon == "GSA South", Polygon := "Gwich'in Settlement Area South"]
                                              
                                              DT[, Polygon := factor(Polygon, 
                                                                     levels = c("Gwich'in Settlement Area North", 
                                                                                "Gwich'in Settlement Area South", 
                                                                                "Dehcho North", "Dehcho South", 
                                                                                "Hay River Lowlands"))]
                                            }
                                            popModelPlot <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           group = climateModel)) +
                                              facet_grid(rows = vars(Polygon)) +
                                              geom_line(size = 0.9, aes(y = get(paste0("average", yaxis)),
                                                                        group = climateModel,
                                                                        linetype = climateModel),
                                                        color = "black") +
                                              geom_hline(yintercept = 1, linetype = "dotted", 
                                                         color = "grey20", size = 0.8) + 
                                                geom_ribbon(aes(ymin = get(paste0("lowerMin", 5)),
                                                                ymax = get(paste0("upperMax", 5)),
                                                                group = climateModel,
                                                                fill = Polygon),
                                                            alpha = alphy,
                                                            colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 15)), 
                                                              ymax = get(paste0("upperMax", 15)),
                                                              group = climateModel,
                                                              fill = Polygon), 
                                                          alpha = alphy, 
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 25)),
                                                              ymax = get(paste0("upperMax", 25)),
                                                              group = climateModel,
                                                              fill = Polygon),
                                                          alpha = alphy,
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 35)), 
                                                              ymax = get(paste0("upperMax", 35)),
                                                              group = climateModel,
                                                              fill = Polygon), 
                                                          alpha = alphy, 
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 45)),
                                                              ymax = get(paste0("upperMax", 45)),
                                                              group = climateModel,
                                                              fill = Polygon),
                                                          alpha = alphy,
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 55)), 
                                                              ymax = get(paste0("upperMax", 55)),
                                                              group = climateModel,
                                                              fill = Polygon), 
                                                          alpha = alphy, 
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 65)),
                                                              ymax = get(paste0("upperMax", 65)),
                                                              group = climateModel,
                                                              fill = Polygon),
                                                          alpha = alphy,
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 75)), 
                                                              ymax = get(paste0("upperMax", 75)),
                                                              group = climateModel,
                                                              fill = Polygon), 
                                                          alpha = alphy, 
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 85)),
                                                              ymax = get(paste0("upperMax", 85)),
                                                              group = climateModel,
                                                              fill = Polygon),
                                                          alpha = alphy,
                                                          colour = NA) +
                                              geom_ribbon(aes(ymin = get(paste0("lowerMin", 95)), 
                                                              ymax = get(paste0("upperMax", 95)),
                                                              group = climateModel,
                                                              fill = Polygon), 
                                                          alpha = alphy, 
                                                          colour = NA) +
                                              theme_linedraw() +
                                              theme(legend.position = "bottom",
                                                    legend.box = "vertical", legend.margin = margin(),
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial"),
                                                    panel.grid.minor = element_line(colour = "lightgrey", 
                                                                                    size = 0.1),
                                                    panel.grid.major.x = element_line(colour = "grey85", 
                                                                                    size = 0.1),
                                                    panel.grid.major.y = element_line(colour = "grey65", 
                                                                                      size = 0.1)) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year") +
                                              geom_jitter(data = DT, aes(x = Year,
                                                                         y = get(yaxis)),
                                                          size = 0.5, colour = "grey45",
                                                          width = 1) +
                                              scale_x_continuous(breaks = sort(as.numeric(unique(DT[["Year"]]))))
                                            tryCatch(quickPlot::clearPlot(), 
                                                     error = function(e){
                                                       message(crayon::red("quickPlot::clearPlot() failed"))
                                                     })
                                            png(file.path(outputFolder, 
                                                          paste0("caribou_", shp, "_allCM",
                                                                 "_", recMod,"_", survMod,
                                                                 ".png")),
                                                units = "cm", res = 300,
                                                width = 29, height = 21)
                                            print(popModelPlot)
                                            dev.off()
                                            
                                            # PLOT ZOOMED
                                            if (makeZoomed){
                                            popModelPlotZ <- ggplot2::ggplot(data = DT, aes(x = Year,
                                                                                           # colour = Polygon, 
                                                                                           group = climateModel)) +
                                              facet_grid(rows = vars(Polygon)) +
                                              geom_line(size = 0.9, aes(y = get(paste0("average", yaxis)),
                                                                        group = climateModel,
                                                                        linetype = climateModel),                                                                        color = "black") +
                                              geom_hline(yintercept = 1, linetype = "dotted", 
                                                         color = "grey20", size = 0.8) +
                                              geom_ribbon(aes(ymin = min(minRib, min(DT[["annualLambdaLDV"]])), 
                                                              ymax = min(maxRib, max(DT[["annualLambdaUDV"]])),
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.1, colour = NA) +
                                              geom_ribbon(aes(ymin = annualLambdaLDV, 
                                                              ymax = annualLambdaUDV,
                                                              group = climateModel,
                                                              fill = Polygon), alpha = 0.4, colour = NA) +
                                              theme_linedraw() +
                                              theme(legend.position = "bottom",
                                                    legend.box = "vertical", legend.margin = margin(),
                                                    title = element_blank(),
                                                    strip.text.y = element_blank(),
                                                    legend.key = element_blank(),
                                                    legend.title = element_blank(),
                                                    axis.title = element_text(family = "Arial")) +
                                              ylab(expression(Mean~annual~lambda)) +
                                              xlab("year") +
                                              geom_jitter(data = DT, aes(x = Year,
                                                                         y = get(yaxis)),
                                                          size = 0.5, colour = "grey45",
                                                          width = 1) +
                                              scale_x_continuous(breaks = sort(as.numeric(unique(DT[["Year"]])))) +
                                              coord_cartesian(ylim = c(min(DT[["annualLambdaLDV"]]), max(DT[["annualLambdaUDV"]])))
                                            
                                            tryCatch(quickPlot::clearPlot(), 
                                                     error = function(e){
                                                       message(crayon::red("quickPlot::clearPlot() failed"))
                                                     })
                                            png(file.path(outputFolder, 
                                                          paste0("caribou_", shp, "_allCM",
                                                                 "_", recMod,"_", survMod,
                                                                 "ZOOM.png")),
                                                units = "cm", res = 300,
                                                width = 29, height = 21)
                                            print(popModelPlotZ)
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


  }
  
