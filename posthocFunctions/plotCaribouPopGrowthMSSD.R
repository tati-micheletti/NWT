plotCaribouPopGrowthMSSD <- function(caribouPopulationGrowthTable, # Pass this if outside of module
                                   outputFolder,
                                   whichPolysToIgnore = NULL)
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
  # CI <- function (x, ci = 0.95, toReturn = "mean"){
  #   a <- mean(x)
  #   s <- sd(x)
  #   n <- length(x)
  #   error <- qt(ci + (1 - ci)/2, df = n - 1) * s/sqrt(n)
  #   if (toReturn == "mean")
  #     return(a) else
  #       if (toReturn == "L")
  #         return(a-error) else
  #           if (toReturn == "U")
  #             return(a+error)
  # }
  # tableAll[, annualLambdaLCI := CI(annualLambda, toReturn = "L"), 
  #          by = c("Polygon", "area", "femSurvMod_recrMod", "Year")]
  # tableAll[, annualLambdaUCI := CI(annualLambda, toReturn = "U"), 
  #          by = c("Polygon", "area", "femSurvMod_recrMod", "Year")]

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
}
