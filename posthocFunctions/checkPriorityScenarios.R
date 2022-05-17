
perc <- 0

climateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
runs <- paste0("run", 1:5)
specificScenarios <- data.table(expand.grid(as.character(as.roman(32:41)), 
                                            c("shrub", "generalist", "deciduous",
                                              "conifer", "wetland", "grassland")))
specificScenarios[, scenarios := paste(Var1, Var2, sep = "_")]
wantedScenarios <- c(as.character(as.roman(22:31)), # Caribou Conservation Scenarios
                     specificScenarios[["scenarios"]], # Reference Scenarios
                     as.character(as.roman(42:51))) # Random Scenarios
yearsWanted <- seq(2011, 2091, by = 20)
  
while (perc < 99.9) {
ScenariosMissing <- rbindlist(lapply(X = climateScenarios, function(ClimateScenario){
  allRuns <- rbindlist(lapply(X = runs, function(Run){
    allScenarios <- rbindlist(lapply(X = wantedScenarios, function(scen){
      booBirdTable <- file.path("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1/",
                                paste0("FigNumbers_", ClimateScenario, "_",
                                       Run,"_",
                                       scen,".qs"))
      DT <- data.table(ClimateScenario = ClimateScenario,
                       Run = Run,
                       Scenario = scen,
                       Exists = ifelse(file.exists(booBirdTable), TRUE, FALSE))
      return(DT)
    }))
  }))
}))
perc <- round(100*NROW(ScenariosMissing[Exists == TRUE, ])/NROW(ScenariosMissing), 2)
message(crayon::green(paste0("Completed ", perc, "% of scenarios...")))
Sys.sleep(240)
}

