areAnyRunsIdentical <- function(outputsFolder){
  
  scenarios <- list.dirs(outputsFolder, recursive = FALSE, full.names = FALSE)
  diffScen <- lapply(scenarios, function(SCENARIO){
    runs <- list.dirs(file.path(outputsFolder, SCENARIO), recursive = FALSE, full.names = FALSE)
    diffRuns <- lapply(runs, function(RUN){
      fireResults <- readRDS(file.path(outputsFolder, SCENARIO, RUN, paste0("burnSummary_year2011.rds")))
      return(fireResults)
    })
    names(diffRuns) <- runs
    return(diffRuns)
  })
  names(diffScen) <- scenarios
  allBurns <- unlist(diffScen, recursive = FALSE, use.names = TRUE)
  factorNames <- data.table::data.table(index = seq_along(names(allBurns)),
                                        scenarioRun = names(allBurns))
  
  allTests <- data.table::data.table(expand.grid(simulation1Name = factorNames[["index"]],
                                                 simulation2Name = factorNames[["index"]]))
  allTests <- allTests[simulation1Name != simulation2Name,]
  
  mn <- pmin(allTests[["simulation1Name"]], allTests[["simulation2Name"]])
  mx <- pmax(allTests[["simulation1Name"]], allTests[["simulation2Name"]])
  int <- as.numeric(interaction(mn, mx))
  allTests <- allTests[match(unique(int), int),]
  allTests <- merge(allTests, factorNames, by.x = "simulation1Name", by.y = "index")
  allTests[, simulation1Name := NULL]
  names(allTests)[names(allTests) == "scenarioRun"] <- "simulation1Name"
  allTests <- merge(allTests, factorNames, by.x = "simulation2Name", by.y = "index")
  allTests[, simulation2Name := NULL]
  names(allTests)[names(allTests) == "scenarioRun"] <- "simulation2Name"

  allTests[, Identical := identical(allBurns[[simulation1Name]], allBurns[[simulation2Name]]), 
           by = seq_len(NROW(allTests))]
  
  if (any(allTests[["Identical"]])){
    return(allTests[Identical == TRUE, ])
  } else return(paste("No identical simulations found!"))
}
