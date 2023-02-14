library("Require")
Require("usefulFuns")
Require("data.table")

# FOR BIRD GROUP SCENARIOS
years <- c(2011, 2031, 2051, 2071)
patt <- paste0(paste0("_solutions_Year", years),".tif")

for (cs in c("CCSM4", "CanESM2", "INM-CM4")){
  for (RUN in paste0("run", 1:5)){
    for (tp in c("shrub", "generalist", "deciduous", "conifer", "wetland", "grassland", "mixedwood")) {
      fls <- grepMulti(list.files(path = file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS", 
                                                   paste0(cs, "_", RUN, ""), "hotspots", "ms1")), 
                       patterns = paste(paste0(tp, patt), collapse = "|"))
      pattRev <- data.table(expand.grid(as.character(as.roman(32:41)),"_", tp, patt))
      pattRev[, X := paste0(Var1, Var2, Var3, Var4)]
      Diff <- setdiff(pattRev[["X"]], fls)
      if (length(Diff) != 0) {
        message(crayon::red(paste0("Scenario ", cs, " ", RUN, " ", tp, " is missing ", 
                                   paste(Diff, collapse = "\n"))))
      } else {
        message(crayon::green(paste0("Scenario ", cs, " ", RUN, " ", tp, " is OK")))
      }
    }
  }
}

# FOR RANDOM SCENARIOS
years <- c(2011, 2031, 2051, 2071)
patt <- data.table(expand.grid(as.character(as.roman(42:51)),
                               paste0("_solutions_Year", years),".tif"))
patt[, X := paste0(Var1, Var2, Var3)]

for (cs in c("CCSM4", "CanESM2", "INM-CM4")){
  for (RUN in paste0("run", 1:5)){
      fls <- grepMulti(list.files(path = file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS", 
                                                   paste0(cs, "_", RUN, ""), "hotspots", "ms1")), 
                       patterns = paste(patt[["X"]], collapse = "|"), unwanted = "XLI_")
      Diff <- setdiff(patt[["X"]], fls)
      if (length(fls) != 10) {
        message(crayon::red(paste0("Scenario ", cs, " ", RUN, " has only ", length(fls), " files")))
      } else {
        message(crayon::green(paste0("Scenario ", cs, " ", RUN, " is OK")))
      }
  }
}

# FOR CARIBOU SCENARIOS
years <- c(2011, 2031, 2051, 2071)
patt <- data.table(expand.grid(as.character(as.roman(22:31)),
                               paste0("_solutions_Year", years),".tif"))
patt[, X := paste0(Var1, Var2, Var3)]

for (cs in c("CCSM4", "CanESM2", "INM-CM4")){
  for (RUN in paste0("run", 1:5)){
      fls <- grepMulti(list.files(path = file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS", 
                                                   paste0(cs, "_", RUN, ""), "hotspots", "ms1")), 
                       patterns = paste(patt[["X"]], collapse = "|"), 
                       unwanted = "XXXVI_")
      Diff <- setdiff(patt[["X"]], fls)
      if (length(Diff) != 0) {
        message(crayon::red(paste0("Scenario ", cs, " ", RUN, " is missing ", 
                                   paste(Diff, collapse = "|"))))
      } else {
        message(crayon::green(paste0("Scenario ", cs, " ", RUN, " is OK")))
    }
  }
}

# Moving the scenarios that already exist
patt <- paste0(c("shrub", "generalist", "deciduous", "conifer", "wetland", "grassland", "mixedwood"), ".qs")
birdGroupFiles <- grepMulti(list.files("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1", 
                                       full.names = TRUE),
                            patterns = paste(patt, collapse = "|"))
for (fl in birdGroupFiles){
  reproducible::file.move(from = fl, 
            to = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/hotspots/ms1/birdGroupswithBoo/")
  message(crayon::green("File ", basename(fl), paste0(" (", round(which(birdGroupFiles == fl)/length(birdGroupFiles)*100, 0)),"%) moved successfully!"))
}



