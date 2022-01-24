#########################################################
##                 H O T S P O T S                     ##
#########################################################

# This version of the hotspot analysis does not use the spatial optimization

# run !sourceSscript 
SpaDES.core::setPaths(cachePath = hotspotsCache,
                      outputPath = checkPath(file.path(getwd(), "outputs",
                                                       toupper(format(Sys.time(), "%d%b%y")),
                                                       definedRun$whichRUN,
                                                       replicateNumber),
                                             create = TRUE))
generalOutputs <- dirname(file.path(getwd(), "outputs",
                                    "landscapeRuns",
                                    definedRun$whichRUN,
                                    replicateNumber))
if (all(runLandR == FALSE)){
  if (is.null(originalDateAnalysis)) stop("If runLandR == FALSE you need to pass the date for the 
                                            analysis (i.e. where LandR results are)")
  newInputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                       replacement = originalDateAnalysis)
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  setPaths(inputPath = newInputPath,
           outputPath = newOutputPath)
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots/ms1"), create = TRUE)
  setPaths(outputPath = hotOutPath)
} else {
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots_ms1"), create = TRUE)
  setPaths(inputPath = newOutputPath,
           outputPath = hotOutPath)
}
