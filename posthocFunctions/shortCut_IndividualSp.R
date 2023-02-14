
# 1. For each year, get all runs and calculate the mean of each scenario
stepInterval <- 20 # What is the interval for which there is data?
yearsWanted <- seq(Times$start, Times$end, by = stepInterval)
# wantedScenarios <- paste0("area", seq(5, 95, by = 10))
wantedScenarios <- seq(0.05, 0.95, by = 0.1)
internalFolder <- "ms1" # Here to set in which folder are the scenarios I am looking for (i.e. ms1)
runAUTO <- FALSE

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
                                    "hotspots/ms"), create = TRUE)
  
  setPaths(outputPath = hotOutPath)
} else {
  newOutputPath <- gsub(x = Paths$outputPath, pattern = toupper(format(Sys.time(), "%d%b%y")), 
                        replacement = originalDateAnalysis)
  hotOutPath <- checkPath(file.path(newOutputPath, 
                                    "hotspots_ms"), create = TRUE)
  setPaths(inputPath = newOutputPath,
           outputPath = hotOutPath)
}


if (runAUTO){
  climateScenarios <- strsplit(x = climateModel, split = "_")[[1]][1]
  # runs <- paste0("run", RUN)
  runs <- paste0("run", 1:5) # Trying all replicates in one bash
  # message(crayon::red(paste0("PROCESSING Climate Scenarios ", climateScenarios, " run ", RUN)))
  message(crayon::red(paste0("PROCESSING Climate Scenarios ", climateScenarios, " all runs")))
} else {
  climateScenarios <- c("CCSM4", "CanESM2", "INM-CM4")
  runs <- paste0("run", 1:5)
}

# Here I need to randomly select areas for each and save them as new scenario names (42:51)
# in Paths$outputPath, as paste0("solRas_", scen, "_Year2011.tif")
source("~/projects/NWT/posthocFunctions/randomlySelectAreas.R")
appendixFolder <- "1oFDas7tzCv3pJgXrr-f7a53N7Gb9Can_"
individualSpFolder <- "1ffWDDurVMVmK1Ezlgzg4Yoos_F_l01B2"
figuresFolder <- "1D8-H4h-59vD3nea9sn3aICa4SqVRulQO"
doBoxplot <- TRUE

source('functions/makePlanningUnit.R')
planningUnit <- makePlanningUnit(years = yearsWanted,
                                 pU = rasterToMatch)

################################
overwriteFig1Numbers <- FALSE ### <~~~~ IMPORTANT!!! ATTENTION!!!
overwriteFinalTables <- FALSE
################################

allb <- usefulFuns::substrBoth(list.files("~/projects/NWT/modules/birdsNWT/data/models/",
                                          pattern = "brt8.R"),
                               howManyCharacters = 4,
                               fromEnd = FALSE)

fullFinalTablePath <- file.path(generalOutputs, "fullFinalTable.qs") # Make and save the darn full table!

  finalTable <- qs::qread(fullFinalTablePath)

# EXCLUDE SPECIES THAT WERE DEEMED TO NOT HAVE GOOD MODEL FIT
spToRemove <- c("BOWA", "BANS", "PIWO", "NOFL", "BARS", "BRBL")
finalTable <- finalTable[!Species %in% spToRemove,]
length(unique(finalTable$Species)) # 71 species left!

print("Process finished!")

################
#              #
#   FIGURES    #
#              #
################

Require("lattice")
Require("rasterVis")
Require("viridis")
Require("maptools")
Require("colorspace")
Require("ggplot2")
Require("googledrive")

generalOutputs <- Paths$outputPath
birdsGroupingTable <- prepInputs(url = "https://drive.google.com/file/d/1_JN2N0JLlM_50zTSZMdTgWXP7-hNS8ua/pub?output=csv", 
                                 targetFile = "Bird Classification - birdHabitatRevisedV2.csv",
                                 destinationPath = generalOutputs, 
                                 fun = "data.table::fread", 
                                 header = TRUE)

# Simplyfying and putting the correct names
birdsGroupingTable <- birdsGroupingTable[, c("Species Code", "Habitat")]
names(birdsGroupingTable) <- c("Species", "Habitat")

########################################
#  PLOTS  EXTRA:  Individual Species   #
########################################

# Still need to adjust the colour based on group and
# look through species
redoIndividualSpecies <- TRUE
redoIndividualMaps <- TRUE
redoIndividualModels <- FALSE

if (redoIndividualSpecies){ # Don't need to touch individual species
  finalTable4 <- merge(finalTable, 
                       birdsGroupingTable, by = "Species", all.x = TRUE)
  DTd <- dcast(finalTable4,
               Species + Run + Year + ClimateScenario + ProportionAreaChosen + Habitat ~ PrioritizedFor,
               value.var = "ProportionIndividualsConserved")
  
  finalTable4 <- rbindlist(lapply(unique(DTd$Species), function(sp){
    allYears <- rbindlist(lapply(unique(DTd$Year), function(Y){
      allAreas <- rbindlist(lapply(unique(DTd$ProportionAreaChosen), function(A){
        toKeep <- c("Species", "Run", "ClimateScenario", "Year", "ProportionAreaChosen",
                    "Habitat", "caribou", sp, "random")
        DT <- DTd[Species == sp & Year == Y & ProportionAreaChosen == A, ..toKeep]
        # ID and Remove rows where random is bigger than the reference --> cause for artifacts
        rowsToRemove <- which(DT[["random"]] > DT[[sp]])
        toExcl <- data.table()
        if (length(rowsToRemove) != 0){
          toExcl <- DT[rowsToRemove]
          # Warning about which Species, Year, climate scenario, and Run we have this problem
          message(crayon::yellow(paste0("Random scenario is higher than reference for ", 
                                        crayon::red(NROW(toExcl))," points for ", crayon::red(sp), 
                                        " year ", crayon::red(Y), " area ", crayon::red(A), 
                                        " and ", 
                                        crayon::red(paste(unique(toExcl[["ClimateScenario"]]), 
                                                          collapse = "; ")))))
          DT <- DT[-rowsToRemove]
          if (NROW(DT) == 0){
            return(list(tb = NULL, pointsExcluded = NROW(toExcl)))
          }
        }
        boo <- DT[["caribou"]]
        low <- DT[["random"]]
        upp <- DT[[sp]]
        umbrellaIndex <- numeric(NROW(DT))
        for (i in 1:length(umbrellaIndex)){
          umbrellaIndex[i] <- scales::rescale(boo[i], c(0, 1), c(low[i], upp[i]))
        }
        DT[, umbrellaIndex := umbrellaIndex]
        return(DT[, c("Species", "Run", "ClimateScenario", 
                      "Year", "ProportionAreaChosen", 
                      "umbrellaIndex")])
      }))
    }))
  })) # I think this was fixed and should be deprecated
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[1:n]
  }
  sps <- c("conifer", "deciduous", "generalist", "grassland", 
           "mixedwood", "shrub", "wetland")
  
  cols <- gg_color_hue(length(sps))
  
  Require("ggplot2")
  
  finalTableSP <- merge(finalTable4,
                        birdsGroupingTable, by = "Species", all.x = TRUE)
  
  setkey(finalTableSP, "Habitat", "Species", "umbrellaIndex")
  
  byScenario <- TRUE
  byClimateModel <- FALSE
  Cls <- c("CanESM2" = "darkred", "CCSM4" = "goldenrod1", "INM-CM4" = "darkgreen")
  scens <- seq(0.05, 0.95, by = 0.1)
  source("posthocFunctions/makeMapsForSp.R")
  tictoc::tic("Total Time Elapsed: ")
  Require("future")
  Require("future.apply")
  # plan("sequential")
  spToRemove <- c("BOWA", "BANS", "PIWO", "NOFL", "BARS", "BRBL")
  bibis <- allb[!allb %in% spToRemove]
  print("Starting maps for species...")
    plan("multicore")
  future_lapply(bibis, function(sp){ # #
      lapply(2031, function(y){ #yearsWanted
        lapply(scens, function(scen){
          print(paste0("Running ", sp, " for ", y, " for ", scen))
          # INDIVIDUAL MAPS
          tic(paste0("Time elapsed for ", sp, " for ", y, " for ", scen, ": "))
          if (redoIndividualMaps){
          mps <- makeMapsForSp(Species = sp,
                               Year = y, 
                               overwriteFig = TRUE,
                               birdSolPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               booSolPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               Scenario = scen,
                               forceUpload = TRUE,
                               outPath = "/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS",
                               uploadTo = individualSpFolder)
          }
        })
      })
    if (redoIndividualModels){
      # INDIVIDUAL MODELS
      tableGLM <- Copy(finalTable4)
      subTableGLM <- tableGLM[Species == sp, ]
      m2 <- lm(umbrellaIndex ~ ProportionAreaChosen + Year, 
               data = subTableGLM)
      fileName <- file.path("~/projects/NWT/outputs/landscapeRuns/LandR.CS_fS/",
                            paste0(sp, "_umbrellaThroughTime.csv"))
      capture.output(summary(m2), file = fileName)
      allFls <- drive_ls(as_id(individualSpFolder))
      fileThere <- any(grepl(pattern = basename(fileName), 
                             x = allFls$name))
      if (!fileThere)
        drive_upload(fileName, as_id(individualSpFolder))
      # INDIVIDUAL BOXPLOT 
      tableIndvPlots <- finalTableSP[Species == sp, ]
      finalPlot <- ggplot(data = tableIndvPlots, aes(x = Year, 
                                                     y = umbrellaIndex)) +
        geom_jitter(aes(color = ClimateScenario)) +
        scale_color_manual(name = "Climate Model", 
                           values = Cls) +
        facet_grid(cols = vars(ProportionAreaChosen),
                   rows = vars(Species)) +
        xlab("Time") + 
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_hline(yintercept = 1, linetype = "dashed") +
        ylab(paste0("Umbrella Index")) +
        coord_cartesian(ylim = c(min(tableIndvPlots[["umbrellaIndex"]])-0.01, 
                                 max(tableIndvPlots[["umbrellaIndex"]])+0.01)) +
        geom_smooth(method = 'lm', colour = "black") +
        theme_bw() +
        theme(legend.position = "bottom",
              axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
              legend.text = element_text(size = 12),
              axis.text = element_text(size = 12))
      fileNamePlot <- file.path(Paths$outputPath, 
                                paste0("speciesLM_", sp,".png"))
      ggsave(device = "png", filename = fileNamePlot, 
             width = 11, height = 8)
      fileThere <- any(grepl(pattern = basename(fileNamePlot), 
                             x = allFls$name))
      if (!fileThere)
        drive_upload(file.path(Paths$outputPath, paste0("speciesLM_", sp,".png")), 
                     as_id(individualSpFolder))
    }
     toc()
  })
  plan("sequential")
  toc()
}

fls <- list.files("/home/tmichele/projects/NWT/outputs/landscapeRuns/LandR.CS_fS", 
                  full.names = TRUE, pattern = ".png")[1:50]

lapply(fls, drive_upload, as_id(individualSpFolder))
