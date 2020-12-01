options(repos = c(CRAN = "https://cloud.r-project.org"),  ## issues with RSPM
        Require.RPackageCache = "~/binaryRPackages")      ## change this to your tastes

if (!requireNamespace("Require", quietly = TRUE))
  install.packages("Require") # use version 0.0.9

# 0. Install working packages
library("Require")

pkgPath <- "./packages"
setLibPaths(pkgPath)

Require::Require(packageVersionFile = "./packageVersions.txt", standAlone = TRUE)

library("googledrive")
library("qs")
library("reproducible")
library("SpaDES.core")

# 1. Set Paths, parameters, times, modules
Times <- list(start = 2011, end = 2100)

# SAVE THE PARAMETERS AS qs

Parameters <- list(
  Biomass_speciesParameters = list(
    .useCache = c(".inputObjects",  "init"),
    sppEquivCol = "NWT_BCR6",
    GAMMiterations = 2,
    GAMMknots = list(Betu_Pap = 3, Lari_Lar = 4, Pice_Gla = 3, Pice_Mar = 4, Pinu_Ban = 3, Popu_Tre = 4),
    minimumPlotsPerGamm = 40,
    constrainMortalityShape = list(
      Betu_Pap = c(15, 25),
      Lari_Lar = c(20, 25),
      Pice_Gla = c(15, 25),
      Pice_Mar = c(15, 25),
      Pinu_Ban = c(15, 25),
      Popu_Tre = c(15, 25)
    ),
    quantileAgeSubset = list(Betu_Pap = 95, Lari_Lar = 95, Pice_Gla = 95, Pice_Mar = 95, Pinu_Ban = 95, Popu_Tre = 99)),
  Biomass_core = list(successionTimestep = 10, .plotInitialTime = NA,
                      .saveInitialTime = NA, .useCache = c(".inputObjects", "init"),
                      seedingAlgorithm = "wardDispersal", initialBiomassSource = "cohortData",
                      growthAndMortalityDrivers = "LandR", keepClimateCols = FALSE,  .useParallel = 2),
  Biomass_borealDataPrep = list(
    speciesUpdateFunction = list(
      quote(LandR::speciesTableUpdate(sim$species, sim$speciesTable,  sim$sppEquiv, P(sim)$sppEquivCol))
    ),
    useCloudCacheForStats = TRUE,
    sppEquivCol = "NWT_BCR6", successionTimestep = 10, pixelGroupAgeClass = 20,
    .useCache = c(".inputObjects", "init"), subsetDataBiomassModel = 50,  exportModels = "all"
  ),
  Biomass_regeneration = list(fireTimestep = 1, fireInitialTime = 2011,
                              .useCache = c(".inputObjects", "init")),
  fireSense_IgnitionPredict = list(.useCache = c(".inputObjects", "init"),
                                   data = c("MDC06", "LCC"),
                                   modelObjName = "fireSense_IgnitionFitted_year2011"),
  fireSense_EscapePredict = list(.useCache = c(".inputObjects", "init"), data = c("MDC06", "LCC"),
                                 modelObjName = "fireSense_EscapeFitted_year2011"),
  fireSense_SpreadPredict = list(.useCache = c(".inputObjects", "init"),
                                 modelObjName = "fireSense_SpreadFitted_year2011"),
  fireSense = list(mapping = list(spreadProbRaster = "fireSense_SpreadPredicted")),
  fireSense_dataPrep = list(
    train = FALSE,
    whichModulesToPrepare = c("fireSense_SpreadPredict",  "fireSense_IgnitionPredict"),
    RCP = "85",
    climateModel = "CCSM4",
    ensemble = "",
    climateResolution = "3ArcMin",
    climateFilePath = "https://drive.google.com/open?id=17idhQ_g43vGUQfT-n2gLVvlp0X9vo-R8"
  ),
  fireSense_SpreadFit = list(
    formula_fire = ~0 + weather + class1 + class2 + class3 + class4 + class5,
    fireAttributesFireSense_SpreadFit = "fireAttributesFireSense_SpreadFit",
    data = c("weather", "class1", "class2", "class3", "class4", "class5"),
    lower = c(0.22, 0.001, -16, -16, -16, -16, -16, -16),
    upper = c(0.29, 10, 32, 32, 32, 32, 32, 32),
    fireYears = 1991:2017, cores = NULL, iterDEoptim = 150,
    iterStep = 150, minBufferSize = 2000, debugMode = FALSE,
    rescaleAll = TRUE, NP = 80L, objFunCoresInternal = 3L,
    maxFireSpread = 0.3, objfunFireReps = 100, verbose = TRUE,
    trace = 1, visualizeDEoptim = TRUE,
    cacheId_DE = "40927e9ca42d33b3",
    cloudFolderID_DE = "1kUZczPyArGIIkbl-4_IbtJWBhVDveZFZ",
    useCloud_DE = TRUE
  )
)

Modules <- c("Biomass_borealDataPrep", "Biomass_speciesParameters",
             "Biomass_regeneration", "Biomass_core", "fireSense_dataPrep",
             "fireSense_IgnitionPredict", "fireSense_EscapePredict", "fireSense_SpreadPredict",
             "LBMR2LCC_DataPrep", "fireSense")
Paths <- list(cachePath = "./cache",
              inputPath = "./inputs",
              modulePath = "./modules",
              outputPath = "./outputs",
              rasterPath = "./scratch")
lapply(Paths, checkPath, create = TRUE)

# 2. Download the inputs
toDownloadInputs <- data.frame(
  ID = c("11WFSCtKxEiTdyXD_PIIrf5TKFT4Ex4Tn",
         "1_43Mv_M3zHL-ivv5xhl1gKbjlW6B0IQl",
         "1yqLpTGlnYagPJlZX3DEvKIpmJSHFtzBD",
         "1LJDKaFeD9BwypL-dvdbelTtiN739LZxl"),
  fileNames = c("fireSense_EscapeFitted_year2011.rds",
                "fireSense_IgnitionFitted_year2011.rds",
                "fireSense_SpreadFitted_year2011.rds",
                "objects.qs")
)
apply(toDownloadInputs, 1, function(row) {
  if (!file.exists(file.path(Paths$inputPath, row[2])))
    drive_download(file = as_id(row[1]), path = file.path(Paths$inputPath, row[2]))
})

# 3. Create the Inputs for the simulation
Inputs <- data.frame(
  files = c(file.path(Paths$inputPath, "fireSense_IgnitionFitted_year2011.rds"),
            file.path(Paths$inputPath, "fireSense_EscapeFitted_year2011.rds"),
            file.path(Paths$inputPath, "fireSense_SpreadFitted_year2011.rds")),
  functions = "base::readRDS",
  stringsAsFactors = FALSE
)
message("Following Inputs Loaded:")
print(Inputs)

# 4. Load the objects
Objects <- qs::qread(file.path(Paths$inputPath, "objects.qs"))

# 5. Create outputs list
succTS <- c(seq(Times$start, Times$end,
                by = Parameters$Biomass_core$successionTimestep), Times$end)

outputsSimulation <- data.frame(
  objectName = rep(c("burnMap",
                     "cohortData",
                     "simulationOutput",
                     "pixelGroupMap",
                     "simulatedBiomassMap",
                     "ANPPMap",
                     "mortalityMap",
                     "MDC06"), each = length(succTS)),
  saveTime = c(rep(succTS, times = 8))
)

lastYears <- data.frame(objectName = c("fireRegimeRas", "speciesEcoregion",
                                       "species", "gcsModel", "mcsModel",
                                       "spreadPredictedProbability"),
                        saveTime = Times$end)

clim <- data.frame(objectName = rep(c("fireSense_IgnitionPredicted",
                                        "fireSense_EscapePredicted", "burnSummary",
                                        "successionLayers", "activePixelIndex"),
                                      each = 3),
                     saveTime = rep(c(Times$start, round((Times$start + Times$end)/2, 0),
                                      Times$end),
                                    Times = 1))

outputsSimulation <- unique(rbind(outputsSimulation, lastYears, clim))

# 6. Create the simulation list
sim <- simInit(inputs = Inputs,
               times = Times,
               params = Parameters,
               modules = as.list(Modules),
               objects = Objects,
               paths = Paths,
               loadOrder = Modules,
               outputs = outputsSimulation)
