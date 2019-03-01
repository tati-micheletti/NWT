---
title: "birdsNWT"
author: ""
date: "04 February 2019"
output: pdf_document
---

# Overview

The birds module for tyhe NWT loads an existing glm object for each sp located in  
https://drive.google.com/open?id=1obSvU4ml8xa8WMQhQprd6heRrN47buvI. 
The model uses dynamic (vegetation: biomass of tree species) and static
layers (Non-veg: WAT = water (1/0), URBAG = urban/agriculture (1/0),
lLED25 = lake edge density with 5x5 moving window (continuous), 
DEV25 = development proportion within 5x5 moving window (continuous),
and landform (categorical). North American landcover 2005 (MODIS) is
source for all but landform (from AdaptWest land facet datset). 
Vector ruggedness (already available - 
https://drive.google.com/open?id=1dgIw70mDpDYrBExA52SkPoS1TFaZdLE9) 
and road density (that should be available from the Anthropogenic
module) will be added to the next version of these models. The birds' 
prediction is masked to uplands as we do not have data for lowlands.

### Update and/or install all needed packages

If you don't have all packages installed yet, please 
first update all your packages and then install SpaDES. 
Make sure you restart your session after installing all packages.

```{r github, include=FALSE, eval = FALSE}
knitr::opts_chunk$set(echo = TRUE)

update.packages(checkBuilt = TRUE)
devtools::install_github("PredictiveEcology/reproducible@development")
devtools::install_github("achubaty/amc@development")
devtools::install_github("PredictiveEcology/pemisc@development")
devtools::install_github("PredictiveEcology/map@development")
devtools::install_github("PredictiveEcology/LandR@development") # Updates SpaDES.tools and SpaDES.core quickPlot
```

### Please, make sure you loaded the project file in RStudio. This file can be found in "NWT/modules/birdsNWT/" ()

### Module Usage

```{r module_usage}

# Load SpaDES
library(SpaDES)

# Source functions in R folder
invisible(sapply(X = list.files(file.path(getwd(), "R"), full.names = TRUE), FUN = source))

# Set a storage project folder
workDirectory <- getwd()
message("Your current temporary directory is ", tempdir())

# Set up paths
paths <- list(
inputPath = file.path(workDirectory, "inputs"),
outputPath = file.path(workDirectory, "outputs"),
modulePath = file.path(workDirectory, "modules"),
cachePath <- file.path(workDirectory, "cache")
)

SpaDES.core::setPaths(modulePath = paths$modulePath, inputPath = paths$inputPath, 
                      outputPath = paths$outputPath, cachePath = paths$cachePath)

times <- list(start = 0, end = 20)

parameters <- list(
  .progress = list(type = "text", interval = 1), # for a progress bar
birdsNWT = list(
    "baseLayer" = 2005,
    "overwritePredictions" = TRUE,
    "useTestSpeciesLayers" = FALSE,
    "useParallel" = TRUE
  )
)

# Check the list of species available:
showAvailableBirdSpecies()

.objects <- list(
  "birdsList" = c("BBWA", "BOCH", "RBNU")) ### Inform which species should this be run for.
modules <- list("birdsNWT")
inputs <- list()
outputs <- list()

birdsNWT <- simInitAndSpades(times = times, params = parameters, modules = modules,
                 objects = .objects, debug = 2)

```

### Retrieve results:

The bird prediction rasters can be found in `birdsNWT$birdPrediction`:

```{r results1}

ls.str(birdsNWT$birdPrediction)

```

The bird models can be accessed using `birdsNWT$birdModels`:

```{r results2}

ls.str(birdsNWT$birdModels)

```

At last, static and succession Layers can be seen using `birdsNWT$staticLayers` and `birdsNWT$successionLayers`, respectively:

```{r results3}

ls.str(birdsNWT$staticLayers)
ls.str(birdsNWT$successionLayers)

```
