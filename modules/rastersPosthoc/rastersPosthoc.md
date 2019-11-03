---
title: "birdsPosthoc"
author: "tmichele"
date: "29 October 2019"
output: pdf_document
---

# Overview

Provide an overview of what the module does / how to use the module.

Module documentation should be written so that others can use your module.
This is a template for module documentation, and should be changed to reflect your module.

## R Markdown

R Markdown syntax allows R code, outputs, and figures to be rendered in the documentation.

For help writing in R Markdown, see http://rmarkdown.rstudio.com/.

# Usage


```r
  library("SpaDES")
```

```
## Warning: no DISPLAY variable so Tk is not available
```

```
## Paths set to:
##   options(
##     rasterTmpDir = '/tmp/RtmpYGcycv/raster'
##     reproducible.cachePath = '/tmp/RtmpYGcycv/reproducible/cache'
##     spades.inputPath = '/tmp/Rtmp0C7x0G/SpaDES/inputs'
##     spades.outputPath = '/tmp/Rtmp0C7x0G/SpaDES/outputs'
##     spades.modulePath = '/tmp/Rtmp0C7x0G/SpaDES/modules'
##   )
```

```
## loading reproducible     0.2.10.9006
## loading quickPlot        0.1.6.9000
## loading SpaDES.core      0.2.6.9006
## loading SpaDES.tools     0.3.2.9002
## loading SpaDES.addins    0.1.2
```

```
## Default paths for SpaDES directories set to:
##   cachePath:  
##   inputPath:  /tmp/Rtmp0C7x0G/SpaDES/inputs
##   modulePath: /tmp/Rtmp0C7x0G/SpaDES/modules
##   outputPath: /tmp/Rtmp0C7x0G/SpaDES/outputs
## These can be changed using 'setPaths()'. See '?setPaths'.
```

```r
  # library("future")
  # library("future.apply")
  # plan("multiprocess")
  devtools::load_all("/mnt/data/Micheletti/usefun/")
```

```
## Loading usefun
```

```r
  if (pemisc::user() %in% c("Tati", "tmichele"))
    setwd("/mnt/data/Micheletti/NWT")
  
  setPaths(modulePath = file.path(getwd(), "modules"),
           inputPath = file.path(getwd(), "outputs/23OCT19/"),
           outputPath = checkPath(file.path(getwd(), "outputs/23OCT19/birdResults"), create = TRUE)
  )
```

```
## Setting:
##   options(
##     spades.inputPath = '/mnt/data/Micheletti/NWT/outputs/23OCT19'
##     spades.outputPath = '/mnt/data/Micheletti/NWT/outputs/23OCT19/birdResults'
##     spades.modulePath = '/mnt/data/Micheletti/NWT/modules'
##   )
```

```
## Paths set to:
##   options(
##     rasterTmpDir = '/tmp/RtmpYGcycv/raster'
##     reproducible.cachePath = '/tmp/RtmpYGcycv/reproducible/cache'
##     spades.inputPath = '/mnt/data/Micheletti/NWT/outputs/23OCT19'
##     spades.outputPath = '/mnt/data/Micheletti/NWT/outputs/23OCT19/birdResults'
##     spades.modulePath = '/mnt/data/Micheletti/NWT/modules'
##   )
```

```r
  getPaths() # shows where the 4 relevant paths are
```

```
## $cachePath
## [1] "/tmp/RtmpYGcycv/reproducible/cache"
## 
## $inputPath
## [1] "/mnt/data/Micheletti/NWT/outputs/23OCT19/"
## 
## $modulePath
## [1] "/mnt/data/Micheletti/NWT/modules"
## 
## $outputPath
## [1] "/mnt/data/Micheletti/NWT/outputs/23OCT19/birdResults"
## 
## $rasterPath
## [1] "/tmp/RtmpYGcycv/raster//"
```

```r
  times <- list(start = 1, end = 1)
  Run <- "run1"
  species <- c("RUBL", "CAWA", "OSFL", "BBWA", "WEWP")
  parameters <- list(
    rastersPosthoc = list(patternsToRetrieveRasters = c("predicted", "Year", "tif"),
                          patternsUsedForGrouping = species,
                          years = c(seq(2011, 2100, by = 30), 2100),
                          species = species, 
                          n = 50,
                          sampleSize = 45)
    )
  modules <- list("rastersPosthoc")
  objectsV6 <- list(dataFolder = list(
                  LandR.CS_fS_V6 = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "birdPredictions"),
                  LandR_fS_V6 = file.path(getPaths()$inputPath, "LandR_fS", Run, "birdPredictions"),
                  LandR.CS_SCFM_V6 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "birdPredictions"),
                  LandR_SCFM_V6 = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "birdPredictions"))
                  )
  objectsV3 <- list(dataFolder = list(
                  LandR.CS_fS_V3 = file.path(getPaths()$inputPath, "LandR.CS_fS", Run, "birdPredictions"),
                  LandR_fS_V3 = file.path(getPaths()$inputPath, "LandR_fS", Run, "birdPredictions"),
                  LandR.CS_SCFM_V3 = file.path(getPaths()$inputPath, "LandR.CS_SCFM", Run, "birdPredictions"),
                  LandR_SCFM_V3 = file.path(getPaths()$inputPath, "LandR_SCFM", Run, "birdPredictions"))
                  )
  inputs <- list()
  outputs <- list(
    data.frame(
    objectName = c("deltaRasters",
                   "significantChanges",
                   "pixelsSummaries"),
    saveTime = times$end
  )
  )
  
  birdV3 <- simInit(times = times, params = parameters, modules = modules,
                   objects = objectsV6, outputs = outputs)
```

```
## Error in simInit(times = times, params = parameters, modules = modules, : simInit is incorrectly specified.  The outputs argument is specified incorrectly. It is expected to be outputs = data.frame
```

```r
birdResults <- spades(birdV3, debug = 1)
```

```
## Error in spades(birdV3, debug = 1): object 'birdV3' not found
```

```r
# 
# birdV3 <- simInit(times = times, params = parameters, modules = modules,
#                  objects = objectsV3)

# library(SpaDES.experiment)
# birdResults <- experiment2(birdV6, birdV3)

# devtools::install_github("PredictiveEcology/SpaDES.experiment@development")
```

# Events

Describe what happens for each event type.

## Plotting

Write what is plotted.

## Saving

Write what is saved.

# Data dependencies

## Input data

How to obtain input data, and a description of the data required by the module.
If `sourceURL` is specified, `downloadData("birdsPosthoc", "path/to/modules/dir")` may be sufficient.

## Output data

Description of the module outputs.

# Links to other modules

Describe any anticipated linkages to other modules.

