library("raster")
library("data.table")
library("reproducible")

NWT.url <- "https://drive.google.com/open?id=1LUxoY2-pgkCmmNH5goagBp3IMpj6YrdU"
# EDE.url <- "https://drive.google.com/open?id=1fYvNPwovjNtTABoGcegrvdFGkNfCUsxf"
studyArea <- Cache(prepInputs,
                   url = NWT.url,
                   destinationPath = tempdir(),
                   userTags = "edeSA",
                   omitArgs = c("destinationPath"))

rasterToMatch <- Cache(prepInputs, url = "https://drive.google.com/open?id=1fo08FMACr_aTV03lteQ7KsaoN9xGx1Df",
                       studyArea = studyArea,
                       targetFile = "RTM.tif", destinationPath = tempdir(),
                       filename2 = NULL,
                       userTags = "edeRTM",
                       omitArgs = c("destinationPath", "filename2"))
# rasterToMatch2 <- rasterToMatch
# res(rasterToMatch2) <- c(100, 100)
# rasterToMatch <- Cache(raster::resample, rasterToMatch, rasterToMatch2, userTags = "rtmHD")

vegMap <- LandR::prepInputsLCC(year = 2005,
                               destinationPath = tempdir(),
                               studyArea = studyArea,
                               rasterToMatch = rasterToMatch,
                               filename2 = NULL,
                               useCache = FALSE)

rstCurrentBurn2011 <- createRstCurrBurn(readRDS(file.path(folderPath, "burnDT_year2011.rds")), vegMap)
rstCurrentBurn2041 <- createRstCurrBurn(readRDS(file.path(folderPath, "burnDT_year2041.rds")), vegMap)
rstCurrentBurn2091 <- createRstCurrBurn(readRDS(file.path(folderPath, "burnDT_year2091.rds")), vegMap)
quickPlot::Plot(rstCurrentBurn2011)
quickPlot::Plot(rstCurrentBurn2041)
quickPlot::Plot(rstCurrentBurn2091)
