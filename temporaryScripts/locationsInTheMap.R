fl <- list.files(folder, full.names = TRUE, pattern = "changeInSpecies")
fl <- list.files(folder, full.names = TRUE, pattern = "hotspots3")
fl <- list.files(folder, full.names = TRUE, pattern = "birdPlot")

lapply(fl, drive_upload, path = as_id("1dlfMktYIkHYXl7AdPvh61RPjDGkbgKB5"))
lapply(file.path(getwd(), "locations.csv"), drive_upload, path = as_id("1dlfMktYIkHYXl7AdPvh61RPjDGkbgKB5"))

loc <- prepInputs(url = "https://drive.google.com/file/d/1S3fs8owJ7_Ss7PBZbmJoPSDD9qKgwRYR/view?usp=sharing",
           fun = "load", destinationPath = tempdir(), targetFile = "BAM_data_package_November2019.RData")
loc <- get(loc)

locs <- data.table(loc$SScombo)

locsIhave <- fread(file.path(getwd(), "locations.csv"), header = TRUE)
locs2 <- copy(locs)
locs2 <- locs2[SS %in% locsIhave$SS,]

allLocations <- unique(merge(locsIhave, locs2, by = "SS"))
setkey(allLocations, "V1")

dups <- which(duplicated(allLocations, by = "V1"))
dupsV1 <- allLocations[dups, V1]
allLocations2 <- allLocations[!V1 %in% dupsV1, ]

xy <- allLocations2[, c("X","Y")]

spdf <- SpatialPointsDataFrame(coords = xy, data = allLocations2,
                               proj4string = loc$LCC)
prov <- prepInputs(url = "http://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/files-fichiers/gpr_000b11a_e.zip",
                   destinationPath = tempdir())

pointCountLocations <- projectInputs(spdf, targetCRS = crs(prov)) 

provToKeep <- c("British Columbia", "Saskatchewan", "Manitoba", "Northwest Territories", "Alberta")

prov2 <- prov[prov$PRENAME %in% provToKeep,]

plot(prov2)
plot(pointCountLocations, add = TRUE, col = "red")

library("spatialEco")
pts.poly <- point.in.poly(pointCountLocations, prov2)
pointCounts <- as.data.table(pts.poly)
pointCounts <- data.table(table(pointCounts$PRENAME, useNA = "always"))
names(pointCounts) <- c("Province", "Count")
pointCounts[, proportion := round(Count/sum(Count), 5)]

write.csv(pointCounts, file.path(getwd(), "proportionOfPointsPerLocation.csv"))
lapply(file.path(getwd(), "proportionOfPointsPerLocation.csv"), 
       drive_upload, path = as_id("1dlfMktYIkHYXl7AdPvh61RPjDGkbgKB5"))
