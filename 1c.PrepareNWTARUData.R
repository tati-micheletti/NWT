library(raster)

load("F:/BAM/BAMData/nwt-pkey-table-2019-01-14.RData")
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(pk) <- c("X", "Y") 
proj4string(pk) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
PKLCC <- as.data.frame(spTransform(pk, LCC))

landcov <- raster("F:/BAM/BAMData/calculate_offsets/lc052010.tif")
treecov <- raster("F:/BAM/BAMData/calculate_offsets/treecoverlcc.tif")
dat <- cbind(PKLCC, extract(landcov,as.matrix(cbind(PKLCC$X,PKLCC$Y))))
names(dat)[ncol(dat)] <- "landcov"
dat <- cbind(dat, extract(treecov,as.matrix(cbind(dat$X,dat$Y))))
names(dat)[ncol(dat)] <- "treecov"

write.csv(dat,file="F:/BAM/BAMData/calculate_offsets/NWTpkey.csv",row.names=FALSE)

wildtrax <- read.csv("F:/BAM/BAMData/ARU/speciesReport.csv")
wt <- na.omit(wildtrax[,1:20])
wt <- wt[wt$latitude > 0,]
coordinates(wt) <- c("longitude","latitude")
proj4string(wt) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
WTLCC <- as.data.frame(spTransform(wt, LCC))
names(WTLCC)[19:20] <- c("X","Y")
dat <- cbind(WTLCC, extract(landcov,as.matrix(cbind(WTLCC$X,WTLCC$Y))))
names(dat)[ncol(dat)] <- "landcov"
dat <- cbind(dat, extract(treecov,as.matrix(cbind(dat$X,dat$Y))))
names(dat)[ncol(dat)] <- "treecov"
