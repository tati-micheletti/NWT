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
