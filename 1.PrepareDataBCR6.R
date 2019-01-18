library(raster)
library(maptools)
library(dplyr)
library(data.table)
library(reshape2)

LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
w <-"G:/Boreal/NationalModelsV2/BCR6/"
bcr6 <- raster("G:/Boreal/NationalModelsV2/BCR6/bcr6.tif")

offl <- read.csv("G:/Boreal/NationalModelsV2/Quebec/BAMoffsets.csv")
offla <- read.csv("G:/Boreal/NationalModelsV2/Quebec/Atlasoffsets.csv")

load("F:/BAM/BAMData/data_package_2016-04-18.Rdata")	
load("F:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSBAM <- as.data.frame(spTransform(SS, LCC))
PCBAM <- PCTBL
PKEYBAM <- PKEY

load("F:/BAM/BAMData/atlas_data_processed-20181018.RData")
SS <- na.omit(SS)
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSAtlas <- as.data.frame(spTransform(SS, LCC))
PCAtlas <- PCTBL
PKEYAtlas <- PKEY
names(PKEYAtlas)[4] <- "YEAR"

load("F:/BAM/BamData/ARU/nwt-wildtrax-offsets-2019-01-16.RData")
SSWT <- unique(dd[,c(33,39:40)])
PKEYWT <- unique(dd[,c(33,34,36)])
#PCWT <- dd[,c(33,34,36,38,47)]
PCWT <- melt(y)
names(PCBU) <- c("PKEY","SPECIES","ABUND")
offWT <- data.table(melt(off))
names(offWT) <- c("PKEY","SPECIES","logoffset")
offWT$SPECIES <- as.character(offWT$SPECIES)
offWT$PKEY <- as.character(offWT$PKEY)

load("F:/BAM/BamData/ARU/nwt-BU-offsets-2019-01-14.RData")
SSBU <- unique(dd[,c(14,20:21)])
PKEYBU <- unique(dd[,c(14,15,17)])
PCBU <- melt(y)
names(PCBU) <- c("PKEY","SPECIES","ABUND")
offBU <- data.table(melt(off))
names(offBU) <- c("PKEY","SPECIES","logoffset")
offBU$SPECIES <- as.character(offBU$SPECIES)
offBU$PKEY <- as.character(offBU$PKEY)

SScombo <- rbind(SSBAM[,c(2,48,49)],SSAtlas[,c(1,6,7)],SSWT,SSBU)
PKEYcombo <- rbind(PKEYBAM[,c(1,2,8)],PKEYAtlas[,c(1,2,4)],PKEYWT,PKEYBU)

eco <- raster("F:/GIS/ecoregions/CEC/quebececo1.tif")
nalc <- raster("F:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
bcr6 <- raster("G:/Boreal/NationalModelsV2/BCR6/bcr6.tif")
urbag <- raster("G:/Boreal/NationalModelsV2/urbag2011_lcc1.tif")
ua6 <- crop(urbag,bcr6)
ua6 <- mask(ua6,bcr6)
dev25 <- focal(ua6, fun=mean, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
wat <- raster("G:/Boreal/NationalModelsV2/wat2011_lcc1.tif")
wat6 <- crop(wat,bcr6)
wat6 <- mask(wat6,bcr6)
led25 <- focal(wat6, fun=mean, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)

lf <- raster("D:/NorthAmerica/topo/lf_lcc1.tif")
lf6 <- crop(lf,bcr6)

b2011 <- list.files("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2011/",pattern="tif$")
setwd("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2011/")
bs2011 <- stack(raster(b2011[1]))
for (i in 2:length(b2011)) {bs2011 <- addLayer(bs2011, raster(b2011[i]))}
names(bs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(bs2011))
bs2011bcr6 <- crop(bs2011,bcr6)
bs2011bcf6 <- mask(bs2011bcr6,bcr6)

bs2011bcr6_1km <- aggregate(bs2011bcr6, fact=4, fun=mean)
wat1km <- resample(wat6, bs2011bcr6_1km, method='ngb')
bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, wat1km)
names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "wat"
led251km <- resample(led25, bs2011bcr6_1km, method='bilinear')
bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, led251km)
names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "led25"
urbag1km <- resample(ua6, bs2011bcr6_1km, method='ngb')
bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, urbag1km)
names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "urbag"
dev251km <- resample(dev25, bs2011bcr6_1km, method='bilinear')
bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, dev251km)
names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "dev25"
lf_1km <- resample(lf6, bs2011bcr6_1km, method='ngb')
bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, lf_1km)
names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "landform"
writeRaster(bs2011bcr6_1km,file=paste(w,"bcr6_2011rasters",sep=""),overwrite=TRUE)

bs2011bcr6 <- addLayer(bs2011bcr6,wat6)
names(bs2011bcr6)[nlayers(bs2011bcr6)] <- "wat"
bs2011bcr6 <- addLayer(bs2011bcr6,led25)
names(bs2011bcr6)[nlayers(bs2011bcr6)] <- "led25"
bs2011bcr6 <- addLayer(bs2011bcr6,ua6)
names(bs2011bcr6)[nlayers(bs2011bcr6)] <- "urbag"
bs2011bcr6 <- addLayer(bs2011bcr6,dev25)
names(bs2011bcr6)[nlayers(bs2011bcr6)] <- "dev25"
lf250 <- resample(lf6, bs2011bcr6, method='ngb')
bs2011bcr6 <- addLayer(bs2011bcr6, lf250)
names(bs2011bcr6)[nlayers(bs2011bcr6)] <- "landform"
writeRaster(bs2011bcr6,file=paste(w,"bcr6_2011rasters250",sep=""),overwrite=TRUE)

b2001 <- list.files("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/",pattern="tif$")
setwd("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) {bs2001 <- addLayer(bs2001, raster(b2001[i]))}
names(bs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(bs2001))
bs2001bcr6 <- crop(bs2001,bcr6)
bs2001bcf6 <- mask(bs2001bcr6,bcr6)
# 
# bs2001bcr6_1km <- aggregate(bs2001bcr6, fact=4, fun=mean)
# wat1km <- resample(wat6, bs2001bcr6_1km, method='ngb')
# bs2001bcr6_1km <- addLayer(bs2001bcr6_1km, wat1km)
# names(bs2001bcr6_1km)[nlayers(bs2001bcr6_1km)] <- "wat"
# led251km <- resample(led25, bs2001bcr6_1km, method='bilinear')
# bs2001bcr6_1km <- addLayer(bs2001bcr6_1km, led251km)
# names(bs2001bcr6_1km)[nlayers(bs2001bcr6_1km)] <- "led25"
# urbag1km <- resample(ua6, bs2001bcr6_1km, method='ngb')
# bs2001bcr6_1km <- addLayer(bs2001bcr6_1km, urbag1km)
# names(bs2001bcr6_1km)[nlayers(bs2001bcr6_1km)] <- "urbag"
# dev251km <- resample(dev25, bs2001bcr6_1km, method='bilinear')
# bs2001bcr6_1km <- addLayer(bs2001bcr6_1km, dev251km)
# names(bs2001bcr6_1km)[nlayers(bs2001bcr6_1km)] <- "dev25"
# lf_1km <- resample(lf6, bs2001bcr6_1km, method='ngb')
# bs2001bcr6_1km <- addLayer(bs2001bcr6_1km, lf_1km)
# names(bs2001bcr6_1km)[nlayers(bs2001bcr6_1km)] <- "landform"
# writeRaster(bs2001bcr6_1km,file=paste(w,"bcr6_2001rasters",sep=""),overwrite=TRUE)

bs2001bcr6 <- addLayer(bs2001bcr6,wat6)
names(bs2001bcr6)[nlayers(bs2001bcr6)] <- "wat"
bs2001bcr6 <- addLayer(bs2001bcr6,led25)
names(bs2001bcr6)[nlayers(bs2001bcr6)] <- "led25"
bs2001bcr6 <- addLayer(bs2001bcr6,ua6)
names(bs2001bcr6)[nlayers(bs2001bcr6)] <- "urbag"
bs2001bcr6 <- addLayer(bs2001bcr6,dev25)
names(bs2001bcr6)[nlayers(bs2001bcr6)] <- "dev25"
lf250 <- resample(lf6, bs2001bcr6, method='ngb')
bs2001bcr6 <- addLayer(bs2001bcr6, lf250)
names(bs2001bcr6)[nlayers(bs2001bcr6)] <- "landform"
writeRaster(bs2001bcr6,file=paste(w,"bcr6_2001rasters250",sep=""),overwrite=TRUE)

bs2011bcr6 <- dropLayer(bs2011bcr6,98)
dat2011 <- cbind(SScombo, extract(bs2011bcr6,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(eco,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"
dat2011 <-cbind(dat2011,extract(lfq,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "landform"
dat2011$SS <- as.character(dat2011$SS)
dat2011$PCODE <- as.character(dat2011$PCODE)
write.csv(dat2011,paste(w,"bcr6_dat2011.csv",sep=""),row.names=FALSE)

bs2001bcr6 <- dropLayer(bs2001bcr6,98)
dat2001 <- cbind(SScombo, extract(bs2001bcr6,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2001 <-cbind(dat2001,extract(nalc,as.matrix(cbind(dat2001$X,dat2001$Y)))) 
names(dat2001)[ncol(dat2001)] <- "LCC"
dat2001 <-cbind(dat2001,extract(eco,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "eco"
dat2001 <-cbind(dat2001,extract(lfq,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "landform"

samprast2001 <- rasterize(cbind(dat2001$X,dat2001$Y), led25, field=1)
gf <- focalWeight(samprast2001, 100, "Gauss")
sampsum25 <- focal(samprast2001, w=gf, na.rm=TRUE)
dat2001 <- cbind(dat2001,extract(sampsum25,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "sampsum25"
dat2001$wt <- 1/dat2001$sampsum25
dat2001$SS <- as.character(dat2001$SS)
dat2001$PCODE <- as.character(dat2001$PCODE)
write.csv(dat2001,paste(w,"bcr6_dat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCTBL[,2:10],PKEY[,1:8],by=c("PKEY","SS")) #n=5808402
QCPC <- inner_join(PC, QCSS[,c(2,5)], by=c("SS")) #n=465693
QCPC$SS <- as.character(QCPC$SS)
QCPC$PKEY <- as.character(QCPC$PKEY)
QCPC$PCODE <- as.character(QCPC$PCODE)
QCPC$SPECIES <- as.character(QCPC$SPECIES)
QCPC2001 <- QCPC[QCPC$YEAR < 2006,] #n=212901
QCPC2011 <- QCPC[QCPC$YEAR > 2005,] #n=252792
write.csv(QCPC2011,paste(w,"BCR6PC2011.csv",sep=""),row.names=FALSE)
write.csv(QCPC2001,paste(w,"BCR6PC2001.csv",sep=""),row.names=FALSE)
