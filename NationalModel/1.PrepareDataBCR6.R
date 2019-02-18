library(raster)
library(maptools)
library(dplyr)
library(data.table)
library(reshape2)

LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
w <-"G:/Boreal/NationalModelsV2/BCR6/"
bcr6 <- raster("G:/Boreal/NationalModelsV2/BCR6/bcr6.tif")

# offl <- read.csv("G:/Boreal/NationalModelsV2/Quebec/BAMoffsets.csv")
# offla <- read.csv("G:/Boreal/NationalModelsV2/Quebec/Atlasoffsets.csv")

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
SSWT <- unique(dd[dd$Y>0,c(33,39:40)])
SSWT1 <- na.omit(SSWT)
coordinates(SSWT1) <- c("X", "Y") 
proj4string(SSWT1) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSWTLC <- as.data.frame(spTransform(SSWT1, LCC))
PKEYWT <- unique(dd[,c(33,34,36)])
#PCWT <- dd[,c(33,34,36,38,47)]
PCWT <- melt(y)
names(PCWT) <- c("PKEY","SPECIES","ABUND")
offWT <- data.table(melt(off))
names(offWT) <- c("PKEY","SPECIES","logoffset")
offWT$SPECIES <- as.character(offWT$SPECIES)
offWT$PKEY <- as.character(offWT$PKEY)
write.csv(offWT,file="G:/Boreal/NationalModelsV2/BCR6/offwt.csv", row.names=FALSE)

load("F:/BAM/BamData/ARU/nwt-BU-offsets-2019-01-14.RData")
SSBU <- unique(dd[,c(14,20:21)])
coordinates(SSBU) <- c("X", "Y") 
proj4string(SSBU) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSBULC <- as.data.frame(spTransform(SSBU, LCC))
PKEYBU <- unique(dd[,c(14,15,17)])
PCBU <- melt(y)
names(PCBU) <- c("PKEY","SPECIES","ABUND")
offBU <- data.table(melt(off))
names(offBU) <- c("PKEY","SPECIES","logoffset")
offBU$SPECIES <- as.character(offBU$SPECIES)
offBU$PKEY <- as.character(offBU$PKEY)
write.csv(offBU,file="G:/Boreal/NationalModelsV2/BCR6/offbu.csv", row.names=FALSE)

SScombo <- rbind(SSBAM[,c(2,48,49)],SSAtlas[,c(1,6,7)],SSWTLC,SSBULC)
# SSARU <- rbind(SSWT,SSBU)
# write.csv(SSARU,file=paste(w,"SSARU.csv",sep=""),row.names=FALSE)
SSBCR6 <- cbind(SScombo, extract(bcr6, as.matrix(cbind(SScombo$X,SScombo$Y)))) #n=311518
SSBCR6 <- na.omit(SSBCR6) #n=49043
SSBCR6 <- SSBCR6[,1:3]
PKEYcombo <- rbind(PKEYBAM[,c(1,2,8)],PKEYAtlas[,c(1,2,4)],PKEYWT,PKEYBU)
PCcombo <- rbind(PCBAM[,c(3:5)], PCAtlas[,c(2,4:5)], PCWT, PCBU) 

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
bs2011bcr6 <- mask(bs2011bcr6,bcr6)
bs2011bcr6 <- dropLayer(bs2011bcr6, c(1,2,3,4,5,7,8,9,11,12,13,14,15,16,17,18,19,20,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,46,47,48,49,52,53,54,55,56,57,59,60,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,90,91,92,93))

# bs2011bcr6_1km <- aggregate(bs2011bcr6, fact=4, fun=mean)
# wat1km <- resample(wat6, bs2011bcr6_1km, method='ngb')
# bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, wat1km)
# names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "wat"
# led251km <- resample(led25, bs2011bcr6_1km, method='bilinear')
# bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, led251km)
# names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "led25"
# urbag1km <- resample(ua6, bs2011bcr6_1km, method='ngb')
# bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, urbag1km)
# names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "urbag"
# dev251km <- resample(dev25, bs2011bcr6_1km, method='bilinear')
# bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, dev251km)
# names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "dev25"
# lf_1km <- resample(lf6, bs2011bcr6_1km, method='ngb')
# bs2011bcr6_1km <- addLayer(bs2011bcr6_1km, lf_1km)
# names(bs2011bcr6_1km)[nlayers(bs2011bcr6_1km)] <- "landform"
# writeRaster(bs2011bcr6_1km,file=paste(w,"bcr6_2011rasters",sep=""),overwrite=TRUE)

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
#bs2011bcr6<-stack(paste(w,"bcr6_2011rasters250",sep=""))

b2001 <- list.files("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/",pattern="tif$")
setwd("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) {bs2001 <- addLayer(bs2001, raster(b2001[i]))}
names(bs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(bs2001))
bs2001bcr6 <- crop(bs2001,bcr6)
bs2001bcr6 <- mask(bs2001bcr6,bcr6)
bs2001bcr6 <- dropLayer(bs2001bcr6, c(1,2,3,4,5,7,8,9,11,12,13,14,15,16,17,18,19,20,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,39,40,41,42,43,46,47,48,49,52,53,54,55,56,57,59,60,62,63,64,65,66,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,90,91,92,93))

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
#bs2001bcr6<-stack(paste(w,"bcr6_2001rasters250",sep=""))

bs2011bcr6 <- dropLayer(bs2011bcr6,18)
dat2011 <- cbind(SSBCR6, extract(bs2011bcr6,as.matrix(cbind(SSBCR6$X,SSBCR6$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(lf6,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "landform"
dat2011$SS <- as.character(dat2011$SS)
dat2011 <- na.omit(dat2011)
dat_2011 <- inner_join(dat2011, PKEYcombo[,2:3], by=c("SS")) #n=150051
dat_2011 <- distinct(dat_2011[dat_2011$YEAR > 2005,1:22]) #n=32130
write.csv(dat_2011,paste(w,"bcr6_dat2011.csv",sep=""),row.names=FALSE)

bs2001bcr6 <- dropLayer(bs2001bcr6,18)
dat2001 <- cbind(SSBCR6, extract(bs2001bcr6,as.matrix(cbind(SSBCR6$X,SSBCR6$Y))))
dat2001 <-cbind(dat2001,extract(nalc,as.matrix(cbind(dat2001$X,dat2001$Y)))) 
names(dat2001)[ncol(dat2001)] <- "LCC"
dat2001 <-cbind(dat2001,extract(lf6,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "landform"
dat2001$SS <- as.character(dat2001$SS)
dat2001 <- na.omit(dat2001)
dat_2001 <- inner_join(dat2001, PKEYcombo[,2:3], by=c("SS")) #n=150051
dat_2001 <- distinct(dat_2001[dat_2001$YEAR < 2006,1:22]) #n=18945
write.csv(dat_2001,paste(w,"bcr6_dat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCcombo,PKEYcombo,by=c("PKEY")) #n=7707578
PCBCR6 <- inner_join(PC, SSBCR6[,1:3], by=c("SS")) #n=917019
PCBCR6$SS <- as.character(PCBCR6$SS)
PCBCR6$PKEY <- as.character(PCBCR6$PKEY)
PCBCR6$SPECIES <- as.character(PCBCR6$SPECIES)
PCBCR62001 <- PCBCR6[PCBCR6$YEAR < 2006,] #n=394560
PCBCR62011 <- PCBCR6[PCBCR6$YEAR > 2005,] #n=522459
write.csv(PCBCR62011,paste(w,"BCR6PC2011.csv",sep=""),row.names=FALSE)
write.csv(PCBCR62001,paste(w,"BCR6PC2001.csv",sep=""),row.names=FALSE)
