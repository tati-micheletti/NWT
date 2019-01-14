library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)
library(data.table)
library(reshape2)

load("F:/BAM/BAMData/data_package_2016-04-18.Rdata")	
load("F:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
w <-"G:/Boreal/NationalModelsV2/Quebec/"
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))
QCSS <- SSLCC[SSLCC$JURS=="QC",]

# offl <- data.table(melt(OFF))
# names(offl) <- c("PKEY","SPECIES","logoffset")
# offl$SPECIES <- as.character(offl$SPECIES)
# offl$PKEY <- as.character(offl$PKEY)
# write.csv(offl,file=paste(w,"BAMoffsets.csv",sep=""))
                       
eco <- raster("F:/GIS/ecoregions/CEC/quebececo1.tif")
nalc <- raster("F:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
quebec <- raster("F:/GIS/basemaps/quebec250m1.tif")
urbag <- raster("G:/Boreal/NationalModelsV2/urbag2011_lcc1.tif")
uaq <- crop(urbag,quebec)
dev25 <- focal(uaq, fun=mean, w=w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
lf <- raster("D:/NorthAmerica/topo/lf_lcc1.tif")
lfq <- crop(lf,quebec)
#hli <- raster("D:/NorthAmerica/topo/nahli_lcc1.tif")
wat <- raster("G:/Boreal/NationalModelsV2/wat2011_lcc1.tif")
watq <- crop(wat,quebec)
led25 <- focal(watq, fun=mean, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)

b2011 <- list.files("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2011/",pattern="tif$")
setwd("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2011/")
bs2011 <- stack(raster(b2011[1]))
for (i in 2:length(b2011)) {bs2011 <- addLayer(bs2011, raster(b2011[i]))}
names(bs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(bs2011))
qbs2011 <- crop(bs2011,quebec)

qbs2011_1km <- aggregate(qbs2011, fact=4, fun=mean)
wat1km <- resample(watq, qbs2011_1km, method='ngb')
qbs2011_1km <- addLayer(qbs2011_1km, wat1km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "wat"
led251km <- resample(led25, qbs2011_1km, method='bilinear')
qbs2011_1km <- addLayer(qbs2011_1km, led251km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "led25"
urbag1km <- resample(uaq, qbs2011_1km, method='ngb')
qbs2011_1km <- addLayer(qbs2011_1km, urbag1km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "urbag"
# ecor1km <- resample(eco, qbs2011_1km, method='ngb')
# qbs2011_1km <- addLayer(qbs2011_1km, ecor1km)
# names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "eco"
dev251km <- resample(dev25, qbs2011_1km, method='bilinear')
qbs2011_1km <- addLayer(qbs2011_1km, dev251km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "dev25"
lf_1km <- resample(lfq, qbs2011_1km, method='ngb')
qbs2011_1km <- addLayer(qbs2011_1km, lf_1km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "landform"
writeRaster(qbs2011_1km,file=paste(w,"QC2011rasters",sep=""),overwrite=TRUE)

qbs2011 <- addLayer(qbs2011,watq)
names(qbs2011)[nlayers(qbs2011)] <- "wat"
qbs2011 <- addLayer(qbs2011,led25)
names(qbs2011)[nlayers(qbs2011)] <- "led25"
qbs2011 <- addLayer(qbs2011,uaq)
names(qbs2011)[nlayers(qbs2011)] <- "urbag"
qbs2011 <- addLayer(qbs2011,dev25)
names(qbs2011)[nlayers(qbs2011)] <- "dev25"
lf250 <- resample(lfq, qbs2011, method='ngb')
qbs2011 <- addLayer(qbs2011, lf250)
names(qbs2011)[nlayers(qbs2011)] <- "landform"
writeRaster(qbs2011,file=paste(w,"QC2011rasters250",sep=""),overwrite=TRUE)

b2001 <- list.files("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/",pattern="tif$")
setwd("F:/GIS/landcover/Beaudoin/Processed_sppBiomass/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) { bs2001 <- addLayer(bs2001, raster(b2001[i]))}
names(bs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(bs2001))
qbs2001 <- crop(bs2001,quebec)

qbs2001 <- addLayer(qbs2001,watq)
names(qbs2001)[nlayers(qbs2001)] <- "wat"
qbs2001 <- addLayer(qbs2001,led25)
names(qbs2001)[nlayers(qbs2001)] <- "led25"
qbs2001 <- addLayer(qbs2001,uaq)
names(qbs2001)[nlayers(qbs2001)] <- "urbag"
qbs2001 <- addLayer(qbs2001,dev25)
names(qbs2001)[nlayers(qbs2001)] <- "dev25"
lf250 <- resample(lfq, qbs2001, method='ngb')
qbs2001 <- addLayer(qbs2001, lf250)
names(qbs2001)[nlayers(qbs2001)] <- "landform"
writeRaster(qbs2001,file=paste(w,"QC2001rasters250",sep=""),overwrite=TRUE)

dat2011 <- cbind(QCSS, extract(qbs2011,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(eco,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"
dat2011 <-cbind(dat2011,extract(lfq,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "landform"

samprast2011 <- rasterize(cbind(dat2011$X,dat2011$Y), led25, field=1)
gf <- focalWeight(samprast2011, 100, "Gauss")
sampsum25 <- focal(samprast2011, w=gf, na.rm=TRUE)
dat2011 <- cbind(dat2011,extract(sampsum25,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "sampsum25"
dat2011$wt <- 1/dat2011$sampsum25
dat2011$SS <- as.character(dat2011$SS)
dat2011$PCODE <- as.character(dat2011$PCODE)
write.csv(dat2011,paste(w,"QCdat2011.csv",sep=""),row.names=FALSE)

dat2001 <- cbind(QCSS, extract(qbs2001,as.matrix(cbind(QCSS$X,QCSS$Y))))
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
write.csv(dat2001,paste(w,"QCdat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCTBL[,2:10],PKEY[,1:8],by=c("PKEY","SS")) #n=5808402
QCPC <- inner_join(PC, QCSS[,c(2,5)], by=c("SS")) #n=465693
QCPC$SS <- as.character(QCPC$SS)
QCPC$PKEY <- as.character(QCPC$PKEY)
QCPC$PCODE <- as.character(QCPC$PCODE)
QCPC$SPECIES <- as.character(QCPC$SPECIES)
QCPC2001 <- QCPC[QCPC$YEAR < 2006,] #n=212901
QCPC2011 <- QCPC[QCPC$YEAR > 2005,] #n=252792
write.csv(QCPC2011,paste(w,"QCPC2011.csv",sep=""),row.names=FALSE)
write.csv(QCPC2001,paste(w,"QCPC2001.csv",sep=""),row.names=FALSE)
