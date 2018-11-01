library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)
library(data.table)

load("I:/BAM/BAMData/data_package_2016-04-18.Rdata")	
load("I:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
w<-"I:/BAM/BAMData/"
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))
QCSS <- SSLCC[SSLCC$JURS=="QC",]

offl <- data.table(melt(OFF))
names(offl) <- c("PKEY","SPECIES","logoffset")
offl$SPECIES <- as.character(offl$SPECIES)
offl$PKEY <- as.character(offl$PKEY)
write.csv(offl,file=paste(w,"BAMoffsets.csv",sep=""))
                       
eco <- raster("I:/GIS/ecoregions/CEC/quebececo1.tif")
nalc <- raster("I:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
quebec <- raster("I:/GIS/basemaps/quebec250m1.tif")

b2011 <- list.files("I:/GIS/landcover/Beaudoin/2011/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2011/")
bs2011 <- stack(raster(b2011[1]))
for (i in 2:length(b2011)) {bs2011 <- addLayer(bs2011, raster(b2011[i]))}
names(bs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(bs2011))
qbs2011 <- crop(bs2011,quebec)
qbs2011_1km <- aggregate(qbs2011, fact=4, fun=mean)
r2 <- qbs2011_1km[[1]]

ecor1km <- resample(eco, qbs2011_1km)
qbs2011_1km <- addLayer(qbs2011_1km, ecor1km)
names(qbs2011_1km)[nlayers(qbs2011_1km)] <- "eco"
writeRaster(qbs2011_1km,file=paste(w,"QC2011rasters",sep=""))

b2001 <- list.files("I:/GIS/landcover/Beaudoin/2001/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) { bs2001 <- addLayer(bs2001, raster(b2001[i]))}
names(bs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(bs2001))
qbs2001 <- crop(bs2001,quebec)

dat2011 <- cbind(QCSS, extract(qbs2011,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(eco,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"

samprast2011 <- rasterize(cbind(dat2011$X,dat2011$Y), r2, field=1)
sampsum25 <- focal(samprast2011, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
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

samprast2001 <- rasterize(cbind(dat2001$X,dat2001$Y), r2, field=1)
sampsum25 <- focal(samprast2001, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
dat2001 <- cbind(dat2001,extract(sampsum25,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "sampsum25"
dat2001$wt <- 1/dat2001$sampsum25
dat2001$SS <- as.character(dat2001$SS)
dat2001$PCODE <- as.character(dat2001$PCODE)
write.csv(dat2001,paste(w,"QCdat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCTBL[,2:10],PKEY[,1:8],by=c("PKEY","SS")) #n=5808402
PC <- inner_join(PC, SSLCC[,c(2,5)], by=c("SS"))
QCPC <- PC[PC$JURS=="QC",] #n=465693
QCPC$SS <- as.character(QCPC$SS)
QCPC$PKEY <- as.character(QCPC$PKEY)
QCPC$PCODE <- as.character(QCPC$PCODE)
QCPC$SPECIES <- as.character(QCPC$SPECIES)
QCPC2001 <- QCPC[QCPC$YEAR < 2006,] #n=212901
QCPC2011 <- QCPC[QCPC$YEAR > 2005,] #n=252792
write.csv(QCPC2011,paste(w,"QCPC2011.csv",sep=""),row.names=FALSE)
write.csv(QCPC2001,paste(w,"QCPC2011.csv",sep=""),row.names=FALSE)
