library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)
library(data.table)

load("I:/BAM/BAMData/atlas_data_processed-20181018.RData")
w <- "I:/BAM/BAMData/"
SS <- na.omit(SS)
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))

offl <- data.table(melt(OFF))
names(offl) <- c("PKEY","SPECIES","logoffset")
offl$SPECIES <- as.character(offl$SPECIES)
offl$PKEY <- as.character(offl$PKEY)
write.csv(offl,file=paste(w,"Atlasoffsets.csv",sep=""))
                       
eco <- raster("I:/GIS/ecoregions/CEC/quebececo1.tif")
nalc <- raster("I:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
quebec <- raster("I:/GIS/basemaps/quebec250m1.tif")
            
b2011 <- list.files("I:/GIS/landcover/Beaudoin/2011/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2011/")
bs2011 <- stack(raster(b2011[1]))
for (i in 2:length(b2011)) {bs2011 <- addLayer(bs2011, raster(b2011[i]))}
names(bs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(bs2011))
qbs2011 <- crop(bs2011,quebec)
writeRaster(qbs2011,file=paste(w,"QC2011rasters_250",sep=""))

b2001 <- list.files("I:/GIS/landcover/Beaudoin/2001/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) { bs2001 <- addLayer(bs2001, raster(b2001[i]))}
names(bs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(bs2001))
qbs2001 <- crop(bs2001,quebec)
writeRaster(qbs2001,file=paste(w,"QC2001rasters_250",sep=""))

dat2011 <- cbind(SSLCC, extract(qbs2011,as.matrix(cbind(SSLCC$X,SSLCC$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(eco,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"

samprast2011 <- rasterize(cbind(dat2011$X,dat2011$Y), quebec, field=1)
sampsum25 <- focal(samprast2011, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
dat2011 <- cbind(dat2011,extract(sampsum25,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "sampsum25"
dat2011$wt <- 1/dat2011$sampsum25
dat2011$SS <- as.character(dat2011$SS)
dat2011$PCODE <- as.character(dat2011$PCODE)
dat2011 <- na.omit(dat2011)
write.csv(dat2011,paste(w,"QCAtlasdat2011.csv",sep=""),row.names=FALSE)

dat2001 <- cbind(SSLCC, extract(qbs2001,as.matrix(cbind(SSLCC$X,SSLCC$Y))))
dat2001 <-cbind(dat2001,extract(nalc,as.matrix(cbind(dat2001$X,dat2001$Y)))) 
names(dat2001)[ncol(dat2001)] <- "LCC"
dat2001 <-cbind(dat2001,extract(eco,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "eco"

samprast2001 <- rasterize(cbind(dat2001$X,dat2001$Y), quebec, field=1)
sampsum25 <- focal(samprast2001, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
dat2001 <- cbind(dat2001,extract(sampsum25,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "sampsum25"
dat2001$wt <- 1/dat2001$sampsum25
dat2001$SS <- as.character(dat2001$SS)
dat2001$PCODE <- as.character(dat2001$PCODE)
dat2001 <- na.omit(dat2001)
write.csv(dat2001,paste(w,"QCAtlasdat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCTBL[,2:8],PKEY[,1:8],by=c("PKEY","SS")) #n=628787
PC$SS <- as.character(PC$SS)
PC$PKEY <- as.character(PC$PKEY)
PC$PCODE <- as.character(PC$PCODE)
PC$SPECIES <- as.character(PC$SPECIES)
PC2001 <- PC[PC$YearCollected < 2006,] #n=0
#write.csv(PC2001,paste(w,"AtlasPC2001.csv",sep=""),row.names=FALSE)
PC2011 <- PC[PC$YearCollected > 2005,] #n=628787
write.csv(PC2011,paste(w,"AtlasPC2011.csv",sep=""),row.names=FALSE)


