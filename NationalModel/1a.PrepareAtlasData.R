library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)
library(data.table)

load("F:/BAM/BAMData/atlas_data_processed-20181018.RData")
w <-"G:/Boreal/NationalModelsV2/Quebec/"
SS <- na.omit(SS)
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))

# offl <- data.table(melt(OFF))
# names(offl) <- c("PKEY","SPECIES","logoffset")
# offl$SPECIES <- as.character(offl$SPECIES)
# offl$PKEY <- as.character(offl$PKEY)
# write.csv(offl,file=paste(w,"Atlasoffsets.csv",sep=""))
                       
eco <- raster("F:/GIS/ecoregions/CEC/quebececo1.tif")
nalc <- raster("F:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
quebec <- raster("F:/GIS/basemaps/quebec250m1.tif")
lf <- raster("D:/NorthAmerica/topo/lf_lcc1.tif")
lfq <- crop(lf,quebec)

#qbs2001 <- brick(paste(w,"QC2001rasters250",sep=""))
qbs2011 <- brick(paste(w,"QC2011rasters250",sep=""))
            
dat2011 <- cbind(SSLCC, extract(qbs2011,as.matrix(cbind(SSLCC$X,SSLCC$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(eco,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"
dat2011 <-cbind(dat2011,extract(lfq,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "landform"

samprast2011 <- rasterize(cbind(dat2011$X,dat2011$Y), quebec, field=1)
gf <- focalWeight(samprast2011, 100, "Gauss")
sampsum25 <- focal(samprast2011, w=gf, na.rm=TRUE)
dat2011 <- cbind(dat2011,extract(sampsum25,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "sampsum25"
dat2011$wt <- 1/dat2011$sampsum25
dat2011$SS <- as.character(dat2011$SS)
dat2011$PCODE <- as.character(dat2011$PCODE)
dat2011 <- na.omit(dat2011)
write.csv(dat2011,paste(w,"QCAtlasdat2011.csv",sep=""),row.names=FALSE)

# dat2001 <- cbind(SSLCC, extract(qbs2001,as.matrix(cbind(SSLCC$X,SSLCC$Y))))
# dat2001 <-cbind(dat2001,extract(nalc,as.matrix(cbind(dat2001$X,dat2001$Y)))) 
# names(dat2001)[ncol(dat2001)] <- "LCC"
# dat2001 <-cbind(dat2001,extract(eco,as.matrix(cbind(dat2001$X,dat2001$Y))))
# names(dat2001)[ncol(dat2001)] <- "eco"
# dat2001 <-cbind(dat2001,extract(lfq,as.matrix(cbind(dat2001$X,dat2001$Y))))
# names(dat2001)[ncol(dat2001)] <- "landform"
# 
# samprast2001 <- rasterize(cbind(dat2001$X,dat2001$Y), quebec, field=1)
# gf <- focalWeight(samprast2001, 100, "Gauss")
# sampsum25 <- focal(samprast2001, w=gf, na.rm=TRUE)
# dat2001 <- cbind(dat2001,extract(sampsum25,as.matrix(cbind(dat2001$X,dat2001$Y))))
# names(dat2001)[ncol(dat2001)] <- "sampsum25"
# dat2001$wt <- 1/dat2001$sampsum25
# dat2001$SS <- as.character(dat2001$SS)
# dat2001$PCODE <- as.character(dat2001$PCODE)
# dat2001 <- na.omit(dat2001)
# write.csv(dat2001,paste(w,"QCAtlasdat2001.csv",sep=""),row.names=FALSE)

PC <- inner_join(PCTBL[,2:8],PKEY[,1:8],by=c("PKEY","SS")) #n=628787
PC$SS <- as.character(PC$SS)
PC$PKEY <- as.character(PC$PKEY)
PC$PCODE <- as.character(PC$PCODE)
PC$SPECIES <- as.character(PC$SPECIES)
PC2001 <- PC[PC$YearCollected < 2006,] #n=0
#write.csv(PC2001,paste(w,"AtlasPC2001.csv",sep=""),row.names=FALSE)
PC2011 <- PC[PC$YearCollected > 2005,] #n=628787
write.csv(PC2011,paste(w,"AtlasPC2011.csv",sep=""),row.names=FALSE)


