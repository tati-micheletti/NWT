library(raster)
library(dismo)
library(rpart)
library(maptools)
library(data.table)

load("I:/BAM/BAMData/data_package_2016-04-18.Rdata")	
load("I:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))
QCSS <- SSLCC[SSLCC$JURS=="QC",]

AtlasSS <- read.csv("I:/BAM/BAMData/QCAtlas/SS_QCAtlasv2.csv")
AtlasPC <- read.csv("I:/BAM/BAMData/QCAtlas/PC_QCAtlasv2.csv")
AtlasPKEY <- read.csv("I:/BAM/BAMData/QCAtlas/PKEY_QCAtlasv2.csv")

offl <- data.table(melt(OFF))
names(offl) <- c("PKEY","SPECIES","logoffset")
offl$SPECIES <- as.character(offl$SPECIES)
                       
eco <- shapefile("I:/GIS/ecoregions/CEC/NA_CEC_Eco_Level3_LCC.shp")
nalc <- raster("I:/GIS/landcover/NALC/LandCover_IMG/NA_LandCover_2005/data/NA_LandCover_2005/NA_LandCover_2005_LCC.img")
quebec <- shapefile("I:/GIS/basemaps/QuebecLCC.shp")

#plotlc <- "L:/Boreal/maps_lc/"
#setwd(lc)
#curlc <- list.files(lc, pattern =".asc$")
#lcstack <- stack(raster(curlc[1]))
#for (i in 2:length(curlc)) {lcstack <- addLayer(lcstack, raster(curlc[i]))}                 

b2011 <- list.files("I:/GIS/landcover/Beaudoin/2011/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2011/")
bs2011 <- stack(raster(b2011[1]))
for (i in 2:length(b2011)) { bs2011 <- addLayer(bs2011, raster(b2011[i]))}

b2001 <- list.files("I:/GIS/landcover/Beaudoin/2001/",pattern="tif$")
setwd("I:/GIS/landcover/Beaudoin/2001/")
bs2001 <- stack(raster(b2001[1]))
for (i in 2:length(b2001)) { bs2001 <- addLayer(bs2001, raster(b2001[i]))}

qbs2011 <- crop(bs2011,quebec)
qbs2011 <- mask(qbs2011,quebec)
qbs2001 <- crop(bs2001,quebec)
qbs2001 <- mask(qbs2001,quebec)
names(qbs2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(qbs2001))
names(qbs2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(qbs2011))
qbs2011_1km <- aggregate(qbs2011, fact=4, fun=mean)

ecor <- rasterize(eco,bs2011[[1]])
ecor1km <- resample(eco4, qbs2011_1km)
qbs2011_1km <- addLayer(qbs2011_1km, ecor1km)

dat2011 <- cbind(QCSS, extract(qbs2011,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2011 <-cbind(dat2011,extract(nalc,as.matrix(cbind(dat2011$X,dat2011$Y)))) 
names(dat2011)[ncol(dat2011)] <- "LCC"
dat2011 <-cbind(dat2011,extract(ecor,as.matrix(cbind(dat2011$X,dat2011$Y))))
names(dat2011)[ncol(dat2011)] <- "eco"

dat2001 <- cbind(QCSS, extract(qbs2001,as.matrix(cbind(QCSS$X,QCSS$Y))))
dat2001 <-cbind(dat2001,extract(nalc,as.matrix(cbind(dat2001$X,dat2001$Y)))) 
names(dat2001)[ncol(dat2001)] <- "LCC"
dat2001 <-cbind(dat2001,extract(ecor,as.matrix(cbind(dat2001$X,dat2001$Y))))
names(dat2001)[ncol(dat2001)] <- "eco"

PC <- merge(PCTBL,PKEY[,1:8],by=c("PKEY","SS","PCODE"))
PC <- merge(PC,SS[,c(2,5)],by="SS")
QCPC <- PC[PC$JURS=="QC",]
surveydate <- aggregate(QCPC$ABUND, by=list("PKEY"=QCPC$PKEY,"SS"=QCPC$SS,"PCODE"=QCPC$PCODE), FUN=sum)

w <- "L:/Boreal/NationalModelsV2/"
setwd(w)
speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
speclist <- as.factor(as.character(speclist[1:105,1]))

for (j in 1:length(speclist)) {
  specdat <- QCPC[QCPC$SPECIES == as.character(speclist[j]),] #n=2918
  specdat <- merge(specdat,offl, by=c("SPECIES","PKEY"))
  s2001 <- specdat[specdat$YEAR < 2006,] #n=444
  dat1 <- merge(s2001[,1:5],surveydate[,1:3],by=c("SS","PCODE","PKEY"),all.y=TRUE)
  dat1$SPECIES <- as.character(speclist[j])
  dat1$ABUND <- as.integer(ifelse(is.na(dat1$ABUND),0,dat1$ABUND)) #n=13974
  d2001 <- merge(dat1, dat2001, by=c("SS","PCODE"),all.x=TRUE) 
  s2011 <- specdat[specdat$YEAR > 2005,] 
  dat2 <- merge(s2011[,1:5],surveydate[,1:3],by=c("SS","PCODE","PKEY"),all.y=TRUE) #n=13872
  dat2$SPECIES <- as.character(speclist[j])
  dat2$ABUND <- as.integer(ifelse(is.na(dat2$ABUND),0,dat2$ABUND)) 
  d2011 <- merge(dat2, dat2011, by=c("SS","PCODE"),all.x=TRUE) #n=13872  
  #names(d2001) <- gsub("NFI_MODIS250m_2001_kNN_","",names(d2001))
  #names(d2011) <- gsub("NFI_MODIS250m_2011_kNN_","",names(d2011))
  datcombo <- rbind(d2001,d2011)

  x1 <- try(brt1 <- gbm.step(datcombo, gbm.y = 5, gbm.x = c(58,64,66,72,79,80,88,96,97,98,102,106,108,110,113,120,124,127,140,141), family = "poisson", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5))
  if (class(x1) != "try-error") {
    save(brt1,file=paste(w,speclist[j],"brtQC.R",sep=""))
    varimp <- as.data.frame(brt1$contributions)
    write.csv(varimp,file=paste(w,speclist[j],"varimp.csv",sep=""))
    cvstats <- t(as.data.frame(brt1$cv.statistics))
    write.csv(cvstats,file=paste(w,speclist[j],"cvstats.csv",sep=""))
    pdf(paste(w,speclist[j],"_plot.pdf",sep=""))
    gbm.plot(brt1,n.plots=9,smooth=TRUE)
    dev.off()
    rast <- predict(qbs2011, brt1, type="response", n.trees=brt1$n.trees)
    writeRaster(rast, filename=paste(w,speclist[j],"_pred",sep=""), format="geoTiff",overwrite=TRUE)
    png(paste(w,speclist[j],"_pred.png",sep=""))
    plot(rast, zlim=c(0,1))
    points(datcombo$X, datcombo$Y, cex=0.05)
    dev.off()
  }

}
