library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)
library(data.table)

load("I:/BAM/BAMData/data_package_2016-04-18.Rdata")	
load("I:/BAM/BAMData/offsets-v3_2016-04-18.Rdata")
LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
coordinates(SS) <- c("X", "Y") 
proj4string(SS) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SSLCC <- as.data.frame(spTransform(SS, LCC))
QCSS <- SSLCC[SSLCC$JURS=="QC",]

#AtlasSS <- read.csv("I:/BAM/BAMData/QCAtlas/SS_QCAtlasv2.csv")
#AtlasPC <- read.csv("I:/BAM/BAMData/QCAtlas/PC_QCAtlasv2.csv")
#AtlasPKEY <- read.csv("I:/BAM/BAMData/QCAtlas/PKEY_QCAtlasv2.csv")

offl <- data.table(melt(OFF))
names(offl) <- c("PKEY","SPECIES","logoffset")
offl$SPECIES <- as.character(offl$SPECIES)
offl$PKEY <- as.character(offl$PKEY)
rm(OFF) #clear space

dat2011 <- read.csv("L:/Boreal/NationalModelsV2/QCdat2011.csv")
dat2001 <- read.csv("L:/Boreal/NationalModelsV2/QCdat2001.csv")
                       
qbs2011_1km <- raster("L:/Boreal/NationalModelsV2/QC2011rasters.grd")

PC <- inner_join(PCTBL,PKEY[,1:8],by=c("PKEY","SS","PCODE"))
PC <- inner_join(PC,SS@data[,c(2,5)],by="SS")
QCPC <- PC[PC$JURS=="QC",]
QCPC$SS <- as.character(QCPC$SS)
QCPC$PKEY <- as.character(QCPC$PKEY)
QCPC$PCODE <- as.character(QCPC$PCODE)
QCPC$SPECIES <- as.character(QCPC$SPECIES)
QCPC2001 <- QCPC[QCPC$YEAR < 2006,] #n=22262
QCPC2011 <- QCPC[QCPC$YEAR > 2005,] #n=93487
survey2001 <- aggregate(QCPC2001$ABUND, by=list("PKEY"=QCPC2001$PKEY,"SS"=QCPC2001$SS,"PCODE"=QCPC2001$PCODE), FUN=sum) #n=2458
survey2011 <- aggregate(QCPC2011$ABUND, by=list("PKEY"=QCPC2011$PKEY,"SS"=QCPC2011$SS,"PCODE"=QCPC2011$PCODE), FUN=sum) #n=11364

w <- "L:/Boreal/NationalModelsV2/"
setwd(w)
speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv5.csv")
speclist <- as.factor(as.character(speclist[1:105,1]))

for (j in 1:length(speclist)) {
  specoff <- offl[offl$SPECIES==as.character(speclist[j]),]
  
  specdat2001 <- QCPC2001[QCPC2001$SPECIES == as.character(speclist[j]),] #n=444
  dat1 <- right_join(specdat2001[,c(1:5)],survey2001[,1:3],by=c("SS","PCODE","PKEY")) #n=2610
  dat1$SPECIES <- as.character(speclist[j])
  dat1$ABUND <- as.integer(ifelse(is.na(dat1$ABUND),0,dat1$ABUND)) 
  s2001 <- left_join(dat1,specoff, by=c("SPECIES","PKEY"))
  d2001 <- left_join(s2001, dat2001, by=c("SS","PCODE")) 
  
  specdat2011 <- QCPC2011[QCPC2011$SPECIES == as.character(speclist[j]),] #n=444
  dat1 <- right_join(specdat2011[,c(1:5)],survey2011[,1:3],by=c("SS","PCODE","PKEY")) #n=2610
  dat1$SPECIES <- as.character(speclist[j])
  dat1$ABUND <- as.integer(ifelse(is.na(dat1$ABUND),0,dat1$ABUND)) 
  s2011 <- left_join(dat1,specoff, by=c("SPECIES","PKEY"))
  d2011 <- left_join(s2011, dat2011, by=c("SS","PCODE")) 

  datcombo <- rbind(d2001,d2011)
  datcombo$eco <- as.factor(datcombo$eco)

  x1 <- try(brt1 <- gbm.step(datcombo, gbm.y = 5, gbm.x = c(54,59,65,67,73,80,81,89,97,98,99,103,107,109,111,114,121,125,128,141,142), family = "poisson", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5, offset=datcombo$logoffset, site.weights=datcombo$wt))
  if (class(x1) != "try-error") {
    save(brt1,file=paste(w,speclist[j],"brtQC.R",sep=""))
    varimp <- as.data.frame(brt1$contributions)
    write.csv(varimp,file=paste(w,speclist[j],"varimp.csv",sep=""))
    cvstats <- t(as.data.frame(brt1$cv.statistics))
    write.csv(cvstats,file=paste(w,speclist[j],"cvstats.csv",sep=""))
    pdf(paste(w,speclist[j],"_plot.pdf",sep=""))
    gbm.plot(brt1,n.plots=9,smooth=TRUE)
    dev.off()
    rast <- predict(qbs2011_1km, brt1, type="response", n.trees=brt1$n.trees)
    writeRaster(rast, filename=paste(w,speclist[j],"_pred1km",sep=""), format="GTiff",overwrite=TRUE)
    png(paste(w,speclist[j],"_pred1km.png",sep=""))
    plot(rast, zlim=c(0,1))
    points(datcombo$X, datcombo$Y, cex=0.05)
    dev.off()
  }

}
