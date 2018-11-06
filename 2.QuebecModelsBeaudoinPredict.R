library(raster)
library(dismo)
library(rpart)
library(maptools)
library(dplyr)

qbs2011_1km <- brick("L:/Boreal/NationalModelsV2/QC2011rasters.grd")
r2 <- qbs2011_1km[[1]]

LCC <- CRS("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
offl <- read.csv("I:/BAM/BAMData/BAMoffsets.csv")
offla <- read.csv("I:/BAM/BAMData/Atlasoffsets.csv")
offlc <- rbind(offl[2:4],offla[2:4])
offlc$PKEY <- as.character(offlc$PKEY)
offlc$SPECIES <- as.character(offlc$SPECIES)
rm(offla,offl)

dat2001 <- read.csv("L:/Boreal/NationalModelsV2/QCdat2001.csv")
dat2001 <- dat2001[,c(1:2,48:142)]
dat2001$PCODE <- as.character(dat2001$PCODE)
dat2001$SS <- as.character(dat2001$SS)
dat_2001 <- dat2001

dat2011 <- read.csv("L:/Boreal/NationalModelsV2/QCdat2011.csv")
dat2011 <- dat2011[,c(1:2,48:142)]
dat2011$PCODE <- as.character(dat2011$PCODE)
dat2011$SS <- as.character(dat2011$SS)
adat2011 <- read.csv("I:/BAM/BAMData/QCAtlasdat2011.csv")
adat2011 <- adat2011[,c(1:2,6:100)]
adat2011$PCODE <- as.character(adat2011$PCODE)
adat2011$SS <- as.character(adat2011$SS)

d2011 <- rbind(dat2011,adat2011) #n=52560
dat_2011 <- d2011[!duplicated(d2011[, 1:2]), ] #n=42210

#calculating sample weights as inverse of number of survey points within 5x5 pixel radius
samprast2011 <- rasterize(cbind(dat_2011$X,dat_2011$Y), r2, field=1)
sampsum25 <- focal(samprast2011, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
dat_2011 <- cbind(dat_2011,extract(sampsum25,as.matrix(cbind(dat_2011$X,dat_2011$Y))))
names(dat_2011)[ncol(dat_2011)] <- "sampsum25"
dat_2011$wt <- 1/dat_2011$sampsum25
dat_2011$SS <- as.character(dat_2011$SS) #n=42210
rm(samprast2011)

samprast2001 <- rasterize(cbind(dat_2001$X,dat_2001$Y), r2, field=1)
sampsum25 <- focal(samprast2001, w=matrix(1/25, nc=5, nr=5), na.rm=TRUE)
dat_2001 <- cbind(dat_2001,extract(sampsum25,as.matrix(cbind(dat_2001$X,dat_2001$Y))))
names(dat_2001)[ncol(dat_2001)] <- "sampsum25"
dat_2001$wt <- 1/dat_2001$sampsum25
dat_2001$SS <- as.character(dat_2001$SS) 
rm(samprast2001)
rm(r2)

APC2011 <- read.csv("I:/BAM/BAMData/AtlasPC2011.csv")
QCPC2011 <- read.csv("I:/BAM/BAMData/QCPC2011.csv")
QCPC2001 <- read.csv("I:/BAM/BAMData/QCPC2001.csv") #n=212901
QCPC2001$PKEY <- as.character(QCPC2001$PKEY)
QCPC2001$SS <- as.character(QCPC2001$SS)
PC2011 <- rbind(APC2011[,1:4],QCPC2011[,1:4]) #n=881579
PC2011 <- distinct(PC2011) #n=828646
PC2011$PKEY <- as.character(PC2011$PKEY)
PC2011$SS <- as.character(PC2011$SS)

survey2001 <- aggregate(QCPC2001$ABUND, by=list("PKEY"=QCPC2001$PKEY,"SS"=QCPC2001$SS), FUN=sum) #n=26161
survey2011 <- aggregate(PC2011$ABUND, by=list("PKEY"=PC2011$PKEY,"SS"=PC2011$SS), FUN=sum) #n=89289

w <- "L:/Boreal/NationalModelsV2/"
setwd(w)
speclist <- levels(as.factor(offlc$SPECIES))

for (j in 1:length(speclist)) {
  specoff <- filter(offlc, SPECIES==as.character(speclist[j]))
  specoff <- distinct(specoff) 
  
  specdat2001 <- filter(QCPC2001, SPECIES == as.character(speclist[j]))
  dat1 <- right_join(specdat2001[,c(1:4)],survey2001[,1:3],by=c("SS","PKEY")) 
  dat1$SPECIES <- as.character(speclist[j])
  dat1$ABUND <- as.integer(ifelse(is.na(dat1$ABUND),0,dat1$ABUND)) 
  s2001 <- left_join(dat1,specoff, by=c("SPECIES","PKEY"))
  d2001 <- left_join(s2001, dat_2001, by=c("SS")) 
  
  specdat2011 <- filter(PC2011, SPECIES == as.character(speclist[j])) 
  dat2 <- right_join(specdat2011[,c(1:4)],survey2011[,1:3],by=c("SS","PKEY"))
  dat2$SPECIES <- as.character(speclist[j])
  dat2$ABUND <- as.integer(ifelse(is.na(dat2$ABUND),0,dat2$ABUND)) 
  s2011 <- left_join(dat1,specoff, by=c("SPECIES","PKEY"))
  d2011 <- left_join(s2011, dat_2011, by=c("SS")) 
  d2011 <- na.omit(d2011) #eliminate non-Quebec data 

  datcombo <- rbind(d2001,d2011)
  datcombo <- na.omit(datcombo)

  # Beaudoin covariates for model 
  # LandCover_NonVeg_v1
  # LandCover_VegNonTreed_v1
  # Species_Abie_Bal_v1                                                                                    
  # Species_Acer_Rub_v1                      
  # Species_Acer_Sah_v1                      
  # Species_Betu_All_v1                  
  # Species_Betu_Pap_v1                      
  # Species_Fagu_Gra_v1                      
  # Species_Frax_Ame_v1                      
  # Species_Lari_Lar_v1                      
  # Species_Pice_Gla_v1                      
  # Species_Pice_Mar_v1                      
  # Species_Pice_Rub_v1                                            
  # Species_Pinu_Ban_v1    
  # Species_Pinu_Res_v1                                           
  # Species_Pinu_Str_v1                      
  # Species_Popu_Bal_v1                      
  # Species_Popu_Tre_v1                                           
  # Species_Quer_Rub_v1                                           
  # Species_Thuj_Occ_v1                                         
  # Species_Tsug_Can_v1                                                                                 
  # Structure_Biomass_TotalLiveAboveGround_v1
  # Structure_Stand_Age_v1  

  x1 <- try(brt1 <- gbm.step(datcombo, gbm.y = 4, gbm.x = c(10,12,15,21,23,29,30,36,37,45,53,54,55,59,63,65,67,70,77,81,84,97,98), family = "poisson", tree.complexity = 3, learning.rate = 0.001, bag.fraction = 0.5, offset=datcombo$logoffset, site.weights=datcombo$wt))
  if (class(x1) != "try-error") {
    save(brt1,file=paste(w,speclist[j],"brtQC2.R",sep=""))
    varimp <- as.data.frame(brt1$contributions)
    write.csv(varimp,file=paste(w,speclist[j],"varimp2.csv",sep=""))
    cvstats <- t(as.data.frame(brt1$cv.statistics))
    write.csv(cvstats,file=paste(w,speclist[j],"cvstats2.csv",sep=""))
    pdf(paste(w,speclist[j],"_plot2.pdf",sep=""))
    gbm.plot(brt1,n.plots=9,smooth=TRUE)
    dev.off()
    rast <- predict(qbs2011_1km, brt1, type="response", n.trees=brt1$n.trees)
    writeRaster(rast, filename=paste(w,speclist[j],"_pred1km2",sep=""), format="GTiff",overwrite=TRUE)
  }
  gc()

}
