# Open libraries for packages used
library(raster)
library(rgdal)
library(dismo)
library(rpart)
library(sp)

#Load climate raster stack and data frame + bird data
load("K:/Boreal/DensityModels/density4.RData")

lc <- "K:/Boreal/maps_lc/"
setwd(lc)
curlc <- list.files(lc, pattern =".asc$")

lcstack <- stack(raster(curlc[1]), raster(curlc[2]))
i<-3
while (i <= length(curlc)) {
	lcstack <- addLayer(lcstack,raster(curlc[i]))
	i<-i+1
	}

#Create list of grids representing derived climate variables
cur <- "J:/CMIP3/20c3m/maps/"
setwd(cur)
clim <- list.files(cur, pattern =".asc$")

#Create climate raster stack with ID 
curclim<-stack(raster(clim[1]), raster(clim[2]))
i<-3
while (i <= length(clim)) {
	curclim <- addLayer(curclim,raster(clim[i]))
	i<-i+1
	}

#Create topoedaphic raster stack
topo <- "J:/CMIP3/20c3m/maps_topoedaphic/"
setwd(topo)
topoedaphic <- list.files(topo, pattern =".asc$")

topostack <- stack(raster(topoedaphic[1]), raster(topoedaphic[2]))
i<-3
while (i <= length(topoedaphic)) {
	topostack <- addLayer(topostack,raster(topoedaphic[i]))
	i<-i+1
	}	
	
climstack <- curclim
climstack <- addLayer(climstack, topostack,lcstack)	

datsamp5 <- read.csv("K:/Boreal/DensityModelsV5/brt/_datasample4.csv")
survey <- xx[1:2]
survey$ID <- row.names(survey)
surveydate <- aggregate(PC2$ABUND, by=list("PKEY"=PC2$PKEY,"YEAR"=PC2$YEAR,"SS"=PC2$SS,"PCODE"=PC2$PCODE,"SITE"=PC2$SITE), FUN=sum)
#source("I:/BAM/BAMData/calculate_offsets/BAMcorrections.R")

#Build models
w <- "K:/Boreal/DensityModelsV5/rt/"
setwd(w)
speclist <- read.csv("I:/BAM/BAMData/SpeciesClassesModv3.csv")
speclist <- as.factor(as.character(speclist[1:105,1]))
gc()
for (j in 1:length(speclist)) {
	specdat <- PC2[PC2$SPECIES == as.character(speclist[j]),]
	dat1 <- merge(datsamp5,specdat[,1:6],by=c("SS","PKEY","SITE","PCODE"),all.x=TRUE)
	dat1$SPECIES <- as.character(speclist[j])
	dat1$ABUND <- as.integer(ifelse(is.na(dat1$ABUND),0,dat1$ABUND))
	dat2 <- merge(dat1, surveydate[,1:5], by=c("SS","PKEY","SITE","PCODE"))
	off <- as.data.frame(cbind(xx[1],RES[spp==speclist[j]]))
	off$Species <- speclist[j]
	names(off) <- c("PKEY","A","p","q","SPECIES")
	off$offset <- off$A * off$p * off$q
	gc()
	dat3 <- merge(dat2,off[,c(1,5:6)])
	dat3$logoffset <- log(dat3$offset)
	datpres <- dat3[dat3$ABUND > 0,]
	gc()
	
	bird.rt1 <- rpart(ABUND ~ CMI + CMIJJA + dd01 + dd51 + EMT + msp + td + logoffset, data = dat3, method = "poisson", weights=dat3$wt)
	gc()
	save(bird.rt1, file=paste(w,speclist[j],"_rt_clim5.RData",sep=""))
	
	pdf(paste(w,speclist[j],"_rtplotclim5.pdf",sep=""))
	plot.rpart(bird.rt1)
	dev.off()
	
	rast <- predict(climstack, bird.rt1, type="response")
	writeRaster(rast, filename=paste(w,speclist[j],"_rtpredclim5.asc",sep=""), format="ascii",overwrite=TRUE)
	png(paste(w,speclist[j],"rtpredclim5.png",sep=""))
	plot(rast, zlim=c(0,1))
	points(datpres$X, datpres$Y, cex=0.05)
	dev.off()
	gc()
	
	bird.rt2 <- rpart(ABUND ~ CMI + CMIJJA + dd01 + dd51 + EMT + msp + td + na_cti + lc14 + lc15 + lc17 + lc18 + logoffset, data = dat3, method = "poisson", weights=dat3$wt)
	gc()
	save(bird.rt2, file=paste(w,speclist[j],"_rt_climtop5.RData",sep=""))
	
	pdf(paste(w,speclist[j],"_rtplotclimtop5.pdf",sep=""))
	plot.rpart(bird.rt2)
	dev.off()
	
	rast <- predict(climstack, bird.rt2, type="response")
	writeRaster(rast, filename=paste(w,speclist[j],"_rtpredclimtop5.asc",sep=""), format="ascii",overwrite=TRUE)
	png(paste(w,speclist[j],"rtpredclimtop5.png",sep=""))
	plot(rast, zlim=c(0,1))
	points(datpres$X, datpres$Y, cex=0.05)
	dev.off()
	gc()
	
	}
	
