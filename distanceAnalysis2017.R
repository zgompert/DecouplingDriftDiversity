library(unmarked)
distdata<-read.csv("2017_transect_data.csv",header=T)
distdata$trans_num<-as.character(distdata$trans_num)

################## SKI ################################3

## format data ##
ski<-data.frame(distdata[distdata[,1]=='SKI' & distdata[,2]=='1',c(5,3)])
colnames(ski)<-c("distance","transect")
ski$transect<-as.factor(ski$transect)
levels(ski$transect)<-as.character(c(6,2,7,"b5",9,"b3",8,3,5,10))

skidd<-formatDistData(ski,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,100,41,47,95,100,44,100,100,85)

skiumf<-unmarkedFrameDS(y=as.matrix(skidd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
ski_hn<-distsamp(~1~1,skiumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(ski_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

#Estimate  SE LinComb (Intercept)
#     2076 814    7.64           1

## for a detection function histogram
pdf("detection_function_figure_SKI.pdf",width=7,height=7)
hist(ski_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-111491
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
skidayps<-km2 * 2076
skidayps

## 231.4553 butterflies on day of visit


################## BTB ################################

btb<-data.frame(distdata[distdata[,1]=='BTB' & distdata[,2]=='1',c(5,3)])
colnames(btb)<-c("distance","transect")
btb$transect<-as.factor(btb$transect)
levels(btb$transect)<-as.character(c(10,5,8,2,7,6,1,3,9,4))

btbdd<-formatDistData(btb,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,100,100,85,50,100,100,100,85,100)

btbumf<-unmarkedFrameDS(y=as.matrix(btbdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
btb_hn<-distsamp(~1~1,btbumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(btb_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

 # Estimate  SE LinComb (Intercept)
 #    2686 944     7.9           1

## for a detection function histogram
pdf("detection_function_figure_BTB.pdf",width=7,height=7)
hist(btb_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-143372
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
btbdayps<-km2 * 2686
btbdayps

## 385.0972 butterflies on day of visit

############### GNP ########################

gnp<-data.frame(distdata[distdata[,1]=='GNP' & distdata[,2]=='1',c(5,3)])
colnames(gnp)<-c("distance","transect")
gnp$transect<-as.factor(gnp$transect)
levels(gnp$transect)<-as.character(c(9,2,7,4,5,1,3))

gnpdd<-formatDistData(gnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,60,90,100,100,100,90)

gnpumf<-unmarkedFrameDS(y=as.matrix(gnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
gnp_hn<-distsamp(~1~1,gnpumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(gnp_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate  SE LinComb (Intercept)
#     1611 773    7.38           1

## for a detection function histogram
pdf("detection_function_figure_GNP.pdf",width=7,height=7)
hist(gnp_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-70964
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
gnpdayps<-km2 * 1611
gnpdayps

## 114.323 butterflies on day of visit

############### PSP ########################

## format data ##
psp<-data.frame(distdata[distdata[,1]=='PSP' & distdata[,2]=='2',c(5,3)])
colnames(psp)<-c("distance","transect")
psp$transect<-as.factor(psp$transect)
levels(psp$transect)<-as.character(c(2,4,3,6,10))

pspdd<-formatDistData(psp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(55,76,100,94,40)

pspumf<-unmarkedFrameDS(y=as.matrix(pspdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
psp_hn<-distsamp(~1~1,pspumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(psp_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate   SE LinComb (Intercept)
#     3842 1979    8.25           1


## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-31601
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
pspdayps<-km2 * 3842
pspdayps

##121.411 bugs in the pop. on the day of visit
