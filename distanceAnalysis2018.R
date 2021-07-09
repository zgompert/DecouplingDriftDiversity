library(unmarked)
distdata<-read.csv("2018_transect_data.csv",header=T)
distdata$trans_num<-as.character(distdata$trans_num)

################## SKI ################################3

## format data ##
ski<-data.frame(distdata[distdata[,1]=='SKI' & distdata[,2]=='1',c(5,3)])
colnames(ski)<-c("distance","transect")
ski$transect<-as.factor(ski$transect)
levels(ski$transect)<-as.character(c(6,2,7,"b5",9,"b3",8,3,5))

skidd<-formatDistData(ski,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(23,100,35,43,80,100,50,100,100)

skiumf<-unmarkedFrameDS(y=as.matrix(skidd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
ski_hn<-distsamp(~1~1,skiumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(ski_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

#Estimate  SE LinComb (Intercept)
#     9780 2262    9.19           1

## for a detection function histogram
pdf("detection_function_figure_SKI.pdf",width=7,height=7)
hist(ski_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-111491
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
skidayps<-km2 * 9780
skidayps

## 1090.382 butterflies on day of visit


################## BTB ################################

btb<-data.frame(distdata[distdata[,1]=='BTB' & distdata[,2]=='1',c(5,3)])
colnames(btb)<-c("distance","transect")
btb$transect<-as.factor(btb$transect)
levels(btb$transect)<-as.character(c(6,1,3,9,4,10,5,8,2,7))

btbdd<-formatDistData(btb,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,100,100,100,100,100,60,100,85,45)

btbumf<-unmarkedFrameDS(y=as.matrix(btbdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
btb_hn<-distsamp(~1~1,btbumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(btb_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s)

 #Estimate   SE LinComb (Intercept)
     4289 1244    8.36           1

#Transformation: exp 

## for a detection function histogram
pdf("detection_function_figure_BTB.pdf",width=7,height=7)
hist(btb_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-143372
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
btbdayps<-km2 * 4289
btbdayps

## 614.9225 butterflies on day of visit

############### GNP ########################

gnp<-data.frame(distdata[distdata[,1]=='GNP' & distdata[,2]=='1',c(5,3)])
colnames(gnp)<-c("distance","transect")
gnp$transect<-as.factor(gnp$transect)
levels(gnp$transect)<-as.character(c(2,9,7,4,5,1,3))

gnpdd<-formatDistData(gnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(70,100,100,100,90,85,100)

gnpumf<-unmarkedFrameDS(y=as.matrix(gnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
gnp_hn<-distsamp(~1~1,gnpumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(gnp_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate  SE LinComb (Intercept)
#     2171 580    7.68           1

## for a detection function histogram
pdf("detection_function_figure_GNP.pdf",width=7,height=7)
hist(gnp_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-70964
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
gnpdayps<-km2 * 2171
gnpdayps

## 154.0628 butterflies on day of visit

################## USL ################################

usl<-data.frame(distdata[distdata[,1]=='USL' & distdata[,2]=='1',c(5,3)])
colnames(usl)<-c("distance","transect")
usl$transect<-as.factor(usl$transect)
levels(usl$transect)<-as.character(c(5,8,"b5",2,10,7,3,1,9))

usldd<-formatDistData(usl,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(60,100,100,100,60,100,75,60,90)

uslumf<-unmarkedFrameDS(y=as.matrix(usldd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
usl_hn<-distsamp(~1~1,uslumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(usl_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## Estimate   SE LinComb (Intercept)
##     2496 1066    7.82           1

## for a detection function histogram
pdf("detection_function_figure_USL.pdf",width=7,height=7)
hist(usl_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-132891
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
usldayps<-km2 * 2496
usldayps

## 331.6959 butterflies on day of visit

############### BNP ########################

## format data ##
bnp<-data.frame(distdata[distdata[,1]=='BNP' & distdata[,2]=='1',c(5,3)])
colnames(bnp)<-c("distance","transect")
bnp$transect<-as.factor(bnp$transect)
levels(bnp$transect)<-as.character(c(7,4,1,2,3,6,10))

bnpdd<-formatDistData(bnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(80,100,90,100,90,100,31)

bnpumf<-unmarkedFrameDS(y=as.matrix(bnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
bnp_hn<-distsamp(~1~1,bnpumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(bnp_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate   SE LinComb (Intercept)
#     5835 1782    8.67           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-68500
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
bnpdayps<-km2 * 5835
bnpdayps

## 399.6975 butterflies on day of visit

############### BLD ########################

## format data ##
bld<-data.frame(distdata[distdata[,1]=='BLD' & distdata[,2]=='1',c(5,3)])
colnames(bld)<-c("distance","transect")
bld$transect<-as.factor(bld$transect)
levels(bld$transect)<-as.character(c("b2",1,2,8))

blddd<-formatDistData(bld,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(78,100,50,70)

bldumf<-unmarkedFrameDS(y=as.matrix(blddd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
bld_hn<-distsamp(~1~1,bldumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(bld_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate   SE LinComb (Intercept)
#     5037 2580    8.52           1
     
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-99311
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
blddayps<-km2 * 5037
blddayps

## 500.2295 butterflies on day of visit
## 5 butterflies on day of visit