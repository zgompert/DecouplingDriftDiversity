library(unmarked)
distdata<-read.csv("2016_transect_data.csv",header=T)
distdata$trans_num<-as.character(distdata$trans_num)

################## SKI ################################3

## format data ##
ski<-data.frame(distdata[distdata[,1]=='SKI' & distdata[,2]=='1',c(5,3)])
colnames(ski)<-c("distance","transect")
ski$transect<-as.factor(gnp$transect)
levels(ski$transect)<-as.character(c(2,7,"b5",9,3,5,6,"b3",8))

skidd<-formatDistData(ski,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,40,30,80,100,100,100,100,44)

skiumf<-unmarkedFrameDS(y=as.matrix(skidd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
ski_hn<-distsamp(~1~1,skiumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(ski_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

# Estimate   SE LinComb (Intercept)
#     3714 1471    8.22           1

## for a detection function histogram
pdf("detection_function_figure_SKI.pdf",width=7,height=7)
hist(ski_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-111491
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
skidayps<-km2 * 3714

## 414.07 butterflies on day of visit


################## BTB ################################

btb<-data.frame(distdata[distdata[,1]=='BTB' & distdata[,2]=='1',c(5,3)])
colnames(btb)<-c("distance","transect")
btb$transect<-as.factor(btb$transect)
levels(btb$transect)<-as.character(c(10,5,8,6,1,3,9,4))

btbdd<-formatDistData(btb,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,100,100,100,100,100,100,100)

btbumf<-unmarkedFrameDS(y=as.matrix(btbdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
btb_hn<-distsamp(~1~1,btbumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(btb_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## Estimate   SE LinComb (Intercept)
##     6425 2006    8.77           1

## for a detection function histogram
pdf("detection_function_figure_BTB.pdf",width=7,height=7)
hist(btb_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-143372
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
btbdayps<-km2 * 6425
btbdayps

## 921.1651 butterflies on day of visit

################## USL ################################

usl<-data.frame(distdata[distdata[,1]=='USL' & distdata[,2]=='1',c(5,3)])
colnames(usl)<-c("distance","transect")
usl$transect<-as.factor(usl$transect)
levels(usl$transect)<-as.character(c(9,5,8,3,1))

usldd<-formatDistData(usl,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(100,60,100,48,100)

uslumf<-unmarkedFrameDS(y=as.matrix(usldd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
usl_hn<-distsamp(~1~1,uslumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(usl_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## Estimate   SE LinComb (Intercept)
##     7342 2894     8.9           1

## for a detection function histogram
pdf("detection_function_figure_USL.pdf",width=7,height=7)
hist(usl_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-132891
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
usldayps<-km2 * 7342
usldayps

## 975.6857 butterflies on day of visit