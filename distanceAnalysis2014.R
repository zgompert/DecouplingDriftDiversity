library(unmarked)
distdata<-read.csv("2014_transect_data.csv",header=T)
distdata$trans_num<-as.character(distdata$trans_num)

############### GNP ########################

## format data ##
gnp<-data.frame(distdata[distdata[,1]=='GNP' & distdata[,2]=='2',c(5,3)])
gnp[,1]<-gnp[,1] / 3.28084
colnames(gnp)<-c("distance","transect")
gnp$transect<-as.factor(gnp$transect)
levels(gnp$transect)<-as.character(c(2,3,4,7,9,5,1))

gnpdd<-formatDistData(gnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(240,rep(300,6))/3.28084

gnpumf<-unmarkedFrameDS(y=as.matrix(gnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
gnp_hn<-distsamp(~1~1,gnpumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(gnp_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2012
## Estimate  SE LinComb (Intercept)
##    2917 843    7.98           1

## 2013 
## Estimate   SE LinComb (Intercept)
##     5281 1527    8.57           1

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     4813 1511    8.48 

## for a detection function histogram
pdf("detection_function_figure_GNP.pdf",width=7,height=7)
hist(gnp_hn,xlab="Distance (meters)",cex.lab=1.5,main="Detection function",cex.main=1.5)
dev.off()
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-70964
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
gnpdayps<-km2 * 4813
## 2012 = 206.2144 bugs in the pop. on the day of visit
## 2013 = 373.335 bugs in the pop. on the day of visit
## 2014 = 341.5497 bugs in the pop on the day of visit

############### BTB ########################

## format data ##
btb<-data.frame(distdata[distdata[,1]=='BTB' & distdata[,2]=='2',c(5,3)])
btb[,1]<-btb[,1] / 3.28084
colnames(btb)<-c("distance","transect")
btb$transect<-as.factor(btb$transect)
levels(btb$transect)<-as.character(c(10,5,8,2,7,6,3,1,9,4))

btbdd<-formatDistData(btb,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,300,300,275,210,rep(300,5))/3.28084

btbumf<-unmarkedFrameDS(y=as.matrix(btbdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
btb_hn<-distsamp(~1~1,btbumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(btb_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2012
## Estimate   SE LinComb (Intercept)
##    16566 3149    9.72           1
  
## 2013 visit 1
## Estimate   SE LinComb (Intercept)
##     2405 1008    7.79           1
 
## 2013 visit 2
## Estimate   SE LinComb (Intercept)
##     4147 1248    8.33           1

## 2013 visit 3
## Estimate   SE LinComb (Intercept)
#     4275 1542    8.36           1

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
#     4600 1483    8.43           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-143372
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
btbdayps<-km2 * 4600
## 2012 = 2375.101 bugs in the pop. on the day of visit
## 2013 v1 =  344.8097 bugs in the pop. on the day of visit
## 2013 v2 =  594.5637 bbugs in the pop. on the day of visit
## 2013 v3 =  612.9153 bugs in the pop. on the day of visit
## 2014 v4 = 659.5112

############### BNP ########################

## format data ##
bnp<-data.frame(distdata[distdata[,1]=='BNP' & distdata[,2]=='2',c(5,3)])
bnp[,1]<-bnp[,1] / 3.28084
colnames(bnp)<-c("distance","transect")
bnp$transect<-as.factor(bnp$transect)
levels(bnp$transect)<-as.character(c(7,4,5,3,10,6,1,9))

bnpdd<-formatDistData(bnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(160,300,300,280,150,200,300,300)/3.28084

bnpumf<-unmarkedFrameDS(y=as.matrix(bnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
bnp_hn<-distsamp(~1~1,bnpumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(bnp_hn, type="state")
## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2012
## Estimate   SE LinComb (Intercept)
##    10536 2557    9.26           1
 
## 2013
## Estimate   SE LinComb (Intercept)
##    3084 1034    8.03           1

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     6196 1727    8.73           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-68500
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
bnpdayps<-km2 * 6196
## 2012 = 721.716 bugs in the pop. on the day of visit
## 2013 = 211.254 bugs in the pop. on the day of visit
## 2014 visit 2 = 424.426 in the pop. on the day of visit

############### BCR ########################

## format data ##
bcr<-data.frame(distdata[distdata[,1]=='BCR' & distdata[,2]=='2',c(5,3)])
bcr[,1]<-bcr[,1] / 3.28084
colnames(bcr)<-c("distance","transect")
bcr$transect<-as.factor(bcr$transect)
levels(bcr$transect)<-as.character(c(8,3,2,'b5',6,1,7,10,5))

bcrdd<-formatDistData(bcr,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,300,300,300,300,190,300,300,300)/3.28084

bcrumf<-unmarkedFrameDS(y=as.matrix(bcrdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
bcr_hn<-distsamp(~1~1,bcrumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(bcr_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2012
## Estimate   SE LinComb (Intercept)
##    15091 3331    9.62   

## 2013
## Estimate   SE LinComb (Intercept)
##    8963 1956     9.1           1

## 2014
## Estimate   SE LinComb (Intercept)
##     4672 1442    8.45           1
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-88581
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
bcrdayps<-km2 * 4672
## 2012 = 1336.776 bugs in the pop. on the day of visit
## 2013 = 793.9515 bugs in the pop. on the day of visit
## 2014 = 413.8504 bugs in the pop. on the day of visit

############### PSP ########################

## format data ##
psp<-data.frame(distdata[distdata[,1]=='PSP' & distdata[,2]=='2',c(5,3)])
psp[,1]<-psp[,1] / 3.28084
colnames(psp)<-c("distance","transect")
psp$transect<-as.factor(psp$transect)
levels(psp$transect)<-as.character(c(2,4,3,6,10))

pspdd<-formatDistData(psp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(225,300,300,300,300)/3.28084

pspumf<-unmarkedFrameDS(y=as.matrix(pspdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
psp_hn<-distsamp(~1~1,pspumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(psp_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     3868 1626    8.26           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-31601
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
pspdayps<-km2 * 3868
## 2014 = 122.2327 bugs in the pop. on the day of visit

############### USL ########################

## format data ##
usl<-data.frame(distdata[distdata[,1]=='USL' & distdata[,2]=='2',c(5,3)])
usl[,1]<-usl[,1] / 3.28084
colnames(usl)<-c("distance","transect")
usl$transect<-as.factor(usl$transect)
levels(usl$transect)<-as.character(c(4,5,8,'b5',2,10,7,3,1,9))

usldd<-formatDistData(usl,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,229.3,300,300,300,300,300,167,300,300)/3.28084

uslumf<-unmarkedFrameDS(y=as.matrix(usldd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
usl_hn<-distsamp(~1~1,uslumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(usl_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     4285 1234    8.36           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-132891
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
usldayps<-km2 * 4285
## 2014 = 569.4379 bugs in the pop. on the day of visit

############### MRF ########################

## format data ##
mrf<-data.frame(distdata[distdata[,1]=='MRF' & distdata[,2]=='2',c(5,3)])
mrf[,1]<-mrf[,1] / 3.28084
colnames(mrf)<-c("distance","transect")
mrf$transect<-as.factor(mrf$transect)
levels(mrf$transect)<-as.character(c('b4','b3',2,8,'b5'))

mrfdd<-formatDistData(mrf,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,185,300,300,300)/3.28084

mrfumf<-unmarkedFrameDS(y=as.matrix(mrfdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
mrf_hn<-distsamp(~1~1,mrfumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(mrf_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##    5010 2023    8.52           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-65045.43
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
mrfdayps<-km2 * 5010
## 2014 = 325.8776 bugs in the pop. on the day of visit

############### HNV ########################

## format data ##
hnv<-data.frame(distdata[distdata[,1]=='HNV' & distdata[,2]=='1',c(5,3)])
hnv[,1]<-hnv[,1] / 3.28084
colnames(hnv)<-c("distance","transect")
hnv$transect<-as.factor(hnv$transect)
levels(hnv$transect)<-as.character(c('b1','b5',8,9,'b4','b2',5,10,2,6))

hnvdd<-formatDistData(hnv,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,300,300,200,185,300,200,200,150,225)/3.28084

hnvumf<-unmarkedFrameDS(y=as.matrix(hnvdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
hnv_hn<-distsamp(~1~1,hnvumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(hnv_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     5144 1668    8.55           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-342890
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
hnvdayps<-km2 * 5144
## 2014 = 1763.826 bugs in the pop. on the day of visit

############### SKI ########################

## format data ##
ski<-data.frame(distdata[distdata[,1]=='SKI' & distdata[,2]=='2',c(5,3)])
ski[,1]<-ski[,1] / 3.28084
colnames(ski)<-c("distance","transect")
ski$transect<-as.factor(ski$transect)
levels(ski$transect)<-as.character(c(6,2,7,'b5',9,8,'b3',3,5,10))

skidd<-formatDistData(ski,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-c(300,300,175,300,300,175,300,300,300,200)/3.28084

skiumf<-unmarkedFrameDS(y=as.matrix(skidd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=ln,unitsIn="m")

## model fitting, need to transform off of log-scale
ski_hn<-distsamp(~1~1,skiumf,keyfun="halfnorm",output="density",unitsOut="kmsq")
backTransform(ski_hn, type="state")

## Backtransformed linear combination(s) of Density estimate(s), this is per sq km

## 2014 visit 2
## Estimate   SE LinComb (Intercept)
##     4033 1207     8.3           1

## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-111491
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
skidayps<-km2 * 4033
## 2014 = 449.6432 bugs in the pop. on the day of visit