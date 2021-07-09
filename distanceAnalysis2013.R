library(unmarked)
distdata<-read.csv("2013_transect_data.csv",header=T)

############### BCR ########################
distdata$trans_num<-as.character(distdata$trans_num)
## format data,
bcr<-data.frame(distdata[distdata[,1]=='BCR',c(5,3)])
bcr[,1]<-bcr[,1] / 3.28084
colnames(bcr)<-c("distance","transect")
bcr$transect<-as.factor(bcr$transect)
levels(bcr$transect)<-as.character(c(5,10,7,6,'b5',2,3,8,1))
bcrdd<-formatDistData(bcr,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-300/3.28084
bcrumf<-unmarkedFrameDS(y=as.matrix(bcrdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=rep(ln,9),unitsIn="m")

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
    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-88581
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
bcrdayps<-km2 * 8963
## 2012 = 1336.776 bugs in the pop. on the day of visit
## 2013 = 793.9515 bugs in the pop. on the day of visit

############### GNP ########################

## format data,
gnp<-data.frame(distdata[distdata[,1]=='GNP',c(5,3)])
gnp[,1]<-gnp[,1] / 3.28084
colnames(gnp)<-c("distance","transect")
gnp$transect<-as.factor(gnp$transect)
levels(gnp$transect)<-as.character(c(1,3,4,5,7,9,2))
gnpdd<-formatDistData(gnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-300/3.28084
gnpumf<-unmarkedFrameDS(y=as.matrix(gnpdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=rep(ln,7),unitsIn="m")


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

    
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-70694
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
gnpdayps<-km2 * 5281
## 2012 = 206.2144 bugs in the pop. on the day of visite
## 2013 = 373.335 bugs in the pop. on the day of visite


############### BTB ########################

## format data,
visit<-3
btb<-data.frame(distdata[distdata[,1]=='BTB' & distdata[,2]==visit,c(5,3)])
btb[,1]<-btb[,1] / 3.28084
colnames(btb)<-c("distance","transect")
btb$transect<-as.character(btb$transect)
btb$transect<-as.factor(btb$transect)
levels(btb$transect)<-as.character(c(10,5,2,7,8,6,3,1,9,4))
btbdd<-formatDistData(btb,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
ln<-300/3.28084
btbumf<-unmarkedFrameDS(y=as.matrix(btbdd),survey="line",dist.breaks=c(0,1,2,3,4,5),tlength=rep(ln,10),unitsIn="m")


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


## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-143372
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
btbdayps<-km2 * 2405
## 2375.101
## 2012 = 2375.101 bugs in the pop. on the day of visite
## 2013 v1 =  164.7425 bugs in the pop. on the day of visite
## 2013 v2 =  284.0695 bbugs in the pop. on the day of visite
## 2013 v3 =  292.8375 bugs in the pop. on the day of visite

############### BNP ########################

## format data,
bnp<-data.frame(distdata[distdata[,1]=='BNP',c(5,3)])
bnp[,1]<-bnp[,1] / 3.28084
colnames(bnp)<-c("distance","transect")
bnp$transect<-as.character(bnp$transect)
bnp$transect<-as.factor(bnp$transect)
levels(bnp$transect)<-as.character(c(10,6,3,2,9,1,5,4,7))
ln<-c(300,300,232,300,300,250,300,300,150)/3.28084
bnpdd<-formatDistData(bnp,"distance","transect",dist.breaks=c(0,1,2,3,4,5))
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
   
## for population size, convert area from m^2 to m, then m to km, then km to km^2
m2<-68500
m<-sqrt(m2)
km2<-(m/1000)^2

## multiply by density for daily abundance
bnpdayps<-km2 * 3084
## 2012 = 721.716 bugs in the pop. on the day of visite
## 2013 = 211.254 bugs in the pop. on the day of visite


