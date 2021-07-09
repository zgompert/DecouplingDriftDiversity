## summarize results from IM analysis

imf<-list.files(pattern="im3.txt")

N<-length(imf)
dat<-vector("list",N)
llsd<-rep(NA,N)
for(i in 1:N){
	dat[[i]]<-read.table(imf[i])
	llsd[i]<-sd(dat[[i]][,1])
}

maxL<-matrix(NA,nrow=N,ncol=dim(dat[[1]])[2])
for(i in 1:N){
	maxL[i,]<-as.numeric(dat[[i]][16,])
}

## from Ryan Gutenkunst
#just to be complete, the parameter dadi estimates is M12 = 2*Na*m12. Here m12 is the proportion of chromosomes in each generation that are new migrants from population 2 to population 1. So if population 1 has current size nu1*Na individuals, then the number of new migrant individuals is m12 * nu1*Na = nu1*M12/2, exactly as you wrote.
## My math follos this

Nm12<-maxL[,4]*maxL[,7]/2## num mig from 2 into 1
Nm21<-maxL[,5]*maxL[,8]/2## nummig from 1 into 2

## matrix of Nm12 and Nm21 for all runs
matNm12<-matrix(NA,nrow=N,ncol=15)
matNm21<-matrix(NA,nrow=N,ncol=15)
sdNm12<-rep(NA,N)
sdNm21<-rep(NA,N)
conv<-rep(0,N)
for(i in 1:N){
	matNm12[i,]<-dat[[i]][-16,4]*dat[[i]][-16,7]*.5
	matNm21[i,]<-dat[[i]][-16,5]*dat[[i]][-16,8]*.5
	## sd for top 5
	q<-quantile(dat[[i]][-16,1],probs=10/15)
	sdNm12[i]<-sd(matNm12[i,][which(dat[[i]][-16,1] >= q)])
	sdNm21[i]<-sd(matNm21[i,][which(dat[[i]][-16,1] >= q)])
        conv[i]<-sd(dat[[i]][-16,1][which(dat[[i]][-16,1] >= q)])/mean(dat[[i]][-16,1][which(dat[[i]][-16,1] >= q)])
}

sketchy<-which((Nm12 > 3 | Nm21 > 3) & abs(conv) < 0.01)
#[1] "dd_BNP-17_MRF-17_lowm_im3.txt" "dd_GNP-17_PSP-17_lowm_im3.txt"
#[3] "dd_HNV-17_MRF-17_lowm_im3.txt" "dd_HNV-17_RNV-17_lowm_im3.txt"
## BNP X MRF, other "good" ll give low migration, this solution is odd
## GNP x PSP, other "good" ll give low migration, this solution is odd


## read in geo data
pl<-read.table("admixpoplocs.txt",header=FALSE)
# Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

ll<-matrix(NA,nrow=10,ncol=2)
for(j in 1:10){
        ll[j,1]<-deg2rad(pl[j,2])
        ll[j,2]<-deg2rad(pl[j,3])
}


# Calculates the geodesic distance between two points specified by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
  R <- 6371 # Earth mean radius [km]
  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
  return(d) # Distance in km
}

gd<-matrix(NA,nrow=10,ncol=10)
for(j in 1:10){for(k in 1:10){
        gd[j,k]<-gcd.slc(ll[j,1],ll[j,2],ll[k,1],ll[k,2])
}}

good<-which(sdNm12 < 1 & sdNm21 < 1 & conv < 0.01)
## 35 good, which is 78%
## outlier is MRF x PSP
cor.test(rep(gd[lower.tri(gd)][good],2),c(Nm12[good],Nm21[good]))
## r = -0.29, P = 0.015, 95% CI -0.49 to -0.06, for simple pearson, so point estimate valid only
library(scales)
pdf("fig_dadimig.pdf",width=10,height=5)
cl<-1.6;ca<-1.15;cm<-1.6
cs<-alpha("gray10",.8)
layout(matrix(c(1,2,1,3),nrow=2,ncol=2,byrow=TRUE),widths=c(5,5),heights=c(2.5,2.5))
par(mar=c(4.5,5,2.5,2))
plot(rep(gd[lower.tri(gd)][good],2),c(Nm12[good],Nm21[good]),pch=19,col=cs,xlab="Geographic distance (km)",ylab="Number of migrants",cex.lab=cl,cex.axis=ca)
mtext("r = -0.29",side=3,adj=.85,line=-2,cex=1.4)
title("(A) Gene flow estimates",cex.main=cm)

plot(c(0,1),c(0,1),type="n",xlab="",ylab="",axes=FALSE)
title("(B) Model fit XX x XX",cex.main=cm)

plot(c(0,1),c(0,1),type="n",xlab="",ylab="",axes=FALSE)
title("(C) Model fit XX x XX",cex.main=cm)

dev.off()

plot(rep(gd[lower.tri(gd)][good],2),c(Nm12[good],Nm21[good]))

library(xtable)
xtable(data.frame(imf[good],round(maxL[good,-1],4)))

