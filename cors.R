## first read in autosomal and Z allele freqs.
L<-12886 # snps
N<-33 # samples

f_auto<-list.files(path="../AlleleFreqs",pattern=glob2rx("p_auto*SAM_sub.txt"),full.names=TRUE)
f_Z<-list.files(path="../AlleleFreqs",pattern=glob2rx("p_Z*SAM_sub.txt"),full.names=TRUE)

P<-matrix(NA,nrow=L,ncol=N)
for(j in 1:N){
	pa<-read.table(f_auto[j],header=FALSE)
	pz<-read.table(f_Z[j],header=FALSE)
	P[,j]<-c(pa[,3],pz[,3])
}

## snp ids
snps<-read.table("/uufs/chpc.utah.edu/common/home/u6000989/projects/lycaeides_diversity/AlleleFreqs/snpIdsAZ.txt",header=FALSE)

## sample ids
pids<-read.table("popIds.txt")

## test gene vs. selection hypotheses based on patterns of change
## this version uses scaffold-level permutation

## grab data for 2013 (p0) and 2015 (or 2014 for RNV)

jh<-c("BCR","BTB","MRF","PSP","USL","RNV") 
jh13<-which(as.character(pids[,1]) %in% jh & pids[,2] == 13)
jh15<-sort(c(which(as.character(pids[,1]) %in% jh[1:5] & pids[,2] == 15),which(as.character(pids[,1]) %in% jh[6] & pids[,2] == 14)))
jh17<-which(as.character(pids[,1]) %in% jh & pids[,2] == 17)

p_jh13<-apply(P[,jh13],1,mean)
p_jh15<-apply(P[,jh15],1,mean)


## 13 vs 15 core JH pops
ddif<-matrix(NA,nrow=L,ncol=length(jh13))
dchng<-matrix(NA,nrow=L,ncol=length(jh13))
for(j in 1:length(jh13)){
    ddif[,j]<-as.numeric(P[,jh13[j]] > p_jh13)
    dchng[,j]<-as.numeric(P[,jh13[j]] > P[,jh15[j]])
}    

## naive answer, certainly effected by measurement error
obs13v15<-apply(ddif==dchng,2,mean)

## null dist.

NN<-1000
jha<-cbind(jh13,jh15)
n13<-matrix(NA,nrow=NN,ncol=length(jh13))
for(x in 1:NN){
    PN0<-matrix(NA,nrow=L,ncol=length(jh13))
    PN1<-PN0
    sc<-unique(snps[,1])
    Nsc<-length(sc)
    for(j in 1:length(jh13)){
        snull<-sample(1:2,Nsc,replace=TRUE)  
        seta<-which(snps[,1] %in% sc[snull==1])
        setb<-which(snps[,1] %in% sc[snull==2])
        PN0[seta,j]<-P[seta,jh13[j]]
        PN0[setb,j]<-P[setb,jh15[j]]
        PN1[seta,j]<-P[seta,jh15[j]]
        PN1[setb,j]<-P[setb,jh13[j]]
    }
    p_nid13<-apply(PN0,1,mean)
    ndif<-matrix(NA,nrow=L,ncol=length(jh13))
    nchng<-matrix(NA,nrow=L,ncol=length(jh13))
    for(j in 1:length(jh13)){
        ndif[,j]<-as.numeric(PN0[,j] > p_nid13)
        nchng[,j]<-as.numeric(PN0[,j] > PN1[,j])
    }    
    n13[x,]<-apply(ndif==nchng,2,mean)
}    

ps<-rep(NA,length(jh13))
for(j in 1:length(jh13)){
    ps[j]<-mean(n13[,j] >= obs13v15[j])
}
    
ps ## ~0 means convergence towards mean, ~1 means divergence in same direction as difference
##   BCR   BTB   MRF   PSP   RNV   USL
#[1] 0.296 0.002 0.925 1.000 0.692 0.000
mean(apply(n13,2,mean) >= mean(obs13v15))
#[1] 0.6666667

#### PART OF MAIN PLOT ####
boxplot(n13,ylim=c(.71,.8),col="gray",pch=20,cex=.5,names=as.character(pids[jh13,1]))
points(1:6,obs13v15,col="red",pch=20)
##########################

##############################################3
## SKI versus means from all core, 13 and 15 as reference points
## 13 vs 15 core JH pops
ski<-which(pids[,1]=="SKI")
Nski<-length(ski)-1
ddif<-matrix(NA,nrow=L,ncol=Nski)
dchng<-matrix(NA,nrow=L,ncol=Nski)
ddif[,1]<-as.numeric(P[,ski[1]] > p_jh13) ## base 13 vs ski 14
dchng[,1]<-as.numeric(P[,ski[1]] > P[,ski[2]])
ddif[,2]<-as.numeric(P[,ski[1]] > p_jh13) ## base 13 vs ski 15
dchng[,2]<-as.numeric(P[,ski[1]] > P[,ski[3]])
ddif[,3]<-as.numeric(P[,ski[3]] > p_jh15) ## base 15 vs ski 17
dchng[,3]<-as.numeric(P[,ski[3]] > P[,ski[4]])


## naive answer, certainly effected by measurement error
obsSki<-apply(ddif==dchng,2,mean)

## null dist.

NN<-1000
nSki<-matrix(NA,nrow=NN,ncol=Nski)
spairs<-matrix(ski[c(1,2,1,3,3,4)],nrow=3,ncol=2,byrow=TRUE)
p_ski<-list(as.vector(P[,ski[1]]),as.vector(P[,ski[1]]),as.vector(P[,ski[3]]))
for(x in 1:NN){
    PN0<-matrix(NA,nrow=L,ncol=Nski)
    PN1<-PN0
    sc<-unique(snps[,1])
    Nsc<-length(sc)
    for(j in 1:Nski){
        snull<-sample(1:2,Nsc,replace=TRUE)  
        seta<-which(snps[,1] %in% sc[snull==1])
        setb<-which(snps[,1] %in% sc[snull==2])
        PN0[seta,j]<-P[seta,spairs[j,1]]
        PN0[setb,j]<-P[setb,spairs[j,2]]
        PN1[seta,j]<-P[seta,spairs[j,2]]
        PN1[setb,j]<-P[setb,spairs[j,1]]
    }
    p_nidSki<-apply(PN0,1,mean)
    ndif<-matrix(NA,nrow=L,ncol=Nski)
    nchng<-matrix(NA,nrow=L,ncol=Nski)
    for(j in 1:Nski){
        ndif[,j]<-as.numeric(PN0[,j] > p_ski[[j]])
        nchng[,j]<-as.numeric(PN0[,j] > PN1[,j])
    }    
    nSki[x,]<-apply(ndif==nchng,2,mean)
}    

pSki<-rep(NA,Nski)
for(j in 1:Nski){
    pSki[j]<-mean(nSki[,j] >= obsSki[j])
}
    
pSki
#[1] 0.641 0.826 0.304
boxplot(nSki,col="gray",pch=20,cex=.5,names=c("2014","2015","2017"))
points(1:3,obsSki,col="red",pch=20)

## consistency in change through time
Njh<-length(jh13)

dp<-vector("list",3)
for(a in 1:3){
    dp[[a]]<-matrix(NA,nrow=L,ncol=Njh)
}    

for(j in 1:Njh){
    dp[[1]][,j]<-P[,jh15[j]]-P[,jh13[j]]
    dp[[2]][,j]<-P[,jh17[j]]-P[,jh15[j]]
    dp[[3]][,j]<-P[,jh17[j]]-P[,jh13[j]]
}

cors<-matrix(NA,nrow=Njh,ncol=3)
qs<-c(0,0.95,0.99)
for(q in 1:3){
    for(j in 1:Njh){
        lb<-quantile(abs(dp[[1]][,j]),probs=qs[q])
        a<-which(abs(dp[[1]][,j]) >= lb)
        cors[j,q]<-cor(dp[[1]][a,j],dp[[2]][a,j])
    }    
}        
#     [,1]       [,2]       [,3]
#[1,] -0.5405561 -0.7062390 -0.7573922
#[2,] -0.5145238 -0.7023810 -0.8006142
#[3,] -0.3800723 -0.5490568 -0.5862980
#[4,] -0.4750052 -0.6494827 -0.6768982
#[5,] -0.4088871 -0.5991424 -0.6985266
#[6,] -0.5142972 -0.7015102 -0.7638528  
## all negative, makes sense, if 15 is high, then 15-13 > 0 and 17-15 < 0

## now space
spcCor13to15<-vector("list",3)
for(q in 1:3){
    adp<-apply(abs(dp[[1]]),1,mean)
    lb<-quantile(adp,probs=qs[q])
    a<-which(adp >= lb)
    Nc<-(6*5)/2
    ii<-1
    spcCor13to15[[q]]<-as.data.frame(matrix(NA,nrow=Nc,ncol=4))
    for(i in 1:5){for(j in (i+1):6){
        o<-cor.test(dp[[1]][a,i],dp[[1]][a,j])
        spcCor13to15[[q]][ii,3]<-o$estimate
        spcCor13to15[[q]][ii,4]<-o$p.value
        spcCor13to15[[q]][ii,1]<-jh[i]
        spcCor13to15[[q]][ii,2]<-jh[j]
        ii<-ii+1
    }}
}    

#[[1]], full data set, * denotes P < 0.05
#    V1  V2            V3           V4
#1  BCR BTB  0.0125275712 1.550251e-01
#2  BCR MRF -0.0073933778 4.013568e-01
#3  BCR PSP  0.0090892040 3.022145e-01
#4  BCR USL  0.0127788971 1.469096e-01
#5  BCR RNV -0.0038351055 6.633395e-01
#6  BTB MRF  0.0359988536 4.362481e-05 *
#7  BTB PSP  0.0265749585 2.553430e-03 *
#8  BTB USL  0.0099033772 2.609641e-01
#9  BTB RNV -0.0181277195 3.961269e-02 *
#10 MRF PSP  0.0313312448 3.749115e-04 * 
#11 MRF USL  0.0208598486 1.788621e-02 *
#12 MRF RNV -0.0000400627 9.963718e-01
#13 PSP USL  0.0098493484 2.635745e-01
#14 PSP RNV  0.0062867532 4.754819e-01
#15 USL RNV -0.0008447338 9.236143e-01

## space with binary
bSpcCor13to15<-vector("list",3)
for(q in 1:3){
    adp<-apply(abs(dp[[1]]),1,mean)
    lb<-quantile(adp,probs=qs[q])
    a<-which(adp >= lb)
    Nc<-(6*5)/2
    ii<-1
    bSpcCor13to15[[q]]<-as.data.frame(matrix(NA,nrow=Nc,ncol=4))
    for(i in 1:5){for(j in (i+1):6){
        o<-cor.test(as.numeric(dp[[1]][a,i]>0),as.numeric(dp[[1]][a,j]>0))
        bSpcCor13to15[[q]][ii,3]<-o$estimate
        bSpcCor13to15[[q]][ii,4]<-o$p.value
        bSpcCor13to15[[q]][ii,1]<-jh[i]
        bSpcCor13to15[[q]][ii,2]<-jh[j]
        ii<-ii+1
    }}
}    
## full data, note all positive
#    V1  V2          V3           V4
#1  BCR BTB 0.022205662 1.170965e-02 *
#2  BCR MRF 0.010411184 2.373015e-01
#3  BCR PSP 0.004362282 6.204975e-01
#4  BCR USL 0.017367748 4.866857e-02 *
#5  BCR RNV 0.010688865 2.250229e-01
#6  BTB MRF 0.014083829 1.098936e-01
#7  BTB PSP 0.016660304 5.860199e-02
#8  BTB USL 0.030254389 5.929344e-04 *
#9  BTB RNV 0.004033709 6.470602e-01
#10 MRF PSP 0.056445838 1.435987e-10 *
#11 MRF USL 0.027936514 1.516188e-03 *
#12 MRF RNV 0.010592700 2.292227e-01
#13 PSP USL 0.043537362 7.650644e-07 *
#14 PSP RNV 0.008661845 3.255162e-01
#15 USL RNV 0.001045210 9.055629e-01

## version with permutations
bSpcCor13to15<-vector("list",3)
nul<-vector("list",3)
for(q in 1:3){
    adp<-apply(abs(dp[[1]]),1,mean)
    lb<-quantile(adp,probs=qs[q])
    a<-which(adp >= lb)
    Nc<-(6*5)/2
    ii<-1
    bSpcCor13to15[[q]]<-as.data.frame(matrix(NA,nrow=Nc,ncol=4))
    nul[[q]]<-matrix(NA,nrow=1000,ncol=Nc)
    for(i in 1:5){for(j in (i+1):6){
        obs<-mean(as.numeric(dp[[1]][a,i]>0) == as.numeric(dp[[1]][a,j]>0))
        for(n in 1:1000){
            ar<-sample(a,length(a),replace=FALSE)
            nul[[q]][n,ii]<-mean(as.numeric(dp[[1]][a,i]>0)==as.numeric(dp[[1]][ar,j]>0))
        }    
        bSpcCor13to15[[q]][ii,3]<-obs
        bSpcCor13to15[[q]][ii,4]<-mean(nul[[q]][,ii] >= obs)
        bSpcCor13to15[[q]][ii,1]<-jh[i]
        bSpcCor13to15[[q]][ii,2]<-jh[j]
        ii<-ii+1
    }}
}   

#[[1]] full data, p < 0.05 *
#    V1  V2        V3    V4
#1  BCR BTB 0.5108645 0.010
#2  BCR MRF 0.5039578 0.140
#3  BCR PSP 0.5009312 0.332
#4  BCR USL 0.5079156 0.020
#5  BCR RNV 0.5055875 0.130
#6  BTB MRF 0.5088468 0.054
#7  BTB PSP 0.5101661 0.028
#8  BTB USL 0.5162192 0.001
#9  BTB RNV 0.5016297 0.342
#10 MRF PSP 0.5375601 0.000
#11 MRF USL 0.5197113 0.001
#12 MRF RNV 0.5032594 0.121
#13 PSP USL 0.5275493 0.000
#14 PSP RNV 0.5022505 0.164
#15 USL RNV 0.4993016 0.468


mean(bSpcCor13to15[[1]][,3])
#[1] 0.5103834

## all 15 exceed mean of null
.5^15
#[1] 3.051758e-05
## 14 of 15 > 0.5
dbinom(x=14,size=15,prob=0.5)
#[1] 0.0004577637

#### PART OF MAIN PLOT ####
ppids<-paste(bSpcCor13to15[[1]][,1],bSpcCor13to15[[1]][,2],sep="-")
boxplot(nul[[1]],ylim=c(0.476,0.54),col="gray",pch=20,cex=.5,names=as.character(ppids),las=2)
points(1:15,bSpcCor13to15[[1]][,3],col="red",pch=20)
##########################

## time series for SKI and HNV

ts<-c("HNV","SKI") 
tsm<-matrix(NA,nrow=4,ncol=2)
for(j in 1:2){
    tsm[,j]<-which(pids[,1]==ts[j])
}    

## observed consistency
obs<-rep(NA,2)
for(j in 1:length(ts)){
    d1<-as.numeric((P[,tsm[2,j]]-P[,tsm[1,j]]) > 0)
    d2<-as.numeric((P[,tsm[3,j]]-P[,tsm[2,j]]) > 0)
    d3<-as.numeric((P[,tsm[4,j]]-P[,tsm[3,j]]) > 0)
    obs[j]<-mean(d1 == d2 & d2 == d3)
}
obs
#[1] 0.1493093 0.1124476

## null consistency
NN<-1000
tsnull<-matrix(NA,nrow=NN,ncol=2)
for(j in 1:length(ts)){ 
    sc<-unique(snps[,1])
    Nsc<-length(sc)
    for(x in 1:NN){
        PN<-P[,tsm[,j]]
        for(i in 1:Nsc){
            aord<-sample(1:4,4,replace=FALSE)
            a<-which(snps[,1]==sc[i])
            PN[a,]<-PN[a,aord]
        }
        d1<-as.numeric((PN[,2] - PN[,1]) > 0)
        d2<-as.numeric((PN[,3] - PN[,2]) > 0)
        d3<-as.numeric((PN[,4] - PN[,3]) > 0)
        tsnull[x,j]<-mean(d1 == d2 & d2 == d3)
    }
}            

tsp<-rep(NA,2)
for(j in 1:2){
    tsp[j]<-mean(tsnull[,j] > obs[j])
}
tsp
#[1] 0.000 0.593
# clear (albeit weak) signal for HNV, nothing for SKI
summary(obs[1]/tsnull[,1])
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.036   1.128   1.158   1.160   1.190   1.300
sd(obs[1]/tsnull[,1])
#[1] 0.04419403
quantile(obs[1]/tsnull[,1],probs=c(.05,.95))
#1.090610 1.235789

## doesn't seem like HNV is getting more/less similar to any specific other population that makes senses from Fst, but we don't really have anything super close by, gene flow with a nearby population seems like the most likely scenario, especially a very nearby one that we haven't sampled

save(list=ls(),file="cors.rdat")

## make afcors plot
cl<-1.65
cm<-1.5
ca<-1.1

pdf("fig_afChngCors.pdf",width=8,height=8)
layout(matrix(c(1,2,3,3),nrow=2,ncol=2,byrow=TRUE),widths=c(5,3),heights=c(4,4))
par(mar=c(5,5,2.5,1.5))
## consistency in change relative to mean in south
boxplot(n13,ylim=c(.71,.8),col="gray",pch=20,cex=.5,names=as.character(pids[jh13,1]),ylab="Correlation",cex.lab=cl,cex.axis=ca,xlab="Population")
points(1:6,obs13v15,col="red",pch=20)
title(main="(A) Change vs. mean p",cex.main=cm)

## conistency in change in HNV and SKI, for pairs of time intervals
boxplot(tsnull,ylim=c(0.1,0.15),col="gray",pch=20,cex=.5,names=c("HNV","SKI"),ylab="Consistency",cex.lab=cl,cex.axis=ca,xlab="Population")
points(1:2,obs,col="red",pch=20)
title(main="(b) Consistency in time",cex.main=cm)

## consistency in change between pairs from south
boxplot(nul[[1]],ylim=c(0.476,0.54),col="gray",pch=20,cex=.5,names=as.character(ppids),las=3,ylab="Consistency",cex.lab=cl,cex.axis=ca,xlab="Population pair")
points(1:15,bSpcCor13to15[[1]][,3],col="red",pch=20)
title(main="(C) Consistency between population pairs",cex.main=cm)
dev.off()


#############################################################################
################### sims of expected cor dp versus p0 by pbar ###############
Pjh<-P[,jh13]

## BCR, BTB, MRF, PSP, RNV, USL
Ne<-2*c(178.035,271.039,115.897,127.769,100.46,227.146)

mean(7.6/(0.5 * Ne))
#[1] 0.05081616
# thus 0.05 level makes the most sense

M<-c(0.001,0.01,0.05,0.1)

cors<-matrix(0,nrow=length(M),ncol=6)
pv<-cors
for(x in 1:100){
for(i in 1:4){for(j in 1:6){
	ppb<-p_jh13-Pjh[,j]
	m<-M[i]
	p0<-Pjh[,j]
	p1<-p0*(1-m) + p_jh13*m
	p1r<-rbinom(n=length(p0),size=round(Ne[j],0),prob=p1)/round(Ne[j],0)
	p1<-p1r*(1-m) + p_jh13*m
	p1r<-rbinom(n=length(p0),size=round(Ne[j],0),prob=p1)/round(Ne[j],0)
	dp<-p1r-p0
	cors[i,j]<-cors[i,j]+cor(ppb,dp)
	pv[i,j]<-pv[i,j]+cor.test(ppb,dp)$p.value
}}}
cors<-cors/100
pv<-pv/100

library(RColorBrewer)
library(scales)
myc<-c(brewer.pal(n=9,"Set1"),"black")
myc<-alpha(myc,.6)
myc<-myc[c(1,3,6,7,8,10)]

pdf("sfig_migExpect.pdf",width=5,height=5)
par(mar=c(5,5,1,1))
cl<-1.5
ca<-1.1
plot(rep(1:4,6),as.vector(cors),axes=FALSE,xlab="Migration proportion",ylab="Pearson correlation",cex.lab=cl,pch=c(21,19)[c(as.vector(pv) < 0.05)+1],col=rep(myc,each=4))
axis(1,at=1:4,M,cex.axis=ca)
axis(2,cex.axis=ca)
box()
legend(.9,0.35,c("BCR","BTB","MRF","PSP","RNV","USL"),fill=myc,bty='n')
dev.off()

