## equilibrium island model fit in Stan for the 7 core JH  populations
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

files17<-list.files(pattern="17_SAM")
jh<-c(1,3,6:10)
fjh<-files17[jh]
nL<-11638
nPop<-7

Pmat<-matrix(NA,nrow=nL,ncol=nPop)
for(j in 1:nPop){
    pp<-read.table(fjh[j],header=FALSE)
    Pmat[,j]<-pp[,3]
    }
Pmat[Pmat==0]<-0.0001
Pmat[Pmat==1]<-0.9999

## four chains, 2000 steps, 1000 warup, thin = 1

Nm<-matrix(NA,nrow=10,ncol=10)
## repeat 10 times
for(x in 1:10){
    dat<-list(L=1000,N=nPop,p=Pmat[sample(1:nL,1000,replace=FALSE),])
    fit<-stan("beta_ismod.stan",data=dat)
    o<-summary(fit)
    Nm[x,]<-o[[1]][1,]
}    
    
## some runs failed to sample well (ESS ~2 and warning messages), whereas six did quite well
# 1 = mean, 2 = se_mean, 3 = sd, 4 = 2.5%, 5 = 25%, 6=50%, 7=75%, 8=97.5%, n=N_eff, and 10 = Rhat
#         [,1]        [,2]      [,3]     [,4]     [,5]     [,6]     [,7]
#[1,] 8.069599 0.004550054 0.1812157 7.720992 7.949332 8.066288 8.189734
#[2,] 7.714724 0.004531139 0.1762797 7.377417 7.596807 7.713301 7.831521
#[3,] 7.877188 0.004378000 0.1811698 7.535876 7.753927 7.877148 7.998167
#[4,] 7.850707 0.004367101 0.1791177 7.509159 7.729221 7.849096 7.969304
#[5,] 7.751677 0.004497602 0.1776508 7.408881 7.632035 7.747851 7.872961
#[6,] 7.624365 0.004077869 0.1769505 7.291507 7.504084 7.619054 7.743555
#         [,8]     [,9]    [,10]
#[1,] 8.430833 1586.202 1.001691
#[2,] 8.070245 1513.525 1.001763
#[3,] 8.232193 1712.459 1.002035
#[4,] 8.206694 1682.249 1.001841
#[5,] 8.107659 1560.171 1.001687
#[6,] 7.984130 1882.942 1.001410
   

### trying 2013 and 2015 samples
## equilibrium island model fit in Stan for the 6 core JH  populations
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

files13<-list.files(pattern="13_SAM")
jh<-c(1,3,6:10)
fjh<-files13[jh]
nL<-11638
nPop<-7

Pmat<-matrix(NA,nrow=nL,ncol=nPop)
for(j in 1:nPop){
    pp<-read.table(fjh[j],header=FALSE)
    Pmat[,j]<-pp[,3]
    }
Pmat[Pmat==0]<-0.0001
Pmat[Pmat==1]<-0.9999

## four chains, 2000 steps, 1000 warup, thin = 1

Nm13<-matrix(NA,nrow=10,ncol=10)
## repeat 10 times
for(x in 1:10){
    dat<-list(L=1000,N=nPop,p=Pmat[sample(1:nL,1000,replace=FALSE),])
    fit<-stan("beta_ismod.stan",data=dat)
    o<-summary(fit)
    Nm13[x,]<-o[[1]][1,]
}    

files15<-list.files(pattern="15_SAM")
jh<-c(1,3,6:9)
fjh<-files15[jh]
nL<-11638
nPop<-6

Pmat<-matrix(NA,nrow=nL,ncol=nPop)
for(j in 1:nPop){
    pp<-read.table(fjh[j],header=FALSE)
    Pmat[,j]<-pp[,3]
    }
Pmat[Pmat==0]<-0.0001
Pmat[Pmat==1]<-0.9999

## four chains, 2000 steps, 1000 warup, thin = 1

Nm15<-matrix(NA,nrow=10,ncol=10)
## repeat 10 times
for(x in 1:10){
    dat<-list(L=1000,N=nPop,p=Pmat[sample(1:nL,1000,replace=FALSE),])
    fit<-stan("beta_ismod.stan",data=dat)
    o<-summary(fit)
    Nm15[x,]<-o[[1]][1,]
}    
 
save(list=ls(),file="MigStan13and15.rdat")
