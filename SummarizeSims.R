## summary plots
library(scales)
## diversity over time
f_div<-list.files(pattern="sim_log_[0-9]")
div_p1_mig<-vector("list",10)
for(i in 1:10){
	div_p1_mig[[i]]<-read.table(f_div[i],header=TRUE,sep=",")
}

f_div_15<-list.files(pattern="sim_log_p15")
div_p15_mig<-vector("list",10)
for(i in 1:10){
	div_p15_mig[[i]]<-read.table(f_div_15[i],header=TRUE,sep=",")
}

par(mfrow=c(2,1))
plot(div_p1_mig[[1]]$generation,div_p1_mig[[1]]$Het,type='l')
for(j in 2:10){
	lines(div_p1_mig[[j]]$generation,div_p1_mig[[j]]$Het)
	lines(div_p15_mig[[j]]$generation,div_p15_mig[[j]]$Het,col="red")
}
plot(div_p1_mig[[1]]$generation,div_p1_mig[[1]]$Theta,type='l')
for(j in 2:10){
	lines(div_p1_mig[[j]]$generation,div_p1_mig[[j]]$Theta)
	lines(div_p15_mig[[j]]$generation,div_p15_mig[[j]]$Theta,col="red")
}

## no migration
f_div_nm<-list.files(pattern="sim_nm_log_[0-9]")
div_p1_nm<-vector("list",10)
for(i in 1:10){
	div_p1_nm[[i]]<-read.table(f_div_nm[i],header=TRUE,sep=",")
}
f_div_15_nm<-list.files(pattern="sim_nm_log_p15")
div_p15_nm<-vector("list",10)
for(i in 1:10){
	div_p15_nm[[i]]<-read.table(f_div_15_nm[i],header=TRUE,sep=",")
}

par(mfrow=c(2,1))
plot(div_p1_nm[[1]]$generation,div_p1_nm[[1]]$Het,type='l')
for(j in 2:10){
	lines(div_p1_nm[[j]]$generation,div_p1_nm[[j]]$Het)
	lines(div_p15_nm[[j]]$generation,div_p15_nm[[j]]$Het,col="red")
}
plot(div_p1_nm[[1]]$generation,div_p1_nm[[1]]$Theta,type='l')
for(j in 2:10){
	lines(div_p1_nm[[j]]$generation,div_p1_nm[[j]]$Theta)
	lines(div_p15_nm[[j]]$generation,div_p15_nm[[j]]$Theta,col="red")
}

## low migration
f_div_lm<-list.files(pattern="sim_lm_log_[0-9]")
div_p1_lm<-vector("list",10)
for(i in 1:10){
	div_p1_lm[[i]]<-read.table(f_div_lm[i],header=TRUE,sep=",")
}
f_div_15_lm<-list.files(pattern="sim_lm_log_p15")
div_p15_lm<-vector("list",10)
for(i in 1:10){
	div_p15_lm[[i]]<-read.table(f_div_15_lm[i],header=TRUE,sep=",")
}

par(mfrow=c(2,1))
plot(div_p1_lm[[1]]$generation,div_p1_lm[[1]]$Het,type='l',ylim=c(0,0.004))
for(j in 2:10){
	lines(div_p1_lm[[j]]$generation,div_p1_lm[[j]]$Het)
	lines(div_p15_lm[[j]]$generation,div_p15_lm[[j]]$Het,col=alpha("red",.5))
}
plot(div_p1_lm[[1]]$generation,div_p1_lm[[1]]$Theta,type='l')
for(j in 2:10){
	lines(div_p1_lm[[j]]$generation,div_p1_lm[[j]]$Theta)
	lines(div_p15_lm[[j]]$generation,div_p15_lm[[j]]$Theta,col=alpha("red",.5))
}

## ne
nf<-list.files(pattern="ne_")
Ne<-vector("list",3)
for(i in 1:3){
	Ne[[i]]<-read.table(nf[i],header=FALSE)
}


### make figure
pdf("fig_slim.pdf",width=9,height=8)
par(mfrow=c(2,2))
par(mar=c(4.5,5,2.5,1))
cl<-1.7;ca<-1.2;cm<-1.7
#cs<-brewer.pal(n=3,"Dark2")
cs<-c("skyblue1","skyblue3","skyblue4")
## high migration
plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_mig[[i]]$generation[a],div_p1_mig[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_mig[[i]]$generation,div_p15_mig[[i]]$Het,col=alpha(cs[3],.8))
}
title(main="(A) Diversity for m = 0.01",cex.main=cm)

plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_lm[[i]]$generation[a],div_p1_lm[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_lm[[i]]$generation,div_p15_lm[[i]]$Het,col=alpha(cs[2],.5))
}
title(main="(B) Diversity for m = 0.001",cex.main=cm)

plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_nm[[i]]$generation[a],div_p1_nm[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_nm[[i]]$generation,div_p15_nm[[i]]$Het,col=alpha(cs[1],.8))
}
title(main="(C) Diversity for m = 0.0",cex.main=cm)

boxplot(Ne[[1]][,4],Ne[[2]][,4],Ne[[3]][,4],outline=FALSE,ylim=c(0,800),ylab=expression(paste("Variance ",N[e],sep="")),xlab="Gene flow",names=c("m=0.01","m=0.001","m=0.0"),cex.lab=cl,cex.axis=ca)## cuts 5 points > 800
points(jitter(rep(1,360),factor=2),Ne[[1]][,4],col=alpha(cs[3],.5),pch=19)
points(jitter(rep(2,360),factor=2),Ne[[2]][,4],col=alpha(cs[2],.5),pch=19)
points(jitter(rep(3,360),factor=2),Ne[[3]][,4],col=alpha(cs[1],.5),pch=19)
abline(h=173,lty=2,col="gray10")
title(main="(D) Effective population size",cex.main=cm)
dev.off()


### make figure 4g
## ne
nf<-list.files(pattern="ne2_")
Ne<-vector("list",3)
for(i in 1:3){
	Ne[[i]]<-read.table(nf[i],header=FALSE)
}
pdf("fig_slim4g.pdf",width=9,height=8)
par(mfrow=c(2,2))
par(mar=c(4.5,5,2.5,1))
cl<-1.7;ca<-1.2;cm<-1.7
#cs<-brewer.pal(n=3,"Dark2")
cs<-c("skyblue1","skyblue3","skyblue4")
## high migration
plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_mig[[i]]$generation[a],div_p1_mig[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_mig[[i]]$generation,div_p15_mig[[i]]$Het,col=alpha(cs[3],.8))
}
title(main="(A) Diversity for m = 0.01",cex.main=cm)

plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_lm[[i]]$generation[a],div_p1_lm[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_lm[[i]]$generation,div_p15_lm[[i]]$Het,col=alpha(cs[2],.5))
}
title(main="(B) Diversity for m = 0.001",cex.main=cm)

plot(c(0,300000),c(0,0.004),type='n',xlab="Generation",ylab=expression(paste("Nucleotide diversity (",pi,")",sep="")),cex.lab=cl,cex.axis=ca)
for(i in 1:10){
	a<-which(div_p1_lm[[i]]$generation < 100000)
	lines(div_p1_nm[[i]]$generation[a],div_p1_nm[[i]]$Het[a],col=alpha("darkgray",.5))
	lines(div_p15_nm[[i]]$generation,div_p15_nm[[i]]$Het,col=alpha(cs[1],.8))
}
title(main="(C) Diversity for m = 0.0",cex.main=cm)

boxplot(Ne[[1]][,1],Ne[[2]][,1],Ne[[3]][,1],outline=FALSE,ylim=c(0,650),ylab=expression(paste("Variance ",N[e],sep="")),xlab="Gene flow",names=c("m=0.01","m=0.001","m=0.0"),cex.lab=cl,cex.axis=ca)## cuts 5 points > 800
points(jitter(rep(1,250),factor=2),Ne[[1]][,1],col=alpha(cs[3],.5),pch=19)
points(jitter(rep(2,211),factor=2),Ne[[2]][,1],col=alpha(cs[2],.5),pch=19)
points(jitter(rep(3,241),factor=2),Ne[[3]][,1],col=alpha(cs[1],.5),pch=19)
abline(h=173,lty=2,col="gray10")
title(main="(D) Effective population size",cex.main=cm)
dev.off()
