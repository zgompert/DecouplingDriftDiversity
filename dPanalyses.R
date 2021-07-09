##

dat<-matrix(scan("pest_slim_out_gen_1",n=2418204*6,sep=" "),nrow=2418204,ncol=6,byrow=TRUE)
Pgen<-vector("list",6)
L<-vector("list",6)
snps<-paste(dat[,1],dat[,2],dat[,3],sep="_")
for(k in 1:6){
	L[[k]]<-unique(snps[dat[,5] ==k])
}

set1<-L[[1]][which(L[[1]] %in% L[[3]] & L[[1]] %in% L[[2]])]
set2<-L[[4]][which(L[[4]] %in% L[[6]] & L[[4]] %in% L[[5]])]

for(k in 1:3){
	Pgen[[k]]<-matrix(NA,nrow=length(set1),ncol=36)
	for(j in 1:36){
		subdat<-dat[which(dat[,4]==j & dat[,5]==k),]
		subsnps<-snps[which(dat[,4]==j & dat[,5]==k)]
		xx<-which(subsnps %in% set1)
		yy<-which(set1 %in% subsnps)
		Pgen[[k]][yy,j]<-subdat[xx,6]
	}
}

