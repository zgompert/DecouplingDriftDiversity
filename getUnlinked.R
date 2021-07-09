## 
snps<-read.table("snps.txt",header=FALSE)
L<-dim(snps)[1]
dif1<-snps[-1,1]-snps[-L,1]
dif2<-abs(snps[-1,2]-snps[-L,2])
ld<-1000
keep<-rep(TRUE,L)
for(i in 2:L){
	if(dif1[i-1]!=0 | dif2[i-1]>ld){
		keep[i]<-TRUE
	}
	else{
		keep[i]<-FALSE
	}
}

write.table(keep,"prunedSnps.txt",row.names=FALSE,col.names=FALSE)
