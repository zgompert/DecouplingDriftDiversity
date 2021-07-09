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

## calc fst
fst<-function(p1=NA,p2=NA){
    pbar<-(p1+p2)/2
    Ht<-2 * pbar * (1-pbar)
    Hs<-((p1 * (1-p1) * 2) + (p2 * (1-p2) * 2))/2
    gst<-mean(Ht-Hs)/mean(Ht)
    return(gst)
}

fstM<-matrix(NA,nrow=N,ncol=N)
for(i in 1:(N-1)){for(j in (i+1):N){
    fstM[i,j]<-fst(p1=P[,i],p2=P[,j])
    fstM[j,i]<-fstM[i,j]
}}

pids<-read.table("../AlleleFreqs/popIds.txt")
colnames(fstM)<-pids[,1]
rownames(fstM)<-pids[,1]

write.table(round(fstM,4),file="fstMatrix.txt",quote=FALSE)

## 2017 submatrix
yr17<-which(grepl(pids[,1],pattern="17"))
fstM17<-fstM[yr17,yr17]

pops<-unlist(strsplit(as.character(pids[,1]),"-"))[seq(1,66,2)]
upops<-unique(pops)
Np<-length(upops)
Ppops<-matrix(NA,nrow=L,ncol=Np)
for(k in 1:Np){
	a<-which(pops == upops[k])
	Ppops[,k]<-apply(P[,a],1,mean)
}


fstPopsM<-matrix(NA,nrow=Np,ncol=Np)
for(i in 1:(Np-1)){for(j in (i+1):Np){
    fstPopsM[i,j]<-fst(p1=Ppops[,i],p2=Ppops[,j])
    fstPopsM[j,i]<-fstPopsM[i,j]
}}


