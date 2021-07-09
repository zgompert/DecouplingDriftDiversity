## RDA with pop and year 
L<-12886
N<-1536
file<-"G_SAM_sub.txt"
## rows = inds, cols = SNPs
G<-matrix(scan(file,n=L*N,sep=","),nrow=N,ncol=L,byrow=TRUE)
ids<-read.table("indIds.txt",header=FALSE)
og<-which(ids[,1]=="PLUPAC") ## drop outgroup
ids<-ids[-og,]
G<-G[-og,]

PC<-prcomp(G,center=TRUE,scale=FALSE)

library(vegan)
## RDA pop
o1<-rda(G ~ ids[,1])
## RDA year, conditioning on pop
o2<-rda(G ~ as.factor(ids[,2]) + Condition(ids[,1]))

X1<-ids[,1]
X2<-as.factor(ids[,2])
o_adonis<-adonis(G ~ X1 + X2 , permutations=100, strata=X1, method="euclidean")
o_adonis

#Call:
#adonis(formula = G ~ X1 + X2, permutations = 100, method = "euclidean",      strata = X1) 

#Blocks:  strata 
#Permutation: free
#Number of permutations: 100

#Terms added sequentially (first to last)

#            Df SumsOfSqs MeanSqs F.Model      R2   Pr(>F)   
#X1           9     29217  3246.3  4.4275 0.02548 0.009901 **
#X2           3      2445   815.1  1.1117 0.00213 0.009901 **
#Residuals 1521   1115207   733.2         0.97239            
#Total     1533   1146869                 1.00000            
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1










## plots
library(RColorBrewer)
library(scales)
myc<-brewer.pal(n=10,"Paired")
myc<-c(brewer.pal(n=9,"Set1"),"black")
myc<-alpha(myc,.6)
pdf("pca.pdf",width=5,height=10)
par(mfrow=c(2,1))
par(mar=c(4,5.5,2.5,1))

plot(c(0,1),c(0,1),type="n",axes=FALSE,xlab="",ylab="")

title(main="(A) MAP",cex.main=1.5)

plot(PC$x[,1],PC$x[,2],pch=ids[,2]+2,col=myc[as.factor(ids[,1])],xlab="PC1 (1.8%)",ylab="PC2  (0.2%)",cex.lab=1.5)
legend(-4,6.7,unique(ids[,1]),fill=myc,ncol=2,bty='n')
title(main="(B) PCA",cex.main=1.5)
dev.off()
