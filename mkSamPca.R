## version with map

## THIS BIT RUN ON MY COMPUTER
library(sf)
library(raster)
library(dplyr)
library(spData)
#library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps
library(ggplot2) # tidyverse data visualization package

crds<-read.table("coords.txt",header=TRUE)
pnts<-st_as_sf(crds,coords=c("LONGITUDE","LATITUDE"),crs=4326)
west<-us_states[which(us_states$NAME %in% c("Wyoming","Idaho","Montana")),]


library(RColorBrewer)
library(scales)
myc<-brewer.pal(n=10,"Paired")
myc<-c(brewer.pal(n=9,"Set1"),"black")
pdf("mymap.pdf",width=5,height=5)
tm_shape(west) + tm_fill(col="peachpuff",alpha=.5) + tm_borders(col="white",lwd=3) + tm_scale_bar(pos=c("left","bottom"),breaks=c(0,100,200,300),text.size=1.1) + tm_compass(text.size=1.1,position=c("right","top")) + tm_shape(pnts) + tm_dots(col="Pop",palette=myc,alpha=.7,size=.5)
dev.off()

## PCA plot for samtools gls, see also commands.R
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

## plots
library(RColorBrewer)
library(scales)
A<-ids[,2]+2
A[A==16]<-18
myc<-brewer.pal(n=10,"Paired")
myc<-c(brewer.pal(n=9,"Set1"),"black")
myc<-alpha(myc,.6)
pdf("pca2.pdf",width=5,height=10)
par(mfrow=c(2,1))
par(mar=c(4,5.5,2.5,1))

plot(c(0,1),c(0,1),type="n",axes=FALSE,xlab="",ylab="")

title(main="(A) MAP",cex.main=1.5)

ranord<-sample(1:1534,1534,replace=FALSE)

plot(PC$x[ranord,1],PC$x[ranord,2],pch=A[ranord],col=myc[as.factor(ids[ranord,1])],xlab="PC1 (1.8%)",ylab="PC2  (0.2%)",cex.lab=1.5)
legend(-4,6.7,sort(paste(20,unique(ids[,2]),sep="")),pch=c(15,18,17,19),co="gray10",bty='n')
title(main="(B) PCA",cex.main=1.5)
dev.off()

## supp figure
library(RColorBrewer)
library(scales)
A<-ids[,2]+2
A[A==16]<-18
myc<-brewer.pal(n=10,"Paired")
myc<-c(brewer.pal(n=9,"Set1"),"black")
myc<-alpha(myc,.6)
pdf("supp_pca.pdf",width=10,height=10)
par(mfrow=c(2,2))
par(mar=c(5,5.5,1.5,1))

ranord<-sample(1:1534,1534,replace=FALSE)

plot(PC$x[ranord,1],PC$x[ranord,3],pch=A[ranord],col=myc[as.factor(ids[ranord,1])],xlab="PC1 (1.8%)",ylab="PC3  (0.2%)",cex.lab=1.6)

plot(PC$x[ranord,2],PC$x[ranord,3],pch=A[ranord],col=myc[as.factor(ids[ranord,1])],xlab="PC2 (0.2%)",ylab="PC3  (0.2%)",cex.lab=1.6)

plot(PC$x[ranord,1],PC$x[ranord,4],pch=A[ranord],col=myc[as.factor(ids[ranord,1])],xlab="PC1 (1.8%)",ylab="PC4  (0.2%)",cex.lab=1.6)

plot(PC$x[ranord,2],PC$x[ranord,4],pch=A[ranord],col=myc[as.factor(ids[ranord,1])],xlab="PC2 (0.02%)",ylab="PC4  (0.2%)",cex.lab=1.6)
legend(-4,6.7,sort(paste(20,unique(ids[,2]),sep="")),pch=c(15,18,17,19),co="gray10",bty='n')
dev.off()
