## basic summaries and plots of Ne
ne<-read.table("SummaryNe.txt",header=TRUE)

uset<-as.character(unique(ne$set))

pdf("varNe.pdf",width=7,height=12)
par(mfrow=c(3,1))
par(mar=c(4,5,3,1))
for(i in 1:3){
	x<-which(ne$set==uset[i])
	xx<-barplot(ne[x,]$median,ylim=c(0,325),names.arg=ne[x,]$pop,ylab="Var. effective pop. size",cex.lab=1.8,cex.axis=1.3,cex.names=1.5)
	segments(xx,ne[x,]$lb95,xx,ne[x,]$ub95)
	title(main=uset[i],cex.main=1.6)
}
dev.off()

