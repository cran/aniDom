plot_ranks <- function(ranks, plot.CIs=FALSE, ordered.by.rank=TRUE,identities=NULL,plot.identities=TRUE,colors=NULL) {
	
	ranks <- apply(ranks,2,function(x) { rank(-x) })

	if (is.null(identities)) {
		identities <- rownames(ranks)
	}

	if (dim(ranks)[2] > 1) {
		mean.ranks <- rowMeans(ranks)
	} else {
		mean.ranks <- ranks
	}

	if (is.null(colors)) {
		colors <- rep("black",length(identities))
	}

	if (ordered.by.rank==TRUE) {
		colors <- colors[order(mean.ranks)]
		identities <- identities[order(mean.ranks)]
		if (plot.CIs==TRUE) {
			CIs <- apply(ranks,1,quantile,c(0.025,0.975),na.rm=TRUE)
			CIs <- CIs[,order(mean.ranks)]
		}
		mean.ranks <- mean.ranks[order(mean.ranks)]
	} else {
		if (plot.CIs==TRUE) {
			CIs <- apply(ranks,1,quantile,c(0.025,0.975),na.rm=TRUE)
		}
	}

	x <- 1:length(identities)
	
	if (plot.identities==TRUE) {
		plot(x, mean.ranks,pch=20,cex=1.5,axes=FALSE,xlab="Identity",ylab="Dominance rank",col=colors,ylim=c(max(ranks,na.rm=T),min(ranks,na.rm=T)))
		axis(2)
		axis(1,at=x,labels=identities,las=3)
		box()
	} else {
		plot(x,mean.ranks,pch=20,cex=1.5,xlab="Identity",ylab="Dominance rank",col=colors,ylim=c(max(ranks,na.rm=T),1))
	}

	if (plot.CIs==TRUE) {
		arrows(x,CIs[1,],x,CIs[2,],code=3,angle=90,length=0.1,col=colors)
	}
}
