plot_hierarchy_shape <-
function(identity, rank, winners, losers, fitted=FALSE) {

	winners.rank <- rank[match(winners,identity)]
	losers.rank <- rank[match(losers,identity)]
	xx <- winners.rank-losers.rank
	x <- 1:(max(abs(xx)))
	y <- rep(NA,length(x))
	for (i in 1:length(x)) {
		y[i] <- sum(xx==-x[i])/sum(abs(xx)==x[i])
	}
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]

	plot(x,y, xlab="rank difference", ylab="probability of dominant winning", ylim=c(min(y),1), pch=20, cex=2)

	if (fitted) {
		l <- loess(y~x)
		lines(x,l$fitted, col="red", lwd=2)
	}

	invisible(data.frame(Rank.diff=x,Prob.dom.win=y))

}
