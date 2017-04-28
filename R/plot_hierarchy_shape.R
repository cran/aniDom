plot_hierarchy_shape <-
function(identity, rank, winners, losers, fitted=FALSE) {

	winners.rank <- rank[match(winners,identity)]
	losers.rank <- rank[match(losers,identity)]
	xx <- winners.rank-losers.rank
	x <- 1:(max(abs(xx)))
	y <- rep(NA,length(x))
	CI.upper <- y
	CI.lower <- y
	for (i in 1:length(x)) {
		y[i] <- sum(xx==-x[i])/sum(abs(xx)==x[i])
		CI.upper[i] <- y[i] + sqrt(y[i]*(1-y[i])/sum(abs(xx)==x[i])) + 0.5/sum(abs(xx)==x[i])
		CI.upper[i] <- min(CI.upper[i],1)
		CI.lower[i] <- y[i] - sqrt(y[i]*(1-y[i])/sum(abs(xx)==x[i])) - 0.5/sum(abs(xx)==x[i])
		CI.lower[i] <- max(CI.lower[i],0)
	}
	x <- x[!is.na(y)]
	y <- y[!is.na(y)]

	plot(x,y, xlab="Difference in rank", ylab="Probability that higher rank wins", ylim=c(min(0.5,min(y)),1), pch=20, cex=2)
	arrows(x,CI.lower,x,CI.upper,length=0.1,angle=90,code=3)

	if (fitted) {
		l <- loess(y~x)
		lines(x,l$fitted, col="red", lwd=2)
	}

	invisible(data.frame(Rank.diff=x,Prob.dom.win=y,CI.upper=CI.upper,CI.lower=CI.lower))

}
