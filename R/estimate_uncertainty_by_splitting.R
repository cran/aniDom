estimate_uncertainty_by_splitting <-
function(winners, losers, identities=NULL, sigmoid.param=1/100, K=200, init.score=0, randomise=FALSE, n.rands=1000) {

  	if (is.null(identities)) {
		identities <- unique(c(winners,losers))  
  	}

	n.inds <- length(identities)

  	if (randomise==FALSE) {
    		n.rands <- 1
  	}
	n.observations <- length(winners)
	obs1 <- seq(1,n.observations/2,1)
	obs2 <- seq(max(obs1)+1,n.observations,1)

	if (randomise==FALSE) {
		scores1 <- elo_scores(winners[obs1],losers[obs1], identities, sigmoid.param, K, init.score, randomise, n.rands)
		scores2 <- elo_scores(winners[obs2],losers[obs2], identities, sigmoid.param, K, init.score, randomise, n.rands)
		scores.cor <- cor(scores1,scores2,use="complete.obs",method="spearman")
	} else {
		scores.cor <- rep(NA, n.rands)
		for (i in 1:n.rands) {
			new.order <- sample(1:n.observations)
			winners.tmp <- winners[new.order]
			losers.tmp <- losers[new.order]
			scores1 <- elo_scores(winners.tmp[obs1],losers.tmp[obs1], identities, sigmoid.param, K, init.score, randomise=FALSE)
			scores2 <- elo_scores(winners.tmp[obs2],losers.tmp[obs2], identities, sigmoid.param, K, init.score, randomise=FALSE)
			scores.cor[i] <- cor(scores1,scores2,use="complete.obs",method="spearman")
		}
		CIs <- quantile(scores.cor,c(0.025,0.975),na.rm=TRUE)
		scores.cor <- c(mean(scores.cor,na.rm=T), CIs[1], CIs[2])
		names(scores.cor) <- c("Mean",names(CIs))
	}

	invisible(scores.cor)

}
