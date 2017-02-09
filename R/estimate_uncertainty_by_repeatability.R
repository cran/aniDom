estimate_uncertainty_by_repeatability <-
function(winners, losers, identities=NULL, sigmoid.param=1/100, K=200, init.score=0, n.rands=1000) {

	randomise <- TRUE
	
  	if (is.null(identities)) {
		identities <- unique(c(winners,losers))  
  	}

	n.inds <- length(identities)

	result <- elo_scores(winners, losers, sigmoid.param=sigmoid.param, K=K, init.score=init.score, randomise=randomise, n.rands=n.rands)

	results <- data.frame(score=c(result[,1:ncol(result)]))
	results$id <- rep(rownames(result),ncol(result))

	repea <- rptR::rptGaussian(score ~ (1|id), 
                               grname="id", 
                               data=results,
                               nboot=0, npermut=0)

	invisible(as.numeric(repea$R))

}
