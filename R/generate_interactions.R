generate_interactions <-
function(N.inds, N.obs, a, b, id.biased=FALSE, rank.biased=FALSE) {

	hierarchy <- data.frame(ID=1:N.inds, Rank=(1:N.inds), Prob=1/N.inds)

	interactions <- data.frame(Winner=rep(NA,N.obs), Loser=rep(NA,N.obs))

	if (id.biased==TRUE) {
		x <- rgamma(N.inds,0.5)
		hierarchy$Prob <- x/sum(x)
	}

	if (rank.biased==TRUE) {
		ids <- expand.grid(ID1=hierarchy$ID,ID2=hierarchy$ID)
		ids <- ids[which(ids[,1] != ids[,2]),]
		ids$Rank1 <- hierarchy$Rank[match(ids$ID1,hierarchy$ID)]
		ids$Rank2 <- hierarchy$Rank[match(ids$ID2,hierarchy$ID)]
		ids$Prob1 <- hierarchy$Prob[match(ids$ID1,hierarchy$ID)]
		ids$Prob2 <- hierarchy$Prob[match(ids$ID2,hierarchy$ID)]
		ids$Rank.diff <- abs(ids$Rank1-ids$Rank2)
	}

	for (i in 1:N.obs) {
		
		ints <- select_interactants(hierarchy, rank.biased, ids)
		outcome <- calculate_winner(hierarchy$Rank[ints[1]],hierarchy$Rank[ints[2]],a,b,max(hierarchy$Rank)-min(hierarchy$Rank))

		interactions$Winner[i] <- hierarchy$ID[ints[outcome[1]]]
		interactions$Loser[i] <- hierarchy$ID[ints[outcome[2]]]
	
	}

	return(list(hierarchy=hierarchy,interactions=interactions))

}
