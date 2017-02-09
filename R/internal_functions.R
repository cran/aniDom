
select_interactants <- function(hierarchy,biased,ids=NULL) {

	if (biased == FALSE) {
		interactants <- sample(hierarchy$ID,2,prob=hierarchy$Prob)
	} else {
		rank.diffs <- unique(ids$Rank.diff)
		rank.diff.to.pick <- sample(rank.diffs,1,prob=(1-rank.diffs/(max(rank.diffs)+1)))
		ids <- ids[which(ids$Rank.diff==rank.diff.to.pick),]
		int.row <- sample(1:nrow(ids),1,prob=(ids$Prob1+ids$Prob2))
		interactants <- c(ids$ID1[int.row],ids$ID2[int.row])
	}
	return(interactants)

}

calculate_winner <- function(rank1, rank2, a, b, max.diff.rank) {
	
	diff.rank <- abs(rank1 - rank2)
	#diff.rank.norm <- diff.rank/max(diff.rank)
	diff.rank.norm <- diff.rank/max.diff.rank

	p.win <- 0.5+0.5/(1+exp(-diff.rank.norm*a+b))

	winner <- sample(c(1,2),1,prob=c(p.win,1-p.win))

	if (winner == 1) {
		if (rank1 < rank2) {
			winner.loser <- c(1,2)
		} else {
			winner.loser <- c(2,1)
		}
	} else {
		if (rank1 > rank2) {
			winner.loser <- c(1,2)
		} else {
			winner.loser <- c(2,1)
		}
	}
	return(winner.loser)

}
