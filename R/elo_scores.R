elo_scores<- function (winners, losers, identities = NULL, sigmoid.param = 1/100,
                              K = 200, init.score = 0, randomise = FALSE, n.rands = 1000,
                              return.as.ranks = FALSE, return.trajectories = FALSE, dates = NULL){
  
  if (is.null(identities)) {
    identities <- unique(c(winners, losers))
  }
  n.inds <- length(identities)
  if (randomise == FALSE) {
    n.rands <- 1
  } else if (!is.null(dates)) {
	stop("Cannot randomise and keep dates consistent")
  }
  if (sum(c(winners, losers) %in% identities) < length(c(winners, losers))) {
    stop("Not all winners and/or losers are contained in identities")
  }
  T <- length(winners)
  if (return.trajectories) {
    if (randomise == TRUE) {
      all.scores <- array(init.score, c(n.inds, T + 1, n.rands))
    } else {
      all.scores <- array(init.score, c(n.inds, T + 1))
      if(!(is.null(dates))){ #newly implemented if statement
        dates.character <- as.character(dates)
        all.dated.scores <- array(init.score, c(n.inds, length(unique(dates.character))),dimnames = list(identities,unique(dates.character)))
      }
    }
  } else {
    all.scores <- array(init.score, c(n.inds, n.rands))
  }
  rownames(all.scores) <- identities
  if (length(K) == 1) {
    K <- rep(K, T)
  }
  for (r in 1:n.rands) {
    if (randomise == FALSE) {
      ord <- 1:T
    } else {
      ord <- sample(1:T, T, replace = F)
    }
    winners.perm <- winners[ord]
    losers.perm <- losers[ord]
    if(!(is.null(dates))){ #newly implemented if statement
      dates.perm <- dates.character[ord]
      dated.scores <- array(NA, c(n.inds, length(unique(dates.character))),dimnames = list(identities,unique(dates.character)))
    }
    scores <- array(NA, c(n.inds, T + 1))
    scores[, 1] <- init.score
    
    for (i in 1:T) {
      scores[, i + 1] <- scores[, i]
      winner <- which(identities == winners.perm[i])
      loser <- which(identities == losers.perm[i])
      if(!(is.null(dates))){ #newly implemented if statement
        date <- which(dates.character == dates.perm[i])
      }
      p <- 1/(1 + exp(-sigmoid.param * abs((scores[winner, i] - scores[loser, i]))))
      if (scores[winner, i] >= scores[loser, i]) {
        scores[winner, i + 1] <- scores[winner, i] + 
          (1 - p) * K[i]
        scores[loser, i + 1] <- scores[loser, i] - (1 - p) * K[i]
        if(!(is.null(dates))){ #newly implemented if statement
          dated.scores[winner,dates.perm[i]] <- scores[winner, i] + (1 - p) * K[i]
          dated.scores[loser,dates.perm[i]] <- scores[loser, i] - (1 - p) * K[i]
        }
      } else {
        scores[winner, i + 1] <- scores[winner, i] + p * K[i]
        scores[loser, i + 1] <- scores[loser, i] - p * K[i]
        if(!(is.null(dates))){ #newly implemented if statement
          dated.scores[winner,dates.perm[i]] <- scores[winner, i] + p * K[i]
          dated.scores[loser,dates.perm[i]] <- scores[loser, i] - p * K[i]
        }
      }
    }
    if (return.trajectories) {
      if (randomise == TRUE) {
        all.scores[, , r] <- scores
      } else {
        all.scores <- scores
        if(!(is.null(dates))){ #newly implemented if statement
          all.dated.scores <- dated.scores
        }
      }
    } else {
      all.scores[, r] <- scores[, T + 1]
    }
  }
  freq <- table(factor(c(winners, losers), levels = identities))
  if (length(dim(all.scores))==3) {
  	all.scores[which(identities %in% names(freq)[which(freq == 0)]), , ] <- NA
  } else {
	all.scores[which(identities %in% names(freq)[which(freq == 0)]), ] <- NA
  }
  if(!(is.null(dates))){#newly implemented if statement
   all.dated.scores[which(identities %in% names(freq)[which(freq == 0)]), ] <- NA
   invisible(all.dated.scores)
  } else {
    if (return.as.ranks == TRUE) {
      all.ranks <- apply(all.scores, 2, function(x) {rank(-x)})
      all.ranks[is.na(all.scores)] <- NA
      all.scores <- all.ranks
    }
    invisible(all.scores)
  }
}