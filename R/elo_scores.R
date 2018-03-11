elo_scores <- function (winners, losers, identities = NULL, sigmoid.param = 1/100, 
                          K = 200, init.score = 0, randomise = FALSE, n.rands = 1000, 
                          return.as.ranks = FALSE, return.trajectories = FALSE) 
{
  if (is.null(identities)) {
    identities <- unique(c(winners, losers))
  }
  n.inds <- length(identities)
  if (randomise == FALSE) {
    n.rands <- 1
  }
  if (sum(c(winners, losers) %in% identities) < length(c(winners, 
                                                         losers))) {
    stop("Not all winners and/or losers are contained in identities")
  }
  T <- length(winners)
  if (return.trajectories) {
    if (randomise == TRUE) {
      all.scores <- array(init.score, c(n.inds, T + 1, 
                                        n.rands))
    }
    else {
      all.scores <- array(init.score, c(n.inds, T + 1))
    }
  }
  else {
    all.scores <- array(init.score, c(n.inds, n.rands))
  }
  rownames(all.scores) <- identities
  if (length(K) == 1) {
    K <- rep(K, T)
  }
  for (r in 1:n.rands) {
    if (randomise == FALSE) {
      ord <- 1:T
    }
    else {
      ord <- sample(1:T, T, replace = F)
    }
    winners.perm <- winners[ord]
    losers.perm <- losers[ord]
    scores <- array(NA, c(n.inds, T + 1))
    scores[, 1] <- init.score
    for (i in 1:T) {
      scores[, i + 1] <- scores[, i]
      winner <- which(identities == winners.perm[i])
      loser <- which(identities == losers.perm[i])
      p <- 1/(1 + exp(-sigmoid.param * abs((scores[winner, #this is the abs() that was missing
                                                   i] - scores[loser, i]))))
      if (scores[winner, i] >= scores[loser, i]) {
        scores[winner, i + 1] <- scores[winner, i] + 
          (1 - p) * K[i]
        scores[loser, i + 1] <- scores[loser, i] - (1 - 
                                                      p) * K[i]
      }
      else {
        scores[winner, i + 1] <- scores[winner, i] + 
          p * K[i]
        scores[loser, i + 1] <- scores[loser, i] - 
          p * K[i]
      }
    }
    if (return.trajectories) {
      if (randomise == TRUE) {
        all.scores[, , r] <- scores
      }
      else {
        all.scores <- scores
      }
    }
    else {
      all.scores[, r] <- scores[, T + 1]
    }
  }
  freq <- table(factor(c(winners, losers), levels = identities))
  all.scores[which(identities %in% names(freq)[which(freq == 
                                                       0)]), ] <- NA
  if (return.as.ranks == TRUE) {
    all.ranks <- apply(all.scores, 2, function(x) {
      rank(-x)
    })
    all.ranks[is.na(all.scores)] <- NA
    all.scores <- all.ranks
  }
  invisible(all.scores)
}