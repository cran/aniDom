plot_winner_prob <-
function(diff.rank, a, b) {

	diff.rank.norm <- diff.rank/max(diff.rank)

	plot(diff.rank, 0.5+0.5/(1+exp(-diff.rank.norm*a+b)),ylim=c(0.5,1), type='l',ylab="P of higher rank winning", xlab="Difference in rank")

}
