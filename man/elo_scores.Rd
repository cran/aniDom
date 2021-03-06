\name{elo_scores}
\alias{elo_scores}
\title{
Calculate Elo scores (with or without time randomisations)
}
\description{
Function that takes winners and losers from dominance interactions and returns either their Elo score or their ranks. The function can generate replicated datasets by randomising the order (time) of the interactions, which can be used to generate 95\% confidence intervals.
}
\usage{
elo_scores(winners, losers, identities = NULL, sigmoid.param = 1/100, 
	K = 200, init.score = 0, randomise = FALSE, n.rands = 1000, 
	return.as.ranks = FALSE, return.trajectories = FALSE, 
	dates = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{winners}{
Vector containing the identity of the winners. This can be integers or strings.
}
  \item{losers}{
Vector containing the identity of the losers. This can be integers or strings. These should be in the same order as the winners (i.e. winners[1] should be the winner and losers[1] should be the loser from the same contest).
}
  \item{identities}{
Optional vector containing the identity of all individuals. This is useful if not all individuals are represented in the winners and losers.
}
  \item{sigmoid.param}{
A parameter of the Elo function that determines the steepness of the sigmoid function (i.e how much the scores change for small differences in rank). Smaller values flatten the shape (more linear), whereas larger values create a stronger threshold function (more change for small differences in rank). 
}
  \item{K}{
K is a parameter of the Elo function that determines the speed at which scores change after an encounter (default K=200).
}
  \item{init.score}{
Parameter of the Elo function that determines the starting score (does not have an effect on relative differences in score).
}
  \item{randomise}{
Boolean (TRUE/FALSE) describing whether to create replicated datasets by randomising the order of the observed interactions (see details).
}
  \item{n.rands}{
The number of randomisations to perform (ignored if randomise=FALSE).
}
  \item{return.as.ranks}{
Boolean (TRUE/FALSE) describing whether to convert scores into ranks before returning the data.
}
  \item{return.trajectories}{
Boolean (TRUE/FALSE) describing whether to return trajectories (the scores after each interaction T) or only the final scores.
}
  \item{dates}{
Optional vector containing a timestamp identifier (in any format) for each observation (see details).
}
}
\details{

This function calculates Elo scores using a sigmoidal function. Because animal groups often have relatively stable hierarchies during a study period, this implementation allows the order of interactions to be randomised (randomise=TRUE) to create K replicated datasets (where K=n.rands). This method can improve the estimate of the ranks for each individual, and allow 95\% range of values to be computed. If randomise=FALSE, the scores will be calculated only once and maintain the original ordering of winners and losers for each context.

The date option can be used to return timestamped trajectories. Note that because the function assumes that interactions are ordered temporally, the function simply returns each unique timestamp in the order they are encountered. Thus, the aggregation of observations into dates will be determined by the resolution of the timestamp (e.g. minutes, days, months).

}
\value{

The function returns different values depending on the input parameters.

If randomise=FALSE and return.trajectories=FALSE, then the function returns a vector of size N giving final score for each individual from the original ordering of interactions (where N is the number of individuals in the data or in the identities variable if given).
If randomise=TRUE and return.trajectories=FALSE, then the function returns a NxK matrix giving the final scores for each individual (rows) after each randomisation of the orders.
If randomise=FALSE and return.trajcetories=TRUE, then the function returns an Nx(T+1) matrix giving the starting score (init.score) in the first column followed by the score after each of the T interactions.
If randomise=TRUE and return.trajcetories=TRUE, then the function returns an NxTxK array.

}
\references{
Sanchez-Tojar, A., Schroeder, J., Farine, D.R. (in prep) Methods for inferring dominance hierarchies and estimating their uncertainty.
}
\author{
Written by Damien R. Farine & Alfredo Sanchez-Tojar

Maintainer: Damien R. Farine <damien.farine@ieu.uzh.ch>
}
\examples{
	
	# Generate some input data
	data <- generate_interactions(10,20,5,2)
	
	# Extract winners and losers
	winners <- data$interactions$Winner
	losers <- data$interactions$Loser
	
	# Calculate Elo scores
	scores <- elo_scores(winners,losers)
	
	# Plot ranks
	plot_ranks(scores)

	#### return timestamped data

	
	# simulating 10 dominance interactions 
	# in a group of 5 individuals
	output <- generate_interactions(5,
		10, a=10, b=5, id.biased=FALSE,
		rank.biased=FALSE)

	# adding some random dates, this could be, in principle,
	# of any format, e.g. (character, numeric, etc).
	output$interactions$date <- c("date01", "date01",
		"date02","date02","date03","date04",
		"date04","date05","date06","date06")

	dated.trajectories <- elo_scores(
		output$interactions$Winner,
		output$interactions$Loser,
		dates=output$interactions$date,
		identities=c(1:5),
		randomise=FALSE,
		return.as.ranks=FALSE,
		return.trajectories=TRUE)
}
