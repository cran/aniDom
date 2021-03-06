\name{estimate_uncertainty_by_splitting}
\alias{estimate_uncertainty_by_splitting}
\title{
Calculate the repeatability score of ranks by splitting the observed data in half.
}
\description{
Calculates the correlation of the Elo scores for individuals across to exclusive halves of the data.
}
\usage{
estimate_uncertainty_by_splitting(winners, losers, identities = NULL, 
	sigmoid.param = 1/100, K = 200, init.score = 0, randomise = FALSE, n.rands = 1000)
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
The number of randomisations to perform.
}
}
\details{
By calculating the correlation of the Elo scores calculated separately for two halves of the data, this function provides some insights into the uncertainty and robustness of the data collected. If randomise=FALSE, the data are split in half and the correlation between the two halves is returned. If randomise=TRUE, then the ordering of the interactions is randomised n.rands times and the correlation is calculated each time in the same way. The function then returns the mean and 95\% range of the correlation values. Our simulations suggest that correlations above 0.5 suggests a robust dominance hierarchy (or low uncertainty).
}
\value{
If randomise=FALSE: the Spearman rank correlation coefficient of the first half and second half of the data.
If randomise=TRUE: the mean and 95\% range of the Spearkman rank correlation coefficients from two halves of the data with the ordering randomised each time.
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
	
	# Calculate repeatability
	r <- estimate_uncertainty_by_splitting(winners,losers,randomise=TRUE)
	
}