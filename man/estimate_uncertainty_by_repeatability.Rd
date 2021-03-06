\name{estimate_uncertainty_by_repeatability}
\alias{estimate_uncertainty_by_repeatability}
\title{
Calculate the repeatability score of ranks from randomisations of the interaction orders.
}
\description{
Calculates the Intraclass Correlation Coefficient for each individual after randomising the order of interactions in the given dataset.
}
\usage{
estimate_uncertainty_by_repeatability(winners, losers, identities = NULL, 
	sigmoid.param = 1/100, K = 200, init.score = 0, n.rands = 1000)
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
  \item{n.rands}{
The number of randomisations to perform.
}
}
\details{
Each ordering of winners and losers will yield slightly different Elo scores. This function takes the Elo scores from n.rands randomisations of the order of interactions. It then computes the repeatability score. This repeatability score can provide some insight into the level of certainty (or robustness) of the input data. Our simulations suggest that a repeatability score above 0.8 suggests a reasonably robust hierarchy, given a large input dataset (can be unreliable for small datasets, i.e. < 10 observations per indiviudal), or for extremely flat hierarchies.
}
\value{
Returns an object of class rpt from the rptR library. This object contains all of the information required to estimate repeatability.
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
	r <- estimate_uncertainty_by_repeatability(winners,losers)
	
}