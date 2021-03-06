\name{generate_interactions}
\alias{generate_interactions}
\title{
Generate simulated interactions with differently-shaped hierarchies
}
\description{
Generates simulated winners and losers. The function can generate data for different population sizes, with differently-shaped hierarchies, and with varying biases. The output is the hierarchy, and the winner and loser for each interaction.
}
\usage{
generate_interactions(N.inds, N.obs, a, b, id.biased = FALSE, rank.biased = FALSE)
}
\arguments{
  \item{N.inds}{
The number of individuals}
  \item{N.obs}{
The number of observed interactions (in total).
}
  \item{a}{
Parameter to control the steepness of the hierarchy (flatter or more sigmoidal) \code{\link{plot_winner_prob}}.
}
  \item{b}{
Parameter to control the intercept of the hierachy (moves the sigmoid left or right) \code{\link{plot_winner_prob}}.
}
  \item{id.biased}{
Boolean (TRUE/FALSE) describing whether to introduce an individual bias in the observations (some individuals interact more often than others).
}
  \item{rank.biased}{
Boolean (TRUE/FALSE) describing whether to introduce a rank difference bias in the observations (closely-ranked individuals interact more often).
}
}
\details{
This function is useful for generating input data with a known hierarchy. The shape of the hierarchy can be controlled using two parameters, though is by default \code{a} sigmoidal shape. Higher values of a typically create a greater probability of a dominant winning (turn the function into more of a threshold). Higher values of b typically decrease the probability of a dominant winning when ranks are very similar. The \code{\link{plot_winner_prob}} function allows visualisation of the hierarchy function (see examples below).
}
\value{
Returns a list with two elements:
hierarchy: A dataframe containing three columns, the ID of the individual, its Rank, and its Probability of interacting (varies if id.biased=TRUE).
interactions: A dataframe containing two columns, the Winner and the Loser for each interaction. Each row represents one interaction.
}
\references{
Sanchez-Tojar, A., Schroeder, J., Farine, D.R. (in prep) Methods for inferring dominance hierarchies and estimating their uncertainty.
}
\author{
Written by Damien R. Farine & Alfredo Sanchez-Tojar

Maintainer: Damien R. Farine <damien.farine@ieu.uzh.ch>
}
\examples{
	
	par(mfrow=c(2,2))
	
	# Set population size 
	N <- 20
	
	# Set shape parameters
	a = 15
	b = 3
	
	# See what this looks like
	plot_winner_prob(1:N,a,b)
	
	# Generate some input data
	data <- generate_interactions(N,400,a,b)
	
	# See what the hierarchy looks like from the output data
	winners <- data$interactions$Winner
	losers <- data$interactions$Loser
	identities <- data$hierarchy$ID
	ranks <- data$hierarchy$Rank
	plot_hierarchy_shape(identities,ranks,winners,losers,fitted=TRUE)
	
	# Set new shape parameters
	a = 3
	b = 3
	
	# See what this looks like
	plot_winner_prob(1:N,a,b)
	
	# Generate some input data
	data <- generate_interactions(N,400,a,b)
	
	# See what the hierarchy looks like from the output data
	winners <- data$interactions$Winner
	losers <- data$interactions$Loser
	identities <- data$hierarchy$ID
	ranks <- data$hierarchy$Rank
	plot_hierarchy_shape(identities,ranks,winners,losers,fitted=TRUE)
	
}
