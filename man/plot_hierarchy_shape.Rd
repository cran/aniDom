\name{plot_hierarchy_shape}
\alias{plot_hierarchy_shape}
\title{
Plots the shape of a dominance hierarchy from empirical data
}
\description{
This function takes a set of winners and losers from observed interactions and plots the probability of the dominant individual in an interaction winning given the difference in rank to the subordinate in the same interaction.
}
\usage{
plot_hierarchy_shape(identity, rank, winners, losers, fitted = FALSE)
}
\arguments{
  \item{identity}{
A vector containing the identities of all individuals in the data.
}
  \item{rank}{
A vector giving the ranks for each individual (in the same order as the identities).
}
  \item{winners}{
A vector giving the identity of the winner for each interaction.
}
  \item{losers}{
A vector giving the identity of the loser for each interaction in the same order as the winners.
}
  \item{fitted}{
A Boolean (TRUE/FALSE) describing whether to add a fitted line to the plot
}
}
\details{
This function is useful for examining how the probability of winning is shaped by the difference in rank. The shape of this graph provides information about the shape of the dominance hierarchy.
}
\value{
This function will return the data for x (difference in rank) and y (probability of dominant winning) coordinates of the plot as a data frame.
}
\references{
Sanchez-Tojar, A., Schroeder, J., Farine, D.R. (in prep) Methods for inferring dominance hierarchies and estimating their uncertainty.
}
\author{
Written by Damien R. Farine & Alfredo Sanchez-Tojar

Maintainer: Damien R. Farine <damien.farine@ieu.uzh.ch>
}
\examples{

	par(mfrow=c(1,2))

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
	shape <- plot_hierarchy_shape(identities,ranks,winners,losers,fitted=TRUE)

	# Data is contained in shape
	shape

}