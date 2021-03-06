\name{plot_ranks}
\alias{plot_ranks}
\title{
Plot the ranking of individuals
}
\description{
Function to plot the ranking of individuals in different ways.
}
\usage{
plot_ranks(ranks, plot.CIs = FALSE, ordered.by.rank = TRUE, 
	identities = NULL, plot.identities = TRUE, colors = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ranks}{
Either a vector containing the score or rank of each individual, or an NxK matrix containing the results of K randomisations of the data.
}
  \item{plot.CIs}{
Boolean (TRUE/FALSE): if providing an NxK matrix, then setting plot.CIs to TRUE will plot the 95\% range of the scores or ranks given for each individual.
}
  \item{ordered.by.rank}{
Boolean (TRUE/FALSE) describing whether to order individuals by rank or not.
}
  \item{identities}{
A vector containing the identity (name) of each individual to be plotted along the X axis.
}
  \item{plot.identities}{
Boolean (TRUE/FALSE) describing whether to plot the identity of each individual along the X axis.
}
  \item{colors}{
A vector containing the colour for each individual (default="black"). This is useful for example to colour individuals by sex.
}
}
\details{
A simple function that plots individuals' ranks, with options to colour individuals or order them. Here the y axis is reverse, so that rank=1 occurs at the top.
}
\value{
Generates a plot. No data is returned.
}
\references{
Sanchez-Tojar, A., Schroeder, J., Farine, D.R. (in prep) Methods for inferring dominance hierarchies and estimating their uncertainty.
}
\author{
Written by Damien R. Farine & Alfredo Sanchez-Tojar

Maintainer: Damien R. Farine <damien.farine@ieu.uzh.ch>
}
\examples{
	
	# Set population size 
	N <- 10

	# Set shape parameters
	a = 15
	b = 3

	# Generate data
	data <- generate_interactions(N,100,a,b)

	# Extract data (and turn IDs into letters for this example)
	winners <- letters[data$interactions$Winner]
	losers <- letters[data$interactions$Loser]
	identities <- letters[data$hierarchy$ID]
	
	# Calculate Elo scores
	scores <- elo_scores(winners,losers,identities=identities,randomise=TRUE)
	
	# Plot results
	plot_ranks(scores, plot.CIs=TRUE,identities=TRUE,colors=rainbow(N))
	
}