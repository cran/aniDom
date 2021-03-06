\name{plot_trajectories}
\alias{plot_trajectories}
\title{
Plot the Elo trajectories of individuals
}
\description{
Plot the trajectories of Elo scores after each interaction
}
\usage{
plot_trajectories(trajectories, colors = NULL)
}
\arguments{
  \item{trajectories}{
The output of \code{\link{elo_scores}} with the return.trajectories option set to TRUE: an Nx(T+1) matrix (where T is the number of interactions).
}
  \item{colors}{
An optional vector of colours for each line (default = "black").
}
}
\details{
Plots one set of trajectories. If the randomise option in \code{\link{elo_scores}} is set to TRUE, then the resulting matrices should be passed one at a time.
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
	scores <- elo_scores(winners,losers,identities=identities,
		randomise=FALSE,return.trajectories=TRUE)
	
	# Plot results
	plot_trajectories(scores, colors=rainbow(N))
	
}