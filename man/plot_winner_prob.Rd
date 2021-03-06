\name{plot_winner_prob}
\alias{plot_winner_prob}
\title{
Plot the shape of the hierarchy given some input parameters
}
\description{
A simple function that provides visualisations of the shape of the hierarchy given parameters a and b in the \code{\link{generate_interactions}} function
}
\usage{
plot_winner_prob(diff.rank, a, b)
}
\arguments{
  \item{diff.rank}{
A vector containing the x values of the plot (i.e. differences in rank).
}
  \item{a}{
Parameter a (see \code{\link{generate_interactions}}).
}
  \item{b}{
Parameter b (see \code{\link{generate_interactions}}).
}
}
\details{
A simple plotting function to visualise the shapes of curves in the \code{\link{generate_interactions}} function.
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

	# Plot the shape
	plot_winner_prob(1:10,a,b)
	
}