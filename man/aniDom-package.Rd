\name{aniDom-package}
\alias{aniDom-package}
\alias{aniDom}
\docType{package}
\title{
Inferring Dominance Hierarchies and Estimating Uncertainty
}
\description{
Provides (1) Tools to infer dominance hierarchies based on calculating Elo scores, but with custom functions to improve estimates in animals with relatively stable dominance ranks. (2) Tools to plot the shape of the dominance hierarchy and estimate the uncertainty of a given data set.
}
\details{
\tabular{ll}{
Package: \tab aniDom\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2017-04-28\cr
License: \tab GPL-2 \cr
}
}
\author{
Written by Damien R. Farine and Alfredo Sanchez-Tojar

Maintainer: Damien R. Farine <dfarine@orn.mpg.de>
}
\references{
Sanchez-Tojar, A., Schroeder, J., Farine, D.R. (in prep) Methods for inferring dominance hierarchies and estimating their uncertainty.
}
\examples{

# Generate data
data <- generate_interactions(N.inds=10,N.obs=20,a=5,b=3)

# Extract interactions
winners <- data$interactions$Winner
losers <- data$interactions$Loser

# Calculate Elo scores with randomised order
scores <- elo_scores(winners=winners,losers=losers,randomise=TRUE,n.rands=1000)

# Plot ranks
plot_ranks(scores,plot.CIs=TRUE)

# Plot hierachy shape
plot_hierarchy_shape(identity=1:nrow(scores),rank=1:nrow(scores),
	winners=winners,losers=losers,fitted=TRUE)

}
