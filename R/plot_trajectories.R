plot_trajectories <-
function(trajectories,colors=NULL) {
	
	if (is.null(colors)) {
		colors <- rep("black",nrow(trajectories))
	}

	plot(NULL, xlim=c(1,ncol(trajectories)), ylim=range(trajectories,na.rm=T),xlab="interaction number",ylab="score or rank")
	for (i in 1:nrow(trajectories)) {
  		lines(1:ncol(trajectories),trajectories[i,],col=colors[i])
	}
}
