driftPlot <-function(N, nreps=10){
  ngen=length(N) # Set length to run
  plot(c(0,ngen),c(0,1),type="n", xlab="Generation", ylab="p") # create a blank plot
  sapply(c(1:nreps),function (x) {
    pi <-rep(.5,ngen)
    gen <-c(1:ngen)
    for (i in 1:(ngen-1)){
      pi[i+1] <-rbinom(1,2*N[i],pi[i])/(2*N[i])
    }
    lines(gen,pi)
    pi
  }
  )
}