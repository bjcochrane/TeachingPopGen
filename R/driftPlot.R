driftPlot <-function(N, nreps=10,p=0.5,print=TRUE){
  ngen=length(N) # Set length to run
  if(print) plot(c(0,ngen),c(0,1),type="n", xlab="Generation", ylab="p") # create a blank plot
  sapply(c(1:nreps),function (x) {
    pi <-rep(p,ngen)
    gen <-c(1:ngen)
    for (i in 1:(ngen-1)){
      pi[i+1] <-rbinom(1,2*N[i],pi[i])/(2*N[i])
    }
    if(print) lines(gen,pi)
    pi
  }
  )
}