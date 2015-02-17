fixtime <-function(pi,ngen=1000,nreps=1000,N=100){
  n <-rep(N,ngen)
  drft <-driftPlot(n,nreps,p=pi,print=FALSE)
  drft.fix <-drft[,which(drft[length(n),]==1)]
  fix.time <-apply(drft.fix,2,function (x) length(which(x!=1))+1)
  list(mean.time=mean(fix.time),N.fixed=length(fix.time),fix.times=fix.time)
}