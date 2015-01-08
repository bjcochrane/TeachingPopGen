## Some functions to interpret ms output from phyclust
## All based on single data set; ms.summary to be rewritten to analyze 
## multiple sets.

## Input is one raw run to function sfs; it will then be called from pi etc.

sfs <-function(dat,pl=TRUE){
 dat.ms <-read.ms.output(txt=dat)
 gams <-dat.ms$gametes[[1]]
  dist <-apply(gams,2,sum)
  n <-nrow(gams)
  h <-hist(dist, breaks=c(0:(n-1)),plot=FALSE)
  if(pl) plot(h,,main="Unfolded SFS",xlab="i")
  sf <-h$counts
  return(list(n,sf))
}
## Now a function for pi; use sfs as input
pi <-function(dat){
  sf <-dat[[2]]
  n <-dat[[1]]
  pi2 <-2*sf*c(1:(n-1))*c(n-c(1:(n-1)))
  pi2 <-sum(pi2/(n*(n-1)))
  return(pi2)
}
# now a nice simple one for the Watterson estimator
theta.W <-function(dat){
  dat.ms <-dat[[2]]
  s <-sum(dat.ms)
  n <-dat[[1]]
  ivec <-c(1:(n-1)) # create the vector of 1 to n-1
  a <-sum(1/ivec)
  th <-s/a
  th
}
# Then, with all of the above, we can write a tajima test function

taj<-function(dat){
  dif <-pi(dat)-theta.W(dat)
  n <-dat[[1]]
  s <-sum(dat[[2]])
  ivec <-c(1:(n-1)) # create the vector of 1 to n-1
  a1 <-sum(1/ivec)
  a2 <-sum(1/(ivec^2))
  b1 <-(n+1)/(3*(n-1))
  b2 <-(2*(n^2+n+3))/(9*n*(n-1))
  c1=b1-1/a1
  c2 <-b2-(n+2)/(a1*n)+a2/(a1^2)
  e1 <-c1/a1
  e2 <-c2/(a1^2+a2)
  var <-sqrt(e1*s+e2*s*(s-1))
  d <-dif/var
  d
}
# And finally, Fay and Wu's H test

theta.H <-function(sfs){
  n <-sfs[[1]]
  sfs <-sfs[[2]]
  th <-0
  for (i in 1:(n-1)){
    th <-th+(2*sfs[i]*i^2)/(n*(n-1))
  }
  return(th)
}