# Function to calculate two locus linkage disequilibrium from a 3X3 array of genotype numbers
# From Hill, 1974.  Heredity 33: 229-239.
HillD <-function(gmat){
  X11 <- 2*gmat[1,1]+gmat[1,2]+gmat[2,1]
  X12 <-2*gmat[1,3]+gmat[1,2]+gmat[2,3]
  X21 <- 2*gmat[3,1]+gmat[2,1]+gmat[3,2] #NS
  X22 <-2*gmat[3,3]+gmat[2,3]+gmat[3,2]
  N <- sum(gmat)
  phat <- (X11+X12+gmat[2,2])/(2*N)
  qhat <-(sum(gmat[,1])+(sum(gmat[,2])/2))/N
  f11 <- rep(0,20)
  f11[1] <- (X11-X12-X21+X22)/(4*N)+.5-(1-phat)*(1-qhat)
  for (i in 1:19){
    finit <-f11[i] # convenience variable
    t1 <- 1-phat-qhat+finit  
    num <-gmat[2,2]*finit*t1
    denom <-(finit*t1+(phat-finit)*(qhat-finit))
    fnew <-X11+(num/denom)
    fnew <-fnew/(2*N)
    f11[i+1] <-fnew
  }
  D <- f11-phat*qhat
  plot(D,type="b",xlab="Iteration",ylab="D")
  return(D)
}