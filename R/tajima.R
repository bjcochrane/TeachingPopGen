#Function to calculate s, pi and D from ms phyclust
#Input is raw output from ms phyclust; output will be a table of S, pi and D
# H to be added at some point.  In other words, this will be ss

ms.summary <-function(dat){
  dat.ms <-read.ms.output(txt=dat)
  seqs <-dat.ms$gametes
  out <-sapply(seqs,function(x) {
    s <-ncol(x)
    n <-nrow(x)
    pi <-0
    for (i in 1:(n-1)){
      for(j in (i+1):n){
        pi <-pi+sum(abs(x[i,]-x[j,]))
      }
    }
    denom <-(n*(n-1))/2
    pi <-pi/denom
    ivec <-c(1:(n-1)) # create the vector of 1 to n-1
    a1 <-sum(1/ivec)
    a2 <-sum(1/(ivec^2))
    b1 <-(n+1)/(3*(n-1))
    b2 <-(2*(n^2+n+3))/(9*n*(n-1))
    c1=b1-1/a1
    c2 <-b2-(n+2)/(a1*n)+a2/(a1^2)
    e1 <-c1/a1
    e2 <-c2/(a1^2+a2)
    dif <-pi-s/a1
    var <-sqrt(e1*s+e2*s*(s-1))
    d <-dif/var
    c(s,pi,d)
  }
  )
res <-data.frame(t(out))
names(res) <-c("S","pi","D")
return(res)
}
    