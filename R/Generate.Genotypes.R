## Function that, given input of allele frequencies, D, and N, will return a genotype matrix for input into hillD
genotype.generate <-function(p,r,D,N=1000){
  q <-1-p
  s <-1-r
  gams <-rep(0,4)
  gams[1] <-p*r+D
  gams[2] <-p*s-D
  gams[3] <-q*r-D
  gams[4] <-q*s+D
  genos <-matrix(nrow=3,ncol=3)
  rownames(genos) <-c("AA","Aa","aa")
  colnames(genos) <-c("BB","Bb","bb")
  genos[1,1] <-gams[1]^2
  genos[1,2] <-2*gams[1]*gams[2]
  genos[1,3] <-gams[2]^2
  genos[2,1] <-2*gams[1]*gams[3]
  genos[2,2] <-2*gams[1]*gams[4]+2*gams[2]*gams[3]
  genos[2,3] <-2*gams[2]*gams[4]
  genos[3,1] <-gams[3]^2
  genos[3,2] <- 2*gams[4]*gams[3]
  genos[3,3] <-gams[4]^2
  genos <-round(N*genos)
  return(genos)
}