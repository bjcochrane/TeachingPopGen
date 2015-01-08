#Basic one-locus hardy weinberg function
#Input is vector of genotype numbers (AA,Aa,aa)
#Prints the results of chi-square testing
# Returns a list with observed, expected, chi-squared, and probability
hw <- function(obs,print=TRUE){
  n <-sum(obs)
  p <- round((2*obs[1]+obs[2])/(2*n),4)
  q <-1-p
  exp <-as.integer(round(c(p^2*n,2*p*q*n,q^2*n),0))
  chi <-sum((obs-exp)^2/exp)
  pr <-1-pchisq(chi,1)
  if(print){
  print(paste("p=",p,"q=",q,sep=" "),digits=3,quote=FALSE)
  print(cbind(obs,exp))
  print(paste("chi squared =",round(chi,3),"p = ",round(pr,3), "with 1 d. f.",sep=" "),quote=FALSE)
  }
  out <-list(obs,exp,c(p,q),chi,pr)
  names(out) <-c("Observed","Expected","Allele_freqs","Chisq","prob")
  out
}