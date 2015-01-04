#Basic one-locus hardy weinberg function
#Input is vector of genotype numbers (AA,Aa,aa)
#Prints the results of chi-square testing
# Returns a list with observed, expected, chi-squared, and probability
hw <- function(obs){
  n <-sum(obs)
  p <- round((2*obs[1]+obs[2])/(2*n),4)
  q <-1-p
  print(paste("p=",p,"q=",q),sep=" ",digits=3)
  exp <-as.integer(round(c(p^2*n,2*p*q*n,q^2*n),0))
  print(cbind(obs,exp))
  
  chi <-sum((obs-exp)^2/exp)
  pr <-1-pchisq(chi,1)
  print(paste("chi squared =",round(chi,3),"p = ",round(pr,3), "with 1 d. f.",sep=" "))
  out <-list(obs,exp,chi,pr)
  names(out) <-c("Observed","Expected","Chisq","p")
  out
}