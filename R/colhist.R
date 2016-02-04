
colhist =function(dat,breaks=20,tail=2,xlab="X",ylab="Frequency",main="Main",xr=c(0,0)){
  flhist <-hist(dat,breaks=20,plot=FALSE)
  
  
  if(identical(xr,c(0,0))) xr=(range(flhist$breaks))
  if(tail==2){
    q <-quantile(dat,c(.025,.975),na.rm=TRUE)    
    hist(dat,nclass=length(flhist$mids),col=ifelse(flhist$mids<q[1],"red",ifelse(flhist$mids>q[2],"red","white")),xlab=xlab,ylab=ylab,main=main,xlim=xr)
  }
  if(tail==1){
    q <-quantile(dat,.95,na.rm=TRUE)
    hist(dat,nclass=length(flhist$mids),col=ifelse(flhist$mids>q,"red","white"),xlab=xlab,ylab=ylab,main=main,xlim=xr)
  }
  #  print(q)
}
#Basic one-locus hardy weinberg function
#Input is vector of genotype numbers (AA,Aa,aa)
#Prints the results of chi-square testing
# Returns a list with observed, expected, chi-squared, and probability
hw <- function(obs,print=TRUE){
  n <-sum(obs)
  p <- (2*obs[1]+obs[2])/(2*n)
  q <-1-p
  exp <-(c(p^2*n,2*p*q*n,q^2*n))
  chi <-sum((obs-exp)^2/exp)
  pr <-1-pchisq(chi,1)
  f <-1-obs[2]/exp[2]
  if(print){
    print(paste("p=",p,"q=",q,sep=" "),digits=3,quote=FALSE)
    print(cbind(obs,exp=round(exp,0)))
    print(paste("chi squared =",round(chi,3),"p = ",round(pr,3), "with 1 d. f.",sep=" "),quote=FALSE)
    print(paste("F = ",round(f,4),sep=" "),quote=FALSE)
  }
  out <-list(obs,exp,c(p,q),chi,pr,f)
  names(out) <-c("Observed","Expected","Allele_freqs","Chisq","prob","F")
  out
}