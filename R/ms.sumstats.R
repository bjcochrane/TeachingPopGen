#Function to compute summary statistics from phyclust:ms output
#requires use of read.ms.output to process raw ms file
# input to function is then ms.out$gametes
# Demonstrates that the entire analysis derives from the site frequency spectrum
ms.sumstats <-function(ms.raw){
  dat <-read.ms.output(ms.raw)
  gametes <-dat$gametes
  sf <-lapply(gametes,sfs,pl=FALSE) # Get the site frequency spectra
  S <-sapply(sf,function (x) sum(x[[2]]))
  pi <-sapply(sf,pi)
  th.W <-sapply(sf,theta.W)
  th.H <-sapply(sf,theta.H)
  Tajima.D <- sapply(sf,taj)
  H <-pi-th.H
  out <-data.frame(S,pi,th.W,Tajima.D,th.H,H)
  return(out)
}