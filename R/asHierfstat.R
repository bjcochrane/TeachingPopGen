#Function to convert genalex object to one for hierfstat
#Input is list generated by read.genalex
as.hierfstat <-function(dat,missing=0){
  dat.gen <-dat$genotypes
  fac.col <-ifelse(is.null(dat$nregion),1,2) # determine whether or not there is a region column
  pop.levels <-dat$genotypes[,1:fac.col] #select columns with subdivision level
  pop.loci <-dat.gen[,-c(1:fac.col)] #select columns with allelic data
  n <-ncol(pop.loci)-1
  dat.loci <-sapply(seq(1,n,2), function(x) (paste(pop.loci[,x],pop.loci[,x+1],sep=""))) #merge genotypes into single columns
  dat.loci <-data.frame(apply(dat.loci,2, as.numeric)) #convert to numeric
  dat.loci[dat.loci==missing] <-NA
  out <-list(pop.levels,dat.loci)
  names(out)=c("PopulationLevels","GenotypeData")
  return(out)
}