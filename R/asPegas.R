#Function to create pegas locus object from microsatellite data
#input is from read.genalex object; parameters are object name and nfac=2 for populaion 
#(default) and 1 for region

as.pegas <-function(dat,nfac=2){
  dat.geno <-dat$genotypes # Extract Genotypes from lists
  factors <-dat.geno[,c(1,2)] #create object of regions and populations
  names(factors)=c("Region","Population")
  dat.geno <-dat.geno[,-c(1,2)] # remove factor columns for now
  ngeno <-ncol(dat.geno)-1 # set up index for pasting together genotypes
  dat.pegas <- sapply(seq(1,ngeno,2), function(x) (paste(dat.geno[,x],"/",dat.geno[,x+1],sep="")))
  dat.pegas <-data.frame(factors[,nfac],dat.pegas)
  #paste genotypes into single columns; add population column
  dat.p <-as.loci(dat.pegas,col.pop=1) #convert to locus object
  return(dat.p)
}