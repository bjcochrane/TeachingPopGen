#Function to read genalex diploid data, saved as a .csv file.  Returns a list with number of loci, number of samples, 
#number of regions, samples per region, number of populations, samples per population, and a dataframe consisting of 
#populations and regions (factors) along with genotypes with one alllele per column.

#Still need to parse row two to get title and population/region name information.
read.genalex <-function(filename, missing=0){
  param <-read.csv(filename,nrows=1,header=FALSE) # read the first line to get parameters
  param <- param[!is.na(param)] # remove NA's
  nloc <-param[1] # Get parameters into usable variables
  nsamp <-param[2]
  npop <-param[3]
  nreg=NULL
  regsize=NULL
  popsize <-param[4:(4+npop-1)] # extract population numbers
  Population <-lapply(1:npop, function(x) rep(x,popsize[x])) # create vector of factors
  levels.dat <-factor(unlist(Population))
  if(param[4+npop]!=1){ # if loop to see if regions are specified
    nreg <-param[(npop+4)]
    regsize <-param[(npop+5):(5+npop+nreg-1)]
    regfac <-lapply(1:nreg,function(x) rep(x,regsize[x]))
    regfac <- factor(unlist(regfac))
    
    levels.dat <-data.frame(regfac,levels.dat)
    colnames(levels.dat) <-c("Region","Population")
  }
  dat <-read.csv(filename,skip=2) # now read genotype data
  dat <-dat[-c(1:2)] # Remove id and population designator
  dat[dat==missing]=NA
  dat <-data.frame(levels.dat,dat[1:(nloc*2)]) # Add the region and population columns
  if(colnames(dat)[1]!="Region") colnames(dat)[1]="Population" #Fix population column name if no regions
  dat <-list(nloc,nsamp,nreg,regsize,npop,popsize,dat)
  names(dat) <-c("nloci","nsample","nregion","reg.size","npops","popsizes","genotypes")
return(dat) # return the dataset to the calling routine
}