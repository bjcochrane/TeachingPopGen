# Removes monomorphic loci from VCF read from pegas.  Input is
# VCF

monogon <-function(vfile){
  vfile[, sapply(vfile, nlevels) > 1]
}

# Screen by MAF.  Default is MAF=0.5

setmaf <-function(vfile, maf=.05){
  vfile.sum <-summary(vfile)
  
  frqs <-sapply(vfile.sum, function(x) min(x$allele)/sum(x$allele)) #Calculate minor allele frequencies
  
  vfile[,which(frqs>=maf)] 
}

# Remove SNP labels for display purposes

lblstrip <-function(vfile){ attr(vfile,"Labels") <-NULL
vfile}
  
