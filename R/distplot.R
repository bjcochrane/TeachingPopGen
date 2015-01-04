##Function to calculate distribution of haplotypes from hapmap data
#Input is as a csv from hapmap with rows=haplotypes and columns=SNPs
dist.plot <-function(dat,t="Population"){
  unique.hap <-unique(dat)
  unique.hap <-unique.hap[-nrow(unique.hap),]
  dat.sum <-aggregate (rep (1, nrow (dat)), by = as.list (dat), FUN = sum)
  dat.sum <-dat.sum[-(nrow(dat.sum)),]
  barplot(dat.sum$x,names.arg=c(1:nrow(dat.sum)),main=t) #plot histogram
  print(length(dat.sum$x)) #return number of alleles
  print(sum(dat.sum$x))   #return number of subjects   
}