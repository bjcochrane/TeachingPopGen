##Function to return Ewens plot for hapmap data
##Input is csv file of hapmap haplotypes

Ewens.hap <-function(dat){
  unique.hap <-unique(dat)
  unique.hap <-unique.hap[-nrow(unique.hap),]
  dat.sum <-aggregate (rep (1, nrow (dat)), by = as.list (dat), FUN = sum)
  dat.sum <-dat.sum[-(nrow(dat.sum)),]
  fh <-fhat(dat.sum$x)
  print(fh)
  Ewens(sum(dat.sum$x),length(dat.sum$x),fh)
}