## Function to extract genotype numbers only from raw HapMap files
## min sets the limit for eliminating snps with fewer than n represenatatives in any genotype class.
datprep <-function(dat,min=5){
  genos <-cbind(dat$V13,dat$V16,dat$V19)
  rownames(genos) <-rownames(dat)
  colnames(genos) <-c("AA","Aa","aa")
  
  genos.sub <-genos[genos[,1]>min&genos[,2]>min&genos[,3]>min,]
  genos.sub
}