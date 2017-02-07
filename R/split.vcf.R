## Function to split VCF data

split.vcf <-function(dat.vcf,pops){
  data(poplist,package="TeachingPopGen")
  test <-split(poplist.1000g,poplist.1000g$Population.code)
  test2 <-test[which(names(test) %in% pops)]
  nms2 <-lapply(test2, function (x) as.character(x$Sample.name))
  dat.spl <-lapply(nms2, function(x) dat.vcf[x,])
  dat.spl
}