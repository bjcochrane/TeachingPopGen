## Function to split VCF data

split.vcf <-function(dat.vcf,pops){
  data(poplist)
  test <-split(poplist.1000g,poplist.1000g$Population.code)
  test2 <-test[which(names(test) %in% pops)]
  nms2 <-sapply(test2, function (x) x$Sample.name)
  dat.spl <-lapply(nms2, function(x) dat.vcf[x,])
  dat.spl
}