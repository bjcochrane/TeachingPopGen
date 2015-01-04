#Function to read downloaded hapmap file and convert it to a genotype object for use in LD Heatmap analysis
#First input is name of data file (with path if necessary); value is the genotype object.

read.hapmap.genotype <- function (file){
  library(genetics)
  dat.hap <- read.table(file,skip=2,header=TRUE,comment.char="",row.names=1) # read file, rows are subjects, columns are genotypes
  dat.pos <-dat.hap$pos #save chromosomoal positions for future use.
  dat.hap <-dat.hap[,-c(1:10)] # remove extraneous columns
  dat.hap <-t(dat.hap)#transpose
  # now apply the genetics function to the columns and make into a data frame.
  dat.genotype <-makeGenotypes(data.frame(dat.hap),sep="")
  #dat.genotype <-data.frame(dat.genotype)
  #colnames(dat.genotype) <-colnames(dat.hap)
  dat.out <-list(dat.genotype,dat.pos)
  names(dat.out) <-c("Genotypes","Chromosomal Position")
  dat.out
}
