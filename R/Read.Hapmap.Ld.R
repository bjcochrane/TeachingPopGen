#Function that reads downloaded LD table from hapmap, converts r^2 to a square matrix and returns a list with
# the matrix, the chromosome map, and the names of the SNPS

read.hapmap.ld <-function(file){
  dat.ld <-read.table(file,skip=1,header=TRUE, comment.char="") #read the file
  n <-length(unique(dat.ld$marker1)) #determine the number of snps
  x <-matrix(0,n,n) #create an empty nXn matrix
  count=1 #loop to populate the matrix
  for(i in 1:n-1){
    for(j in (i+1):n){
      x[j,i] <-dat.ld$r.2[count]
      x[i,j] <-x[j,i]
      count <-count+1
    }
  }
  map <- unique(dat.ld$X.pos1) #create the map item
  nms <-as.character(unique(dat.ld$marker1)) #and the snp names
  rownames(x) <-nms #Label the rows and columns
  colnames(x) <-nms
  out <-list(x,map,nms)
  names(out) <-c("rMatrix","map","SNP_names")
  return(out)
}