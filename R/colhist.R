library(XML)
library(stringr)
colhist =function(dat,breaks=20,tail=2,labs =c("X","Number","Title"),xr=c(0,0)){
  
  flhist <-hist(dat,breaks=20,plot=FALSE)
  
  
  if(identical(xr,c(0,0))) xr=(range(flhist$breaks))
  if(tail==2){
    q <-quantile(dat,c(.025,.975),na.rm=TRUE)    
    hist(dat,nclass=length(flhist$mids),col=ifelse(flhist$mids<q[1],"red",ifelse(flhist$mids>q[2],"red","white")),xlab=labs[1],ylab=labs[2],main=labs[3],xlim=xr)
  }
  if(tail==1){
    q <-quantile(dat,.95,na.rm=TRUE)
    hist(dat,nclass=length(flhist$mids),col=ifelse(flhist$mids>q,"red","white"),xlab=labs[1],ylab=labs[2],main=labs[3],xlim=xr)
  }
  #  print(q)
}