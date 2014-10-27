Ewens <-function(n,k,fhat,nsim=1000){
#dev.off()
 # plot.new()
  x <-matrix(nrow=nsim,ncol=k)
x[,]=0
f <-rep(0,nsim)
for(rep in 1:nsim){
#  print(rep)
  if(rep==1){
    B <-matrix(nrow=50,ncol=2000)
B[,] <-0
for (J in 1:n){
  B[1,J] <-1/J}
for(I in 2:k){
  B[I,I] <- 1
  NM <-n-1
  for (J in I:NM){
    B[I,J+1] <-(I*B[I-1,J]+J*B[I,J])/(J+1)
  }
}}
#print(B[1:10,1:10])
km <-k-1
  a <-runif(km,0,1)
  nleft=n
  for(l in 1:km){
    cum=0
    for(i in 1:nleft){
      cum <-cum+B[k-l,nleft-i]/(B[k-l+1,nleft]*i)
      if(cum>=a[l]) break
    }
   # print(i)
  #  print(l)
    x[rep,l]=i
  
  nleft <-nleft-x[rep,l]
  }
  x[rep,k]=nleft
f[rep] <-sum((x[rep,]/n)^2)

}
print(mean(f))
q <-quantile(f,c(.025,.975))
print(q)
fhist <-hist(f,breaks=20,plot=FALSE)
#print(fhist$mids)
hist(f,nclass=length(fhist$mids),xlab="F",xlim =c(0,1),main=paste("N =",n,"k =",k,sep=" "),col=ifelse(fhist$mids<q[1],"red",ifelse(fhist$mids>q[2],"red","white")))

abline(v=fhat,col="red")
 #return(f)
}
fhat <-function(dist){
  sum((dist/sum(dist))^2)
}
maxp <-function(n){
  p <-seq(.01,.99,.01)
  C<-factorial(sum(n))/(factorial(n[1])*factorial(n[2])*factorial(n[3]))
  L <-log(C)+((2*n[1]+n[2])*log(p))+(n[2]+2*n[3])*(log((1-p)))

phat <-(2*n[1]+n[2])/(2*sum(n))
  plot(p,L,type="l")
abline(v=phat,col="red")
abline(h=max(L),col="blue")
pmax <-p[which.max(L)]
c(phat,pmax)
  }
p.em <-function(n) #n is vector of numbers of A, B, AB, and O
  {
  p <-matrix(nrow=100,ncol=3)
p[1,]=rep(.33,3) # use a flat distribution to initiate
n2 <-2*sum(n)
for (i in 2:100){
  k <-i-1
  p[i,1] <-(((p[k,1]+p[k,3])/(p[k,1]+2*p[k,3]))*2*n[1]+n[3])/n2
  p[i,2] <-(((p[k,2]+p[k,3])/(p[k,2]+2*p[k,3]))*2*n[2]+n[3])/n2
  p[i,3] <-(((p[k,3]/(p[k,1]+2*p[k,3]))*n[1]+(p[k,3]/(p[k,2]+2*p[k,3]))*n[2])+n[4])/sum(n)
 }
  nms <-c("A,","B","0")
par(mfrow=c(1,3))
for(i in 1:3){
  plot(c(1:10),p[1:10,i],main=nms[i],xlab="Iteration",ylab="p",ylim=c(0,1))
  }
  p[10,1:3]
  }
chixw <-function(obs,exp,df=1){
chi <-sum((obs-exp)^2/exp)
pr <-1-pchisq(chi,df)
chi <-round(chi,2)
pr <-round(pr,2)
out <-cat(paste("chi-square = ",chi), paste("probability(<.05) = ",pr), paste("deg. freedom = ",df),sep="\n")
out
}

# The following uses input from read.HapMap.data in chopsticks and returns a file formatted as saving for csv input into Genalex
hap2gen <- function(x){
d2 <-as.data.frame(x$snp.data)
#d2 <-t(d2)
d2 <-as.matrix(d2)
d2 <-replace(d2,d2=="03",11)
d2 <-replace(d2,d2=="02",12)
d2 <-replace(d2,d2=="01",22)
d2
}
read_ss <-function(file){
  dat <-read.table(file)
  dat <- cbind(dat[,c(2,4,6,8,10)])
  colnames(dat) <- c("pi", "S", "D","thetaH","H")
  dat
}
read.ms.output <- function( txt=NA, file.ms.output=NA ) {
    
    if( !is.na(file.ms.output) ) txt <- scan(file=file.ms.output,
       what=character(0), sep="\n", quiet=TRUE)
    if( is.na(txt[1]) ){
    	print("Usage: read.ms.output(txt), or read.ms.output(file=filename)")
    	return()
    	}
    nsam <- as.integer( strsplit(txt[1], split=" ")[[1]][2] )
    ndraws <- as.integer( strsplit( txt[1], split=" ")[[1]][3] )

    h <- numeric()
    result <- list()
    gamlist <- list()
    positions <- list()

    marker <- grep("prob",txt)
    probs <- sapply(strsplit(txt[marker], split=":"), function(vec) as.numeric(vec[2]))
    marker <- grep("time",txt)
    times <- sapply(strsplit(txt[marker], split="\t"), function(vec){ as.numeric(vec[2:3])} )

    
    ## THE OUTPUT TEXT FOR EACH DRAW SHOULD CONTAIN THE WORD "segsites"
    marker <- grep("segsites", txt)
    stopifnot(length(marker) == ndraws)

    ## GET NUMBERS OF SEGREGATING SITES IN EACH DRAW
    segsites <- sapply(strsplit(txt[marker], split=" "), function(vec) as.integer(vec[2]) )
    for(draw in seq(along=marker)) {
        if(!(draw %% 100)) cat(draw, " ")
        if(segsites[draw] > 0) {
        	  tpos <- strsplit(txt[marker[draw]+1], split=" ")
        	  positions[[draw]] <- as.numeric( tpos[[1]][ 2:(segsites[draw]+1) ] ) 
            haplotypes <- txt[(marker[draw] + 2):(marker[draw] + 2 + nsam - 1)]
            haplotypes <- strsplit(haplotypes, split="")
            h <- sapply(haplotypes, function(el) c(as.integer(el)))
            ## IF THERE'S 1 SEGREGATING SITE, THIS WON'T BE A MATRIX 
            if(segsites[draw] == 1) h <- as.matrix(h)
            ## OTHERWISE, IT NEEDS TO BE TRANSPOSED
            else h <- t(h)
        }
        else {
        	h <- matrix(nrow=nsam, ncol=0)
        	positions[[draw]]<- NA
        }
		 gamlist[[draw]] <- h
        stopifnot(all(dim(h) == c(nsam, segsites[draw]))) 
    }
#	cat("\n")
    list(segsites=segsites, gametes=gamlist, probs=probs, times=t(times), positions=positions, nsam=nsam, nreps=ndraws ) 
}
MK <-function(NS,SS){
cont <-rbind(NS,SS)
print(chisq.test(cont,correct=FALSE))
FI <-(NS[2]*SS[1])/(NS[1]*SS[2])
FI
}