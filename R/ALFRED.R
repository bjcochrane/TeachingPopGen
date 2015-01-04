## Basic read function
library(XML)
library(stringr)
read.alfred <-function(siteid){
  url <-"http://alfred.med.yale.edu/alfred/SiteTable1A_working.asp?siteuid="
  the.url <-paste(url,siteid,sep="")
  dat.region <- readHTMLTable(the.url,which=3)
  dat.region <-dat.region[-(1:2),]
  test <-as.numeric(str_sub(dat.region$V3,start=1,end=2))
  dat.region$V3=test
  pops2 <-gsub( " *\\(.*?\\) *", "", dat.region$V2)
  dat.region$V2=pops2
  dat.region <-dat.region[,-(4:5)]
  colnames(dat.region) <-c("Continent","Population","N2","P","Q")
  dat.region$P <-as.numeric(as.character(dat.region$P))
  dat.region$Q <-as.numeric(as.character(dat.region$Q))
  (dat.region)
}
  ## Aggregates data by continent
  
  aggregate.alfred <-function(dat){
    Nq <-as.integer(dat$N2*dat$Q)
    dat <-cbind(dat,Nq)
    dat.cont <-aggregate(data.frame(dat$N2,dat$Nq),list(dat$Continent),sum)
    colnames(dat.cont) <-c("Continent","N2","Nq")
    dat.cont
  }
  ### Calculates Fst by continent
    
    fst.cont <-function(dat.cont){
      qvar <-var(dat.cont$Nq/dat.cont$N2)
      pmean <-sum(dat.cont$Nq)/sum(dat.cont$N2)
      fst <-qvar/(pmean*(1-pmean))
      fst
    }

# Function to aggregate duplicate populations and return continent, population 2N, Nq and q

aggByPop.alfred <- function(dat){
  #First create a dataframe with the necessary elements (all except p)
  dat <-dat[,-4]
  
  #Now add a column for Nq
  
  
  
  Nq <-as.integer(dat$N2*dat$Q)
  dat <- cbind(dat,Nq)
  dat.pop <-aggregate(data.frame(dat$N2,dat$Nq),list(Continent=dat$Continent, Population=dat$Population),sum)
  colnames(dat.pop) <-c("Continent","Population","N2","Nq")
  Q <-dat.pop$Nq/dat.pop$N2
  dat.pop <-cbind(dat.pop,Q)
  dat.pop
}

# Function to calculate nested F statistics

fst.nest <-function(dat.pop){
  Qt <- mean(dat.pop$Q)
  Ht <-1-Qt^2-(1-Qt)^2
  Qc <- aggregate(dat.pop$Q,list(dat.pop$Continent),mean)
  #Qc <-mean(Qc[,2])
  Hr <-2*mean(Qc$x*(1-Qc$x))
  Qs <-dat.pop$Q
  Hs <-mean(1-Qs^2-(1-Qs)^2)
  Fsr <-(Hr-Hs)/Hr
  Frt <-(Ht-Hr)/Ht
  Fst <-(Ht-Hs)/Ht
  data.frame(Fsr, Frt, Fst)
}