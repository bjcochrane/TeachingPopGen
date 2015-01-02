dat <-read.genalex("./TestData/GoughMice.csv")
str(dat)
head(dat$genotypes)
#Try the Boechera data with subdivision
dat2 <-read.genalex("./Data/Boechera.csv")
head(dat2$genotypes)
str(dat2)
#try to read in the popnames 
filename="./Data/Boechera.csv"
#manually ran the part to generate parameters, now try to read names
pnames <-read.csv(filename,skip=1,nrows=1,header=FALSE)
pnames
pnames <-pnames[,4:(3+npop)]
pnames
dat.pegas <-as.pegas(dat)
str(dat.pegas)
dat.pegas
data(acp29)
acp29.seq
tajima.test(acp29.seq)
data(spaghetti)
spaghetti
devtools::use_vignette("my-vignette")
dat
devtools::build_vignettes()
vignette(package="TeachingPopGen")
