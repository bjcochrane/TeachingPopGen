\name{acp29}
\alias{acp29.seq}
\docType{data}
\title{acp29 Sequences
%%   ~~ data name/kind ... ~~
}
\description{
Seventeen DNA sequences from the acp29 locus of Drosophila melanogaster
}
\usage{data(acp29)}
\format{
  A DNAbin file containing 17 sequences of 702 bases each
  
}
\details{
Data will be loaded as acp29.seq in the list format .  It can then be used to perform sequence-based analyses in ape, pegas, etc.
}
\source{
 \url{http://www.ncbi.nlm.nih.gov/popset/11245776}
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(acp29)
acp29.seq #displays basic data
image.DNAbin(as.matrix(acp29.seq))  # visualize aligned sequences
tajima.test(acp29.seq) #perform Tajima's test for departure from neutral predictions
}
\keyword{datasets}
