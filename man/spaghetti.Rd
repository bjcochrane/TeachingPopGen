\name{spaghetti}
\alias{spaghetti}
\docType{data}
\title{\emph{Spaghetti sauce} DNA Sequences
%%   ~~ data name/kind ... ~~
}
\description{42 aligned DNA sequences, each consisting of 702 base pairs from the \emph{ss} gene
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(spaghetti)}
\format{
  List of 42 sequences in DNAbin format
}
\details{
Data is loaded in the list version of DNAbin.  All sequences are of equal length, so it can be converted to a matrix with \code{as.matrix}
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
\url{http://www.ncbi.nlm.nih.gov/popset/345105052}
}
\references{
Legrand, D. et al, 2011.  Inter-island divergence wotjom \emph{Drosophila mauritiana}, a species of the \emph{D. simulans} complex:  Past history and/or speciation in progress?  Molecular ecology \strong{20}: 2787-2804. 

\url{http://onlinelibrary.wiley.com/doi/10.1111/j.1365-294X.2011.05127.x/abstract}
}
%%  ~~ possibly secondary sources and usages ~~

\examples{
data(spaghetti)
ss.mat <-as.matrix(spaghetti) #convert to matrix
image.DNAbin(ss.mat) #visualizing aligned sequences with function from \code{ape}
tajima.test(spaghetti) #perform Tajima test with function from \code{pegas}; also works with matrix data
}
\keyword{datasets}
