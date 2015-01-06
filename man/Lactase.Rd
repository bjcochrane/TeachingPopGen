\name{Lactase}
\alias{Lactase}
\docType{data}
\title{SNP Data from the Lactase Gene
%%   ~~ data name/kind ... ~~
}
\description{Phased haplotype data from \url{http//www.hapmap.org} for use as input to \code{Ewens.hap} 
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Lactase)}
\format{
List of 3
  \describe{
    \item{YRI}{data.frame:	234 obs. of  25 variables} 
    \item{MKK}{data.frame:	286 obs. of  25 variables}
    \item{CEU}{data.frame:	242 obs. of  25 variables}
 }
}
\details{Data are from 25 SNPs on chromosome 2 in the vicinity of the \emph{lct} gene.  They cover the region chr2:136262314..136310628 and can be used to demonstrate both the function and the ability of the Ewens/Watterson test to demonstrate departures from neutral expectations.  Populations are Yoruba (\code{YRI}), Masai (\code{MKK}) and European (\code{CEU})

%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
\url{http://hapmap.ncbi.nlm.nih.gov/cgi-perl/gbrowse/hapmap3r2_B36/}; search on landmark \code{lct}
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
  \code{\link{Ewens}},  
  \code{\link{Ewens.hap}}
}
\examples{

data(Lactase)
par(mfrow=c(3,1))
out <-lapply(Lactase,Ewens.hap)     # display plots in graphics window
out     #gives mean value of Fexp, with upper and lower confidence intervals.
}
\keyword{datasets}
