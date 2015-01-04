\name{fhat}
\alias{fhat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculation of Expected Homozygosity
}
\description{
Calculates expected homozygosity (\eqn{\Sigma Pi^2}) based on the distribution of alleles in a sample. 
}
\usage{
fhat(dist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
A vector containing numbers of alleles in sample
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the sum of the squared allele frequencies, or the expected homozygosity of the sample assuming it is drawn from a Hardy-Weinberg Population
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bruce J. Cochrane
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
allele.numbers <- c(12,3,4,6,2,1)
out.fhat <-fhat(allele.numbers)
out.fhat

## The function is currently defined as
function (dist) 
{
    sum((dist/sum(dist))^2)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
