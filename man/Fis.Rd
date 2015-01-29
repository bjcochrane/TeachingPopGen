\name{Fis}
\alias{Fis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculate Simple F Statistic
%%  ~~function to do ... ~~
}
\description{A short function that accepts either genotype numbers or genotype frequencies and returns 1-Hobs/Hexp
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Fis(genos, freq = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{genos} - {\code{vector}:  Either numbers or frequencies of genotypes in standard order
%%     ~~Describe \code{genos} here~~
}
  \item{freq} - {\code{logical}:  \code{TRUE} (default) for frequency data.  \code{FALSE} for genotype numbers
%%     ~~Describe \code{freq} here~~
}
}
\details{  Given three genotypes of a biallelic locus (either frequency or numbers), the function calculates F and returns it.
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{numeric}:  Value of F
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Bruce J. Cochrane \email{cochrabj@miamioh.edu}
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## From frequencies

dat <-c(.22,.44,.34) #frequencies of AA, Aa, and aa
f <-Fis(dat)
f

## From numbers

dat <-c(33,68,12) #numbers of AA, Aa, and aa

f <-Fis(dat,freq=FALSE)
f

## The function is currently defined as
function (genos, freq = TRUE) 
{
    if (freq == FALSE) {
        genos = genos/sum(genos)
    }
    p <- genos[1] + 0.5 * genos[2]
    Hexp <- 2 * p * (1 - p)
    f <- (Hexp - genos[2])/Hexp
    f
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
