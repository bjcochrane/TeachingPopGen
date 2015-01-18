\name{genotype.generate}
\alias{genotype.generate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Generate Array of Two Locus Genotypes
%%  ~~function to do ... ~~
}
\description{A function that will, based on given gamete frequencies, linkage disequilibrium, and population size, return an array of genotypes for further analysis
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
genotype.generate(p, r, D, N = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{\code{numerical}:  
  frequency of AB gamete
%%     ~~Describe \code{p} here~~
}
  \item{r}{ \code{numerical}:  
  frequency of ab gamete
%%     ~~Describe \code{r} here~~
}
  \item{D}{ \code{numerical}:
  Value of linkage disequilibrium (-.25 < D < .25)
%%     ~~Describe \code{D} here~~
}
  \item{N}{ \code{integer}:  
  Population size (default=1000)
%%     ~~Describe \code{N} here~~
}
}
\details{  Inputs are the coupling (parental) gamete frequencies and D (f(AB)-f(A)f(B))
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{matrix}:
3 X 3 matrix of genotype numbers generated based on input values.  Suitable for input into \code{\link{HillD}}
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

\seealso{\code{\link{HillD}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Select parental gamete frequencies and D
gam1 <-.3
gam2 <-.4
D <-.1

## Generate 2000 genotypes 

genos <-genotype.generate(gam1,gam2,D,N=2000)
genos

## The function is currently defined as
function (p, r, D, N = 1000) 
{
    q <- 1 - p
    s <- 1 - r
    gams <- rep(0, 4)
    gams[1] <- p * r + D
    gams[2] <- p * s - D
    gams[3] <- q * r - D
    gams[4] <- q * s + D
    genos <- matrix(nrow = 3, ncol = 3)
    rownames(genos) <- c("AA", "Aa", "aa")
    colnames(genos) <- c("BB", "Bb", "bb")
    genos[1, 1] <- gams[1]^2
    genos[1, 2] <- 2 * gams[1] * gams[2]
    genos[1, 3] <- gams[2]^2
    genos[2, 1] <- 2 * gams[1] * gams[3]
    genos[2, 2] <- 2 * gams[1] * gams[4] + 2 * gams[2] * gams[3]
    genos[2, 3] <- 2 * gams[2] * gams[4]
    genos[3, 1] <- gams[3]^2
    genos[3, 2] <- 2 * gams[4] * gams[3]
    genos[3, 3] <- gams[4]^2
    genos <- round(N * genos)
    return(genos)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
