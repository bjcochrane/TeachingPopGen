\name{MK}
\alias{MK}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{McDonald Krietman Test
%%  ~~function to do ... ~~
}
\description{Performs chi-squared contingency test on polymorphism and divergence data
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MK(Div, Poly)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Div}{\code{integer}:  Vector of number of nonsynonymous and synonymous divergent sites (between species).
%%     ~~Describe \code{Div} here~~
}
  \item{Poly}{\code{integer}: Vector of nonsynonymous and synonymous polymorphic sites (within species).
%%     ~~Describe \code{Poly} here~~
}
}
\details{A simple test of McDonald-Kreitman data.   A chi-square contingency test, with continuity correction, is performed and the Fixation Index is calculated.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{ch}{Results of contingency test}
\item{FI}{Fixation Index}
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

\seealso{\code{\link{chisq.test}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
div <-c(7,17)
poly <-c(2,42)
MK(div,poly)

## The function is currently defined as
function (Div, Poly) 
{
    tbl <- rbind(Div, Poly)
    ch <- chisq.test(tbl,correct=TRUE)
    FI <- (Div[1] * Poly[2])/(Div[2] * Poly[1])
    list(ch, FI)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
