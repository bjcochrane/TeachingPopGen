\name{chixw}
\alias{chixw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple chi-square test
%%  ~~function to do ... ~~
}
\description{A small function that will do a chi-squared test
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
chixw(obs, exp, df = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{\code{numeric}:  Vector of observed values
%%     ~~Describe \code{obs} here~~
}
  \item{exp}{\code{Numeric}: Vector of expected values
%%     ~~Describe \code{exp} here~~
}
  \item{df}{\code{Integer}:  Degrees of freedom (default = 1)
%%     ~~Describe \code{df} here~~
}
}
\details{This function performs the simplest possible chi-squared test.  Observed and expected numbers are provided as vectors, as are degrees of freedom (defaults to 1). 
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{Data Frame}:  chi-squared, associated probability and degrees of freedom
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

obs <-c(95,150,7)
exp <-c(100,133,19)
res <-chixw(obs,exp,2) # assume two degrees of freedom
res


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
