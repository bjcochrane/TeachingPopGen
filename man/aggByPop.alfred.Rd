\name{aggByPop.alfred}
\alias{aggByPop.alfred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Aggregate ALFRED Data by population
%%  ~~function to do ... ~~
}
\description{

A short utility to combine multiple entries from one population in the results of \code{read.alfred} 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
aggByPop.alfred(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{output from \code{read.alfred}
%%     ~~Describe \code{dat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{read.alfred}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat) 
{
    dat <- dat[, -4]
    Nq <- as.integer(dat$N2 * dat$Q)
    dat <- cbind(dat, Nq)
    dat.pop <- aggregate(data.frame(dat$N2, dat$Nq), list(Continent = dat$Continent, 
        Population = dat$Population), sum)
    colnames(dat.pop) <- c("Continent", "Population", "N2", "Nq")
    Q <- dat.pop$Nq/dat.pop$N2
    dat.pop <- cbind(dat.pop, Q)
    dat.pop
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
