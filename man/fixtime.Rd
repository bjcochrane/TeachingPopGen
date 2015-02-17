\name{fixtime}
\alias{fixtime}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simulation of Mean Time to Fixation
%%  ~~function to do ... ~~
}
\description{A function that performs drift simulations, identifies those for which the allele being tracked is fixed (p ->1) and returns the results as a list.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fixtime(pi, ngen = 1000, nreps = 1000, N = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pi}{starting allele frequency
%%     ~~Describe \code{pi} here~~
}
  \item{ngen}{number of generations to simulate (default=1000)
%%     ~~Describe \code{ngen} here~~
}
  \item{nreps}{number of replicate simulations to perform (default=1000)
%%     ~~Describe \code{nreps} here~~
}
  \item{N}{Population size (default=100)
%%     ~~Describe \code{N} here~~
}
}
\details{This function takes a brute force approach to determining mean time to fixation of an allele, given some starting frequency.  Based on the parameters provided, it generates the input to \code{\link{driftPlot}}.  The output from that function is then parsed to identify all of the simulatons run for which the final value of p is 1 (fixation).  The number of generations required for fixation is then determined.  
%%  ~~ If necessary, more details than the description above ~~
}
\value{ a \code{list} with the following:
\item{mean.time}{Mean time to fixation}
\item{N.fixed}{Number of simulations in which fixation occurred}
\item{fix.times}{Fixation times for each of the \code{N.fixed} simulations in which fixation occurred}
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

\seealso{\code{\link{driftPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
fix.out <-fixtime(p=.05)
head(fix.out)

##Function
{
    n <- rep(N, ngen)
    drft <- driftPlot(n, nreps, p = pi, print = FALSE)
    drft.fix <- drft[, which(drft[length(n), ] == 1)]
    fix.time <- apply(drft.fix, 2, function(x) length(which(x != 
        1)) + 1)
    list(mean.time = mean(fix.time), N.fixed = length(fix.time), 
        fix.times = fix.time)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
