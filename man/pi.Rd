\name{pi}
\alias{pi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Calculation of Pairwise Differences
%%  ~~function to do ... ~~
}
\description{Internal function ti calculate pi (average pairwise differences among a set of DNA sequences) from the site-frequency spectrum generated from those sequences.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pi(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{ output from function \code{sfs}
%%     ~~Describe \code{dat} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\code{Numeric} - computed value of pi
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Bruce J. Cochrane \email{cochrabj@miamioh.edu}}
%%  ~~who you are~~

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[phyclust]{ms}}
\code{\link{sfs}}
\code{\link{read.ms.output}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{sfs}}
}
\examples{
set.seed(123)
#ms run, 50 samples, one replicate, 15 segregating sites
dat <-ms (nsam=50,nreps=1,opts=("-s 15")) 
dat.ms <-read.ms.output(dat) #convert output to list
gametes <-dat.ms$gametes[[1]] #extract gametic array
dat.sfs <-sfs(gametes,pl=FALSE) #compute sfs, suppressing plotting
dat.pi <-pi(dat.sfs) #calulate pi
dat.pi

## The function is currently defined as
function (dat) 
{
    sf <- dat[[2]]
    n <- dat[[1]]
    pi2 <- 2 * sf * c(1:(n - 1)) * c(n - c(1:(n - 1)))
    pi2 <- sum(pi2/(n * (n - 1)))
    return(pi2)
  }
}

