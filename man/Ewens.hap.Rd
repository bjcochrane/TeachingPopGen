\name{Ewens.hap}
\alias{Ewens.hap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ewens Test for Hapmap Data
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Ewens.hap(dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
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
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dat) 
{
    unique.hap <- unique(dat)
    unique.hap <- unique.hap[-nrow(unique.hap), ]
    dat.sum <- aggregate(rep(1, nrow(dat)), by = as.list(dat), 
        FUN = sum)
    dat.sum <- dat.sum[-(nrow(dat.sum)), ]
    fh <- fhat(dat.sum$x)
    print(fh)
    Ewens(sum(dat.sum$x), length(dat.sum$x), fh)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
