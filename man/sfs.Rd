\name{sfs}
\alias{sfs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Site Frequency Spectrum
%%  ~~function to do ... ~~
}
\description{Function to plot unfolded site frequency spectrum from ms output
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sfs(dat, pl = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{raw output from an ms run (see \code{\link[phyclust]{ms}})
%%     ~~Describe \code{dat} here~~
}
  \item{pl}{\code{LOGICAL} Whether or not a plot will be created
%%     ~~Describe \code{pl} here~~
}
}
\details{Primarily for use of other functions, but can be used directly.  Inputting 
%%  ~~ If necessary, more details than the description above ~~
}
\value{
List of 2
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{nsites }{\code{integer} - number of segregating sites}
\item{site.spec}{\code{vector} - site frequency spectrum}
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
\code{\link[phyclust]{ms}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
sim <-ms(nsam=40,nreps=1,opts=c("-s 8")) #perform ms simulation, 40 samples and 8 segregating sites
ss <-sfs(sim)
ss

## The function is currently defined as
function (dat, pl = TRUE) 
{
    dat.ms <- read.ms.output(txt = dat)
    gams <- dat.ms$gametes[[1]]
    dist <- apply(gams, 2, sum)
    n <- nrow(gams)
    h <- hist(dist, breaks = c(0:(n - 1)), plot = FALSE)
    if (pl) 
        plot(h, , main = "Unfolded SFS", xlab = "i")
    sf <- h$counts
    return(list(n, sf))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
