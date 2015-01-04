\name{colhist}
\alias{colhist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create Histogram with Colored Tails
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A function that plots a histogram, with either one tail (upper 95\%) or two tails (upper and lower 2.5\%, default) colored in red
}
\usage{
colhist(dat, breaks = 20, tail = 2, labs = c("X", "Number", "Title"), xr = c(0, 0))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{vector of data to be plotted
%%     ~~Describe \code{dat} here~~
}
  \item{breaks}{ number of breaks (integer, default=20)
%%     ~~Describe \code{breaks} here~~
}
  \item{tail}{number of tails (default=2)
%%     ~~Describe \code{tail} here~~
}
  \item{labs}{character vector consisting of X axis label, Y axis label, and plot title
%%     ~~Describe \code{labs} here~~
}
  \item{xr}{ x axis range (c(min, max)).  Note - default uses range of data.
%%     ~~Describe \code{xr} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{ Produces plot of histogram
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
Bruce Cochrane (\email{cochrabj@miamioh.edu})
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

## Plot a normal distribution with two tails highlighted

x <-rnorm (10000,0,1)
colhist(x)

### Do the same, highlighting one tail.

colhist(x, tail=1)

### Add labels to the above and specify x axis range

colhist(x, tail=1, labs = c("Normal Data", "N", "Normal Distribution (0,1)"),xr=c(-10,10))

## The function is currently defined as
function (dat, breaks = 20, tail = 2, labs = c("X", "Number", 
    "Title"), xr = c(0, 0)) 
{
    flhist <- hist(dat, breaks = 20, plot = FALSE)
    if (identical(xr, c(0, 0))) 
        xr = (range(flhist$breaks))
    if (tail == 2) {
        q <- quantile(dat, c(0.025, 0.975))
        hist(dat, nclass = length(flhist$mids), col = ifelse(flhist$mids < 
            q[1], "red", ifelse(flhist$mids > q[2], "red", "white")), 
            xlab = labs[1], ylab = labs[2], main = labs[3], xlim = xr)
    }
    if (tail == 1) {
        q <- quantile(dat, 0.95)
        hist(dat, nclass = length(flhist$mids), col = ifelse(flhist$mids > 
            q, "red", "white"), xlab = labs[1], ylab = labs[2], 
            main = labs[3], xlim = xr)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
