\name{hw}
\alias{hw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Basic Hardy Weinberg Calculations
%%  ~~function to do ... ~~
}
\description{Performs basic Hardy-Weinberg calculations for a single locus with two alleles.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
hw(obs, print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obs}{vector of observed genotype numbers (see example)
%%     ~~Describe \code{obs} here~~
}
  \item{print}{Logical; if true, results will be displayed on console
%%     ~~Describe \code{print} here~~
}
}
\details{Given the input, the function will calculate the allele frequencies (p and q), and expected genotype numbers in a Hardy Weinberg population.  It also performs a simple chi-square test (1 d. f., no continuity correction) and returns the value of chi-squared and the associated probability value.
%%  ~~ If necessary, more details than the description above ~~
}
\value{List, consisting of  
%%  ~Describe the value returned
%%  If it is a LIST, use
\item{Observed }{Observed genotype numbers (those inputted to function)}
\item{Expected}{Vector - expected genotype numbers based on calculated allele frequencies}
\item{Allele_freqs}{Vector - computed allele frequencies}
\item{Chisq}{Value of chi-squared calculated based on observed and expected genotype numbers}
\item{p }{probability of observing chi-squared value}
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
# run function with console display active
obs <-c(670,1786,188) # observed numbers of AA, Aa and aa genotypes
out <- hw(obs) 

# Suppress console display; return list

out <-hw(obs,print=FALSE)
str(out)

## The function is currently defined as
function (obs, print = TRUE) 
{
    n <- sum(obs)
    p <- round((2 * obs[1] + obs[2])/(2 * n), 4)
    q <- 1 - p
    exp <- as.integer(round(c(p^2 * n, 2 * p * q * n, q^2 * n), 
        0))
    chi <- sum((obs - exp)^2/exp)
    pr <- 1 - pchisq(chi, 1)
    if (print) {
        print(paste("p=", p, "q=", q, sep = " "), digits = 3, 
            quote = FALSE)
        print(cbind(obs, exp))
        print(paste("chi squared =", round(chi, 3), "p = ", round(pr, 
            3), "with 1 d. f.", sep = " "), quote = FALSE)
    }
    out <- list(obs, exp, chi, pr)
    names(out) <- c("Observed", "Expected", "Chisq", "p")
    out
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
