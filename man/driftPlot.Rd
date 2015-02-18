\name{driftPlot}
\alias{driftPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulate Genetic Drift
}
\description{
Simulates random drift, plotting changes in allele frequencies and returning them as a matrix
}
\usage{
driftPlot(N, nreps = 10, p = 0.5, print = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{
A vector of population sizes
}
  \item{nreps}{
Number of replicate simulations to be performed
}
  \item{p}{Initial allele frequency}
  \item{print}{if TRUE, display graphical output}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Matrix of simulated values of p, in which rows are generations and columns are replicates
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Bruce J. Cochrane \email{cochrabj@miamioh.edu}
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{driftPlot}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
N <- rep(1000,100) # simulate a population of 1000 for 100 generations
driftPlot(N)

##Function
{
    ngen = length(N)
    plot(c(0, ngen), c(0, 1), type = "n", xlab = "Generation", 
        ylab = "p")
    sapply(c(1:nreps), function(x) {
        pi <- rep(0.5, ngen)
        gen <- c(1:ngen)
        for (i in 1:(ngen - 1)) {
            pi[i + 1] <- rbinom(1, 2 * N[i], pi[i])/(2 * N[i])
        }
        lines(gen, pi)
        pi
    })
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
