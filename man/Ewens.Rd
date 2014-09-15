\name{Ewens}
\alias{Ewens}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ewens Distribution Plot
}
\description{
Implementation of the Ewens-Watterson Test of the Infinite Alleles Model
}
\usage{
Ewens(n, k, fhat, nsim = 1000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of samples
}
  \item{k}{
Number of alleles.
}
  \item{fhat}{
Expected heterozygosity (output from function \code{\link{TeachingPopGen::fhat}})
}
  \item{nsim}{
number of simulations to perform (default=1000)
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
function (n, k, fhat, nsim = 1000) 
{
    x <- matrix(nrow = nsim, ncol = k)
    x[, ] = 0
    f <- rep(0, nsim)
    for (rep in 1:nsim) {
        if (rep == 1) {
            B <- matrix(nrow = 50, ncol = 2000)
            B[, ] <- 0
            for (J in 1:n) {
                B[1, J] <- 1/J
            }
            for (I in 2:k) {
                B[I, I] <- 1
                NM <- n - 1
                for (J in I:NM) {
                  B[I, J + 1] <- (I * B[I - 1, J] + J * B[I, 
                    J])/(J + 1)
                }
            }
        }
        km <- k - 1
        a <- runif(km, 0, 1)
        nleft = n
        for (l in 1:km) {
            cum = 0
            for (i in 1:nleft) {
                cum <- cum + B[k - l, nleft - i]/(B[k - l + 1, 
                  nleft] * i)
                if (cum >= a[l]) 
                  break
            }
            x[rep, l] = i
            nleft <- nleft - x[rep, l]
        }
        x[rep, k] = nleft
        f[rep] <- sum((x[rep, ]/n)^2)
    }
    print(mean(f))
    q <- quantile(f, c(0.025, 0.975))
    print(q)
    fhist <- hist(f, breaks = 20, plot = FALSE)
    hist(f, nclass = length(fhist$mids), xlab = "F", xlim = c(0, 
        1), main = paste("N =", n, "k =", k, sep = " "), col = ifelse(fhist$mids < 
        q[1], "red", ifelse(fhist$mids > q[2], "red", "white")))
    abline(v = fhat, col = "red")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\concept{ms}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
