\name{datprep}
\alias{datprep}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Extract Genotype Numbers from HapMap Data
%%  ~~function to do ... ~~
}
\description{A function to extract genotype numbers from a .csv file downloaded from Hapmap
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
datprep(dat, min = 5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{csv file obtained from HapMap SNP genotype data
%%     ~~Describe \code{dat} here~~
}
  \item{min}{\code{integer}:  Cutoff theshold for exclusion of rare alleles (default=5)
%%     ~~Describe \code{min} here~~
}
}
\details{Genotype frequency data downloaded from HapMap as csv files can be read with \code{read.csv}.  This function will then extract numbers of the three genotypes (AA, Aa and aa) from the data.
%%  ~~ If necessary, more details than the description above ~~
}
\value{\code{data frame}:  Data frame of N rows, where N is the number of samples, and three rows containing genotype numbers
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
\code{\link[HapMap]{http://www.hapmap.org}}
}
\author{Bruce J. Cochrane \email{cochrabj@miamioh.edu}
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{read.csv}}
\code{\link{ch6.yri}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data (ch6.yri) # Data from Chromosome 6 region in Yoruba sample
df <-datprep(ch6.yri)
head(df)

## For reading a locally stored Hapmap file (assuming it is in the working directory)

\dontrun{
dat.raw <-read.table(file="filename",skip=3,row.names=1,sep=" ")
dat <-datprep(dat.raw)
}

## The function is currently defined as
function (dat, min = 5) 
{
    genos <- cbind(dat$V13, dat$V16, dat$V19)
    genos <- data.frame(genos)
    rownames(genos) <- rownames(dat)
    colnames(genos) <- c("AA", "Aa", "aa")
    genos.sub <- genos[genos[, 1] > min & genos[, 2] > min & 
        genos[, 3] > min, ]
    genos.sub
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
