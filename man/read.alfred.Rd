\name{read.alfred}
\alias{read.alfred}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read Data from ALFRED Data Base
%%  ~~function to do ... ~~
}
\description{Alfred (\url{http://alfred.med.yale.edu/}) is a database of human allele frequencies from a wide variety of geographic sources.  This function will retrieve and format data for a particular pretermined SNP (see details)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.alfred(siteid)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{siteid}{
  ALFRED UID for frequency table of SNP of interest (character - see details)
%%     ~~Describe \code{siteid} here~~
}
}
\details{The user navigates to the page describing the data for the SNP of interest.  The ALFRED UID is displayed; it can be cut and pasted into the funcion call.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data frame is returned, with the following columns
\tabular{ll}{
Continent\tab Continent of origin \cr
Population\tab Population \cr
N2\tab 2N (number of alleles scored)\cr
P\tab Frequency of Allele 1 \cr
Q\tab Frequency of Allele 2\cr
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Cheung KH, Miller PL, Kidd JR, Kidd KK, Osier MV, Pakstis AJ.  ALFRED: a Web-accessible allele frequency database. Pac Symp Biocomput \strong{2000}: 639-50 (\url{http://alfred.med.yale.edu/alfred/PSB\%202000.pdf})
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
## Example using data for rs4954633, a SNP in the \emph{Lactase} gene

site <-"SI453223T"
dat <-read.alfred(site)
head(dat)


## The function is currently defined as
function (siteid) 
{
    url <- "http://alfred.med.yale.edu/alfred/SiteTable1A_working.asp?siteuid="
    the.url <- paste(url, siteid, sep = "")
    dat.region <- readHTMLTable(the.url, which = 3)
    dat.region <- dat.region[-(1:2), ]
    test <- as.numeric(str_sub(dat.region$V3, start = 1, end = 2))
    dat.region$V3 = test
    pops2 <- gsub(" *\\(.*?\\) *", "", dat.region$V2)
    dat.region$V2 = pops2
    dat.region <- dat.region[, -(4:5)]
    colnames(dat.region) <- c("Continent", "Population", "N2", 
        "P", "Q")
    dat.region$P <- as.numeric(as.character(dat.region$P))
    dat.region$Q <- as.numeric(as.character(dat.region$Q))
    (dat.region)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
