\name{read.https}
\alias{read.https}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read data from url
%%  ~~function to do ... ~~
}
\description{Similar to \code{read.csv}, only uses a url (http:// or https//) as input.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.https(url, header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{url}{:  url of data source; tested on public links from Dropbox and Google Drive}
=======
\item{url}{\code{character} - url of data source; tested on public links from Dropbox and Google Drive}

}
\details{
Other paramters are described in the documentation for \code{\link{read.table}}.  Default values are those of \code{\link{read.csv}}.
}
\value{ Returns a data frame of the tabular file linked to.  Unless specified othewise (for example with header=FALSE or row.names=1), the first input line will be treated as variable names and rows (cases) will be numbered sequentially.
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

\seealso{
\code{\link{read.table}}
\code{\link{read.csv}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
url <- "https://dl.dropboxusercontent.com/u/9752688/QPopgen/fms.csv"
dat <-read.https(url)
str(dat)

## The function is currently defined as
function (url, header = TRUE, sep = ",", quote = "\"", dec = ".", 
    fill = TRUE, comment.char = "", ...) 
{
    csv <- getURL(url)
    read.table(textConnection(csv), header = header, sep = sep, 
        quote = quote, dec = dec, fill = fill, comment.char = comment.char, 
        ...)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
