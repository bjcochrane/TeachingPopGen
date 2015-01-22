\name{ch6.yri}
\alias{ch6.yri}
\docType{data}
\title{Sample HapMap Genotype Frequency Data}
\description{Data for 400 SNP's from chromosome 6 of the YRI dataset.  Format is that of downloaded text genotype frequency text file}
\usage{data(ch6.yri)}
\format{
data frame with 400 rows and 19 columns
}
\details{These are data primarilyfor use in demonstrating \code{\link{datprep}}. The adventurous user could extract more information, such as map position in base pairs, identity of SNPs and alleles, etc.}
\source{\url{http://www.hapmap.org}
}
\seealso{
\code{\link{datprep}}
}
\examples{\dontrun{

data(ch6.yri)
dat <-datprep(ch6.yri)
head(dat)
}}

