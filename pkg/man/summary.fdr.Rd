\name{summary.fdr}
\alias{summary.fdr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary(fdr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdr}{ ~~Describe \code{fdr} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--    or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (fdr) 
{
    r = sum(fdr$Pvals[["rejected"]])
    m = length(fdr$Pvals[["rejected"]])
    cat("There were", r, "rejections out of", m, "hypothsis tested which are", 
        r/m, "percent \n")
    cat("Using method", fdr$method, "at FDR level of", fdr$q, 
        "\n")
  }
}

