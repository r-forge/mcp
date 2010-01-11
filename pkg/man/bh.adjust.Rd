\name{bh.adjust}
\alias{bh.adjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bh.adjust(sorted, m, m0, constant = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{m}{ ~~Describe \code{m} here~~ }
  \item{m0}{ ~~Describe \code{m0} here~~ }
  \item{constant}{ ~~Describe \code{constant} here~~ }
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
function (sorted, m, m0, constant = 1) 
{
    cat("Calling bh.adjust \n")
    flush.console
    adjusted = rep(NA, m)
    temp.min = sorted[m]
    min.ind = rep(0, m)
    for (i in m:1) {
        temp = min(m0 * sorted[i] * constant/i, 1)
        if (temp <= temp.min) {
            temp.min = temp
            min.ind[i] = 1
        }
        adjusted[i] = temp.min
    }
    return(adjusted)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

