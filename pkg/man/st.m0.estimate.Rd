\name{st.m0.estimate}
\alias{st.m0.estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  Utility function for the "fdr" function in package mcp.project.
}
\usage{
st.m0.estimate(sorted, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{m}{ ~~Describe \code{m} here~~ }
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
function (sorted, m) 
{
    k = 2:m
    m0.k = (m + 1 - k)/(1 - sorted[-1])
    diffs = diff(m0.k, lag = 1)
    indicators = diffs > 0
    optimal.k = min(k[indicators])
    m0 = min(ceiling(m0.k[optimal.k]), m)
    stopifnot(m0 <= m, m0 > 0)
    return(m0)
  }
}

