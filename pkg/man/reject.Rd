\name{reject}
\alias{reject}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
reject(sorted, criticals)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{criticals}{ ~~Describe \code{criticals} here~~ }
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
function (sorted, criticals) 
{
    cat("Calling Reject \n")
    flush.console
    m = length(sorted)
    stopifnot(length(criticals) == m)
    indicators = sorted < criticals
    if (!any(indicators)) 
        return(list(cutoff = 0, cut.index = 0))
    cut.index = max((1:m)[indicators])
    cutoff = sorted[cut.index]
    return(list(cutoff = cutoff, cut.index = cut.index))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

