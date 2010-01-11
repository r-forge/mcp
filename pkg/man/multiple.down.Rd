\name{multiple.down}
\alias{multiple.down}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiple.down(sorted, q, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{q}{ ~~Describe \code{q} here~~ }
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
function (sorted, q, m) 
{
    cat("Calling multiple step down \n")
    flush.console
    if (q > 0.5) 
        warning("q values over 0.5 are not recommended")
    criticals = sapply(1:m, function(i) q * i/(m - i * (1 - q) + 
        1))
    indicators = sorted < criticals
    if (!indicators[1]) 
        cutoff = list(cutoff = 0, cut.index = 0)
    else if (all(indicators)) 
        cutoff = list(cutoff = sorted[m], cut.index = m)
    else {
        cut.index = min((1:m)[!indicators]) - 1
        cutoff = list(cutoff = sorted[cut.index], cut.index = cut.index)
    }
    rejected = sorted <= cutoff$cutoff
    adjusted = multiple.down.adjust(sorted, m)
    multiple.pvals = data.frame(original.pvals = sorted, criticals = NA, 
        rejected = rejected, adjusted.pvals = adjusted)
    output = list(Cutoff = cutoff, Pvals = multiple.pvals)
    flush.console
    return(output)
  }
}

