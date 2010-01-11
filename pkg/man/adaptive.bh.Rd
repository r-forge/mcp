\name{adaptive.bh}
\alias{adaptive.bh}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
adaptive.bh(sorted, q, m)
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
    cat("Calling Adaptive BH \n")
    flush.console
    stage.one = bh(sorted, q, adjust = T, m = m)
    r = sum(stage.one$Pvals[["rejected"]])
    if (r == 0) 
        return(stage.one)
    else m0 = bh.m0.estimate(sorted = sorted, m = m)
    output = bh(sorted = sorted, q = q, m0 = m0, m = m)
    output$Pvals[["adjusted.pvals"]] = bh.adjust(sorted, m = m, 
        m0 = m0)
    return(output)
  }
}

