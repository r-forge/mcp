\name{bh}

\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bh(sorted, q, m, adjust = FALSE, m0 = m, pi0, constant = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{q}{ ~~Describe \code{q} here~~ }
  \item{m}{ ~~Describe \code{m} here~~ }
  \item{adjust}{ ~~Describe \code{adjust} here~~ }
  \item{m0}{ ~~Describe \code{m0} here~~ }
  \item{pi0}{ ~~Describe \code{pi0} here~~ }
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
function (sorted, q, m, adjust = F, m0 = m, pi0, constant = 1) 
{
    cat("Calling bh \n")
    flush.console
    if (missing(m0) & !missing(pi0)) 
        m0 = pi0 * m
    else {
        criticals = (1:m) * q/(m0 * constant)
        cutoff = reject(sorted, criticals)
        rejected = sorted <= cutoff$cutoff
        adjusted = rep(NA, m)
        if (adjust) 
            adjusted = bh.adjust(sorted, m = m, m0 = m0, constant = constant)
        multiple.pvals = data.frame(original.pvals = sorted, 
            criticals = criticals, rejected = rejected, adjusted.pvals = adjusted)
        output = list(Cutoff = cutoff, Pvals = multiple.pvals)
        return(output)
    }
  }
}

