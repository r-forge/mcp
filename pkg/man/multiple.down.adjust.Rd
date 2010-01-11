\name{multiple.down.adjust}
\alias{multiple.down.adjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
multiple.down.adjust(sorted, m)
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
    cat("Calling multiple.down.ajust \n")
    flush.console
    adjusted = rep(NA, m)
    temp.max = sorted[1]
    max.ind = rep(0, m)
    for (i in 1:m) {
        temp = min(sorted[i] * (m + 1 - i)/(i * (1 - sorted[i])), 
            1)
        if (temp >= temp.max) {
            temp.max = temp
            max.ind[i] = 1
        }
        adjusted[i] = temp.max
    }
    cat("Exiting multiple.down.ajust \n")
    flush.console
    return(adjusted)
  }
}

