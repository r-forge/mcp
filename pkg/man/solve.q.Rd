\name{solve.q}
\alias{solve.q}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
solve.q(sorted, m, j, r = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{m}{ ~~Describe \code{m} here~~ }
  \item{j}{ ~~Describe \code{j} here~~ }
  \item{r}{ ~~Describe \code{r} here~~ }
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
function (sorted, m, j, r = 0) 
{
    a = sorted * (m - r)/(1:m)
    stopifnot(a > 0, j >= 1, j <= m, r >= 0, r <= m)
    adjusted = ifelse(a > 0.5, 1, a/(1 - a))
    temp.min = adjusted[m]
    for (i in m:j) {
        if (adjusted[i] <= temp.min) 
            temp.min = adjusted[i]
        else adjusted[i] = temp.min
    }
    return(adjusted)
  }
}

