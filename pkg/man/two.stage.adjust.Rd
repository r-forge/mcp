\name{two.stage.adjust}
\alias{two.stage.adjust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  Utility function for the "fdr" function in package mcp.project.
}
\usage{
two.stage.adjust(sorted, r = 0, patience = 4)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sorted}{ ~~Describe \code{sorted} here~~ }
  \item{r}{ ~~Describe \code{r} here~~ }
  \item{patience}{ ~~Describe \code{patience} here~~ }
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
function (sorted, r = 0, patience = 4) 
{
    m = length(sorted)
    adjusted = rep(0, m)
    adjusted.q = solve.q(sorted = sorted, m = m, j = 1, r = 0)
    checking = adjusted.q
    if (sum(bh(sorted, adjusted.q[1]/(1 + adjusted.q[1]), m = m)$Pvals[["rejected"]]) == 
        m) {
        adjusted.q = rep(adjusted.q[1], m)
        return(adjusted.q)
    }
    else {
        for (j in 1:m) {
            delta.r = 1
            delta.q = 1
            new.q = adjusted.q[j]
            r.new = sum(bh(sorted, new.q/(1 + new.q), m = m)$Pvals[["rejected"]])
            counter = 0
            max.q = 0
            while (delta.r > 0 & delta.q > 0) {
                old.q = new.q
                r.old = r.new
                new.q = solve.q(sorted = sorted, m = m, j = j, 
                  r = r.old)[j]
                r.new = sum(bh(sorted, new.q/(1 + new.q), m = m)$Pvals[["rejected"]])
                delta.r = abs(r.new - r.old)
                delta.q = abs(new.q - old.q)
                counter = counter + 1
                if (counter > patience & max.q != new.q) 
                  max.q = max(max.q, new.q)
                else if (counter > patience & max.q == new.q) 
                  break
            }
            adjusted.q[j] = min(new.q, 1)
            adjusted.q[min(j + 1, m)] = adjusted.q[j]
            stopifnot(any(adjusted.q[j] <= checking[j]))
        }
        temp.min = adjusted.q[m]
        for (i in m:1) {
            if (adjusted.q[i] <= temp.min) 
                temp.min = adjusted.q[i]
            else adjusted.q[i] = temp.min
        }
        return(adjusted.q)
    }
  }
}

