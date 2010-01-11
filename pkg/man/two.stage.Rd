\name{two.stage}
\alias{two.stage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  Utility function for the "fdr" function in package mcp.project.
}
\usage{
two.stage(sorted, q, m)
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
    cat("Calling two.stage \n")
    flush.console
    q1 = q/(1 + q)
    stage.one = bh(sorted, q1, adjust = T, m = m)
    r = sum(stage.one$Pvals[["rejected"]])
    if (r == 0) {
        stage.one$Pvals[["adjusted.pvals"]] = 1
        return(stage.one)
    }
    else if (r == m) {
        stage.one$Pvals[["adjusted.pvals"]] = stage.one$Pvals[["adjusted.pvals"]][1]
        return(stage.one)
    }
    else {
        m0 = m - r
        output = bh(sorted = sorted, q = q1, m0 = m0, m = m)
        output$Pvals[["adjusted.pvals"]] = two.stage.adjust(sorted, 
            q)
        return(output)
    }
  }
}

