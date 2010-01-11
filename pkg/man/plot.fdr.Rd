\name{plot.fdr}
\alias{plot.fdr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot(fdr)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fdr}{ ~~Describe \code{fdr} here~~ }
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
function (fdr) 
{
    data = fdr$Pvals
    ord = order(data$original.pvals)
    m = length(ord)
    data = data[ord, ]
    devAskNewPage(ask = T)
    plot(data$original.pvals, cex = 50/m, col = as.numeric(data$rejected) + 
        1, xlab = "Order of P-values", ylab = "", lty = 2, main = "Sorted pvals")
    lines(data$criticals, col = "red")
    abline(h = fdr$Cutoff)
    plot(data$adjusted.pvals ~ data$original.pvals, cex = 50/m, 
        main = "Adjusted p-vals", col = as.numeric(data$rejected) + 
            1, ylim = c(0, 1), xlab = "Original P-values", ylab = "Adjusted P-Values")
    abline(h = fdr$q)
    axis(side = 2, at = fdr$q, labels = "q", las = 2)
    devAskNewPage(ask = F)
  }
}

