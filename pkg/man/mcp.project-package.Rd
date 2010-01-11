\name{mcp.project-package}
\alias{mcp.project-package}
\alias{mcp.project}
\docType{package}
\title{
Applies different False Discovery Rate controlling procedures
}
\description{
The package offers several \acronym{FDR} controlling procedures. Given a p-value vector it will return the rejected hypothsis, the cutoff and the adjusted p-values.
}
\details{
    \tabular{ll}{
        Package: \tab mcp.project\cr
        Type: \tab Package\cr
        Version: \tab 1.0\cr
        Date: \tab 2008-09-03\cr
        License: \tab Unlimited\cr
        LazyLoad: \tab yes\cr
    }
The main function in this package is \code{fdr} which test hypothsis using different \acronym{FDR} controlling procedures. The output is an object of class \acronym{FDR} which can be examined with the generic \code{plot} and \code{summary} functions.
}
\author{
Jonathan Rosenblatt

Maintainer: Jonathan Rosenblatt<john.ros@gmail.com>
}
\references{
    \emph{Adaptive linear step-up procedures that control the false discovery rate.} Y. Benjamini, A.M. Krieger, D. Yekutieli (Biometrika 2006)    
}

\keyword{ package }
\keyword{ htest }

\seealso{
    \url{http://www.math.tau.ac.il/~ybenja/fdr/index.htm}
    \url{http://strimmerlab.org/notes/fdr.html}
}

\examples{

#Generating test data
test=runif(100)^3

#Linear Step up
bh.1=fdr(x=test,q=0.1,'BH')
plot(bh.1)
summary(bh.1)
table(bh.1$Pvals[['adjusted.pvals']]<=bh.1$q)
}
