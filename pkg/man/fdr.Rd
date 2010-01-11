\name{fdr}

\alias{fdr}
\alias{bh}
\alias{plot.fdr}
\alias{summary.fdr}

\title{ Multiple hypothesis testing while controlling the False Discovery rate}

\description{
  The main function of package mcp.project. Given a p-value vector, the function will return the rejected hypothesis, the cutoff, and the FDR adjusted p-values according to the procedure selected.
}

\usage{
fdr(x, q, method = "BH", pi0, lambda = 0.5, m0)  

}

\arguments{
  \item{x}{ A vector if p-values.}
  \item{q}{ The False Discovery Rate to control for. }
  \item{method}{ The desired procedure. See Details. }
  \item{pi0}{ An optional argument \emph{instead} of \code{m0} when the proportion of true null hypothsis is known. }
  \item{lambda}{ The threshold for Story's \code{m0} estimator. }
  \item{m0}{ The number of true null hypothsis. Usually unknown.}
}

\details{
  The \code{fdr} function is the main function in the package \pkg{mcp.project}.
Given a p-value vector, it can apply several \acronym{FDR} controlling procedures and return the rejected hypothesis, the cutoff and the adjusted p-values.
The methods supplied and chosen by the \var{method} parameter are the following:

\describe{
    \item{\kbd{\sQuote{BH}}}{ the plain Linear Step-up procedure is the default procedure of this function. Known to control the \acronym{FDR} in the case of independant or positively correlated test statistics.\cr}

    \item{\kbd{\sQuote{General Dependency}}}{ A "worst case" procedure that controls the \acronym{FDR} even when the dependency structure between the p-values is unknown.\cr}

    \item{\kbd{\sQuote{Oracle}}}{ When \code{m0} is \emph{miraculously} known it can be used to increase the power of the Linear Step Up procedure while still controlling the False Discovery Rate (as long as the dependence structure is such that a linear step up will control the FDR). This has been shown to be the  assymptotically most \dQuote{powerful} procedure which controls the \acronym{FDR} (Genovese and Wasserman 2002).\cr}

    \item{\kbd{\sQuote{BH Adaptive}}}{ An adaptive procedure which gains power by first estimating \code{m0} using a \acronym{FWE} derived estimator and then plugging it in a Linear Step-up. \acronym{FDR} control under independence has not been proven, yet it is controlled in simulation studies. See the references for further details. \cr}

    \item{\kbd{\sQuote{ST Adaptive}}}{ An adaptive procedure. First estimating \code{m0} and then and then plugging it in a Linear Step-up. This procedure requires a threshold (lambda) which is set at 0.5 by default. FDR is *not* controlled if all null hypothesis are true. See the references for further details.\cr}

    \item{\kbd{\sQuote{Two Stage}}}{ An adaptive procedure. First estimating \code{m0} using a conservative linear step-up and then using it to increase the power of a second step-up procedure. In the case of independent test statistics, this procedure has been shown to control the \acronym{FDR}.\cr}

    \item{\kbd{\sQuote{Multiple Step Down}}}{ A non-linear step-down procedure. Enjoys more power then most procedures and like the \kbd{\sQuote{Two Stage}} procedure, has been proven to control the \acronym{FDR} in the indepenent-test-statistics case.\cr}
    }
}

\value{
  \item{method }{The procedure used.}
  \item{q }{The FDR level controlled for.}
  \item{Cutoff }{A list containing the p-value cutoff and it's index.}
  \item{Pvals }{A data frame containing the original p-values, the critical value   each has been compared to, an indicator for the rejection of each   hypothesis and the procedure specific adjusted p-values.}
}
\references{
    \emph{The control of the false discovery rate in multiple testing under dependancy}\cr Y. Benjamini, D. Yekutieli (The Annals of Statistics 2001).\cr
    \emph{Adaptive linear step-up procedures that control the false discovery rate.} \cr Y. Benjamini, A.M. Krieger, D. Yekutieli (Biometrika 2006).\cr
    \emph{Operating characteristics and extensions of the false discovery rate procedure}\cr  C. Genovese, L. Wasserman (Journal of the Royal Statistical Society 2002).\cr
    
}
\author{ Jonathan Rosenblatt.  }

\note{
    One should give special attention to the dependancy structure between the test statistics.
    In the case of independendcy- the \code{Two Stage} and \code{Multiple Step Down} procedure guarantee good power while controlling the \acronym{FDR}. In case of uncertainty, better use the \code{BH} procedure which will control the FDR in most practical circumbstances (while having less power then the previously suggested procedures).
}

\seealso{ 
    \url{http://www.math.tau.ac.il/~ybenja/fdr/index.htm}
    \url{http://strimmerlab.org/notes/fdr.html}
}

\examples{
test=runif(100)^3

#Linear Step up
bh.1=fdr(x=test,q=0.1,'BH')
plot(bh.1)
summary(bh.1)
table(bh.1$Pvals[['adjusted.pvals']]<=bh.1$q)

# General Dependancy structure
general.1=fdr(x=test,q=0.1,method='General Dependency')
plot(general.1)
summary(general.1)

# New Two Stage procedure
twostage.1=fdr(x=test,0.1,'Two Stage')
plot(twostage.1)
summary(twostage.1)

# If m0 is somehow known
oracle.1=fdr(x=test,q=0.1,'Oracle',m0=30)
plot(oracle.1);summary(oracle.1)

#Adaptive procedure with Story's m0 estimator
adaptive.st.1=fdr(x=test,q=0.1,'ST Adaptive')
plot(adaptive.st.1)
summary(adaptive.st.1)

#Adaptive procedure with Benjamini and Hochberg's m0 estimator
adaptive.bh.1=fdr(x=test,0.1,'BH Adaptive')
plot(adaptive.bh.1)
summary(adaptive.bh.1)

# (Non Linear) Step down procedure
multiple.1=fdr(x=test,q=0.1,'Multiple Step Down')
plot(multiple.1)
summary(multiple.1)
}

\keyword{htest}

\concept{false discovery rate}
\concept{linear step up}
\concept{step down}
\concept{family wise error}
\concept{multiple hypothesis}
\concept{benjamini hochberg}
