\docType{data}
\name{stat_funnel}
\alias{StatFunnel}
\alias{stat_funnel}
\title{Add funnel plot boundaries.}
\format{proto object 
 $ objname     : chr "funnel" 
 $ default_aes :function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 48 18 48 34 18 34 48 48 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10acfe7c8>  
 $ default_geom:function (.)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 47 19 47 40 19 40 47 47 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10acfe7c8>  
 $ calculate   :function (., data, scales, ytarget = NULL, n = 101, limit = c(0.95),  
    qfn = qfunnel_prop, qargs = c(), xrange = NULL, args = list(),  
    ...)   
  ..- attr(*, "srcref")=Class 'srcref'  atomic [1:8] 50 16 72 1 16 1 50 72 
  .. .. ..- attr(*, "srcfile")=Classes 'srcfilecopy', 'srcfile' <environment: 0x10acfe7c8>  
 parent: proto object 
 .. parent: proto object}
\usage{
  stat_funnel(mapping = NULL, data = NULL, geom = "funnel",
    position = "identity", ytarget = NULL, n = 101,
    limit = c(0.95), qfn = qfunnel_prop, qargs = c(),
    xrange = NULL, args = list(), ...)

  StatFunnel
}
\arguments{
  \item{ytarget}{The target value on the y-axis. If
  \code{NULL} (the default), the mean of the y-axis data
  weighted by the x-axis is used.}

  \item{n}{Number of points to use in drawing the
  boundary.}

  \item{limit}{A vector of the limits to be used in
  computing the boundary (default = \code{c(0.95)}).}

  \item{qfn}{Quantile function used to compute the limits,
  default is the binary proportion \code{qfunnel_prop}.
  This is a function with at least 4 arguments (see
  below).}

  \item{qargs}{Extra arguments to be passed to the function
  in \code{qfn}. binary proportion \code{qfunnel_prop}.
  This is a function with 4 arguments.}

  \item{xrange}{Either a two-element vector giving the
  minimum and maximum x values of the funnel, or
  \code{NULL} (the default), in which case the range of the
  scale is used.}
}
\value{
  New data frame, with additional columns \item{limit}{the
  limit used to draw the interval} \item{limitgroup}{an
  integer labelling the limits in reverse order}
  \item{x}{x's along a grid} \item{y}{the target value}
  \item{ymin}{the lower boundary} \item{ymax}{the upper
  boundary}
}
\description{
  An
}
\section{Aesthetics}{
  todo
}
\examples{
# Poisson rate data: scale is deaths per 100000
p <- ggplot(bowelcancer,aes(population,rate))
p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000)) + geom_point()
# show 95\% and 99.8\% limits
p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000),limit=c(0.95,0.998) + geom_point()
}
\references{
  Spiegelhalter, D. J. (2005), "Funnel plots for comparing
  institutional performance". Statistics in Medicine,
  24(8):1185-1202. doi:
  \href{http://dx.doi.org/}{10.1002/sim.1970}
}
\seealso{
  The corresponding geom for this stat is
  \code{\link{geom_funnel}}; see that documentation for
  more options to control the display.
}
\keyword{datasets}

