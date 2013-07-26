#' Add funnel plot boundaries.
#'
#' An 
#'
#' @section Aesthetics: 
#' todo
#'
#' @param ytarget The target value on the y-axis. If \code{NULL}
#' (the default), the mean of the y-axis data weighted by the x-axis is used.
#' @param n Number of points to use in drawing the boundary.
#' @param limit A vector of the limits to be used in computing the boundary
#' (default = \code{c(0.95)}).
#' @param qfn Quantile function used to compute the limits, default is the
#' binary proportion \code{qfunnel_prop}. This is a function with at least 4 arguments (see below).
#' @param qargs Extra arguments to be passed to the function in \code{qfn}.
#' binary proportion \code{qfunnel_prop}. This is a function with 4 arguments.
#' @param xrange Either a two-element vector giving the minimum and maximum x values of the funnel, or \code{NULL} (the default), in which case the range of the scale is used.
#' @return New data frame, with additional columns
#'   \item{limit}{the limit used to draw the interval}
#'   \item{limitgroup}{an integer labelling the limits in reverse order}
#'   \item{x}{x's along a grid}
#'   \item{y}{the target value}
#'   \item{ymin}{the lower boundary}
#'   \item{ymax}{the upper boundary}
#' 
#' @seealso The corresponding geom for this stat is \code{\link{geom_funnel}};
#' see that documentation for more options to control the display.
#' @references Spiegelhalter, D. J. (2005), "Funnel plots for comparing institutional performance". Statistics in Medicine, 24(8):1185-1202. doi: \href{http://dx.doi.org/}{10.1002/sim.1970}
#' @export
#' @examples
#' # Poisson rate data: scale is deaths per 100000
#' data(bowelcancer)
#' p <- ggplot(bowelcancer,aes(population,rate))
#' p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000)) + geom_point()
#' # show 95% and 99.8% limits
#' p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000),limit=c(0.95,0.998) + geom_point()
stat_funnel <- function (mapping = NULL, data = NULL, geom = "funnel", position = "identity", 
ytarget = NULL, n = 101, limit = c(0.95), qfn = qfunnel_prop, qargs=c(), xrange = NULL, args = list(), ...) { 
  StatFunnel$new(mapping = mapping, data = data, geom = geom, 
  position = position, ytarget = ytarget, n = n, limit = limit, qfn = qfn, qargs = qargs, xrange = xrange, args = args, ...)
}

#' @format A \code{StatFunnel} \code{\link{proto}} object.
#' @rdname stat_funnel
#' @export
StatFunnel <- proto(ggplot2:::Stat, {
  objname <- "funnel"

  default_geom <- function(.) GeomFunnel
  default_aes <- function(.) aes()
  
  calculate <- function(., data, scales, ytarget = NULL, n=101, limit=c(0.95), qfn = qfunnel_prop, qargs = c(), xrange = NULL, args = list(), ...) {

    if (is.null(ytarget))
        ytarget <- weighted.mean(data$y,data$x)

    if (is.null(xrange))
        xrange <- scale_dimension(scales$x, c(0, 0))
    
    nlimit <- length(limit)
    
    xseq <- rep.int(seq(xrange[1], xrange[2], length=n),nlimit)    
    lseq <- rep(limit,each=n)

        
    data.frame(
        y=ytarget,       
        limit=factor(lseq,limit),
        limitgroup=rep(nlimit:1,each=n),
        x=xseq,
        ymin=do.call(qfn,c(list(0.5*(1-lseq),xseq,ytarget,TRUE),qargs)),
        ymax=do.call(qfn,c(list(0.5*(1+lseq),xseq,ytarget,FALSE),qargs))
        )
}

})



