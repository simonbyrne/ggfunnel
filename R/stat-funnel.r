stat_funnel <- function (mapping = NULL, data = NULL, geom = "funnel", position = "identity", 
ytarget = NULL, n = 101, limits = c(0.95), qfn = qfunnel_prop, args = list(), ...) { 
  StatFunnel$new(mapping = mapping, data = data, geom = geom, 
  position = position, ytarget = ytarget, n = n, limits = limits, qfn = qfn, args = args, ...)
}

StatFunnel <- proto(ggplot2:::Stat, {
  objname <- "funnel"

  default_geom <- function(.) GeomFunnel
  default_aes <- function(.) aes()
  
  calculate <- function(., data, scales, ytarget = NULL, n=101, limits=c(0.95), qfn = qfunnel_prop, args = list(), ...) {
      
    xrange <- scale_dimension(scales$x, c(0, 0))
    xseq <- seq(xrange[1], xrange[2], length=n)    
    data <- data.frame(
        x=xseq,
        ymin=qfn(0.5*(1-limits),xseq,ytarget,lower.tail=TRUE),
        ymax=qfn(0.5*(1+limits),xseq,ytarget,lower.tail=FALSE)
        )
    data
  }  
})

# funnel limits for a binomial proportion
# based on a normal approximation, with a Cornish-Fisher skewness correction
qfunnel_prop <- function(p, prec, target, lower.tail=TRUE) {
    z <- qnorm(p)
    target + z*sqrt(target*(1-target)/prec) + (z*z-1)*(1-2*target)/(6*prec) + ifelse(lower.tail,-0.5,0.5)/prec
}

# funnel limits for a binomial proportion
# based on the binomial cdf: no smoothing has been applied, so use qfunnel_prop
qfunnel_prop_exact <- function(p, prec, target, lower.tail=TRUE)
    if (lower.tail) {
        qbinom(p,ceiling(prec),target)/prec        
    } else {
        1-qbinom(1-p,ceiling(prec),1-target)/prec
    }
     


