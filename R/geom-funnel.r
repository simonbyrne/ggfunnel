#' Add funnel plot boundaries.
#'
#' @section Aesthetics: 
#' todo
#'
#' @inheritParams ggplot2::geom_ribbon
#' @param show_target If \code{TRUE} (default), draw a
#' \code{\link{geom_hline}} at the target value.
#' @param show_guide Should a legend be drawn if an aesthetic is used?
#' (defaults to \code{TRUE})
#' 
#' @seealso The corresponding stat for this geom is \code{\link{stat_funnel}}; see 
#'   that documentation for more options to control the underlying statistical transformation.
#' @export
#' 
#' @examples
#' # Poisson rate data: scale is deaths per 100000
#' data(bowelcancer)
#' p <- ggplot(bowelcancer,aes(population,rate))
#' p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000)) + geom_point()
#' # show 95% and 99.8% limits
#' p + stat_funnel(qfn=qfunnel_rate,qargs=c(scale=100000),
#'   limit=c(0.95,0.998),aes(linestyle=..limit..)) + geom_point()
#'
geom_funnel <- function (mapping = NULL, data = NULL, stat = "funnel", position = "identity", show_guide = TRUE, show_target = TRUE, show_ribbon = TRUE, show_curves = FALSE, ...) { 
  GeomFunnel$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, show_target = show_target, show_ribbon = show_ribbon, show_curves = show_curves, ...)
}

#' @format A \code{GeomFunnel} \code{\link{proto}} object.
#' @rdname geom_funnel
#' @export
GeomFunnel <- proto(ggplot2:::Geom, {  
  objname <- "funnel"

  default_stat <- function(.) StatFunnel
  default_aes <- function(.) defaults(aes(colour="#3366FF",alpha=0.5), GeomRibbon$default_aes())
  guide_geom <- function(.) "polygon"

  draw <- function(., data, scales, coordinates,  show_target = TRUE, show_ribbon = TRUE, show_curves = FALSE,...) {

      if (show_target) {
          hlinedata <- data[1,!(names(data) %in% c("x","ymin","ymax"))]
          hlinedata$yend <- hlinedata$y
          hlinedata$alpha <- NA
          hlinedata$fill <- NA
      }
      
      if (show_ribbon) {
          ribbondata <- data[,!(names(data) %in% c("y","yend"))]
          ribbondata$colour <- NA
          ribbondata$group <- interaction(ribbondata$group,ribbondata$limitgroup)
      }

      if (show_curves) {
          pathdata <- ddply(data,"limit",function(d) {
              a <- rename(d[,!(names(d) %in% c("y","ymax"))], c("ymin" = "y"))
              b <- d[1,!(names(d) %in% c("ymin","ymax"))]
              b$y <- NA
              c <- rename(d[,!(names(d) %in% c("y","ymin"))], c("ymax" = "y"))
              rbind(a,b,c)
          })
          pathdata$fill <- NA
          pathdata$group <- interaction(pathdata$group,pathdata$limitgroup)
      }
      
      gList(
          if (show_ribbon) ggplot2:::GeomRibbon$draw_groups(ribbondata,scales,coordinates),
          if (show_curves) ggplot2:::GeomPath$draw_groups(pathdata,scales,coordinates),
          if (show_target) ggplot2:::GeomHline$draw(hlinedata,scales,coordinates)
          )
  }

  
})

