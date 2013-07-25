geom_funnel <- function (mapping = NULL, data = NULL, stat = "funnel", position = "identity", show_guide = FALSE, ...) { 
  GeomFunnel$new(mapping = mapping, data = data, stat = stat, position = position, show_guide = show_guide, ...)
}

GeomFunnel <- proto(ggplot2:::GeomRibbon, {  
  objname <- "funnel"

  default_stat <- function(.) StatFunnel
  default_aes <- function(.) defaults(aes(alpha=0.5), GeomRibbon$default_aes())
  guide_geom <- function(.) "polygon"
})

