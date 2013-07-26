\docType{data}
\name{geom_funnel}
\alias{GeomFunnel}
\alias{geom_funnel}
\title{Add funnel plot boundaries.}
\format{A \code{GeomFunnel} \code{\link{proto::proto}} object.}
\usage{
  geom_funnel(mapping = NULL, data = NULL, stat = "funnel",
    position = "identity", show_guide = TRUE,
    show_target = TRUE, show_ribbon = TRUE,
    show_curves = FALSE, ...)

  GeomFunnel
}
\arguments{
  \item{show_target}{If \code{TRUE} (default), draw a
  \code{\link{geom_hline}} at the target value.}

  \item{show_guide}{Should a legend be drawn if an
  aesthetic is used? (defaults to \code{TRUE})}

  \item{mapping}{The aesthetic mapping, usually constructed
  with \code{\link{aes}} or \code{\link{aes_string}}. Only
  needs to be set at the layer level if you are overriding
  the plot defaults.}

  \item{data}{A layer specific dataset - only needed if you
  want to override the plot defaults.}

  \item{stat}{The statistical transformation to use on the
  data for this layer.}

  \item{position}{The position adjustment to use for
  overlappling points on this layer}

  \item{...}{other arguments passed on to
  \code{\link{layer}}. This can include aesthetics whose
  values you want to set, not map. See \code{\link{layer}}
  for more details.}
}
\description{
  Add funnel plot boundaries.
}
\section{Aesthetics}{
  todo
}
\examples{
# Some examples
}
\seealso{
  The corresponding stat for this geom is
  \code{\link{stat_funnel}}; see that documentation for
  more options to control the underlying statistical
  transformation.
}
\keyword{datasets}

