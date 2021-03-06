% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorbar.R
\name{colorbar}
\alias{colorbar}
\alias{colorplot}
\title{Display a palette, simulate colorblindness and greyscale, plot Hue Chroma and Lightness}
\usage{
colorbar(palette, dots = FALSE, colorblind = FALSE)

colorplot(palette, dots = FALSE, colorblind = FALSE)
}
\arguments{
\item{palette}{Character vector. A palette (vector of hex RGB or RGBA colors).}

\item{dots}{Logical. If \code{dots=TRUE}, a row of circles (dots) will be drawn instead of a colorbar.}

\item{colorblind}{Logical. If \code{colorblind=TRUE}, colorbars (or dots) for three common types of
colorblindness (deuteranopia, protanopia and tritanopia) and greyscale will also be displayed.}
}
\description{
\code{colorbar()} displays a palette as a horizontal colorbar. \code{colorplot()} displays a palette as a
horizontal colorbar and plots its Hue (H), Chroma (C) and Lightness (L). HCL is a perceptually
uniform colorspace.
}
\details{
Both functions can also simulate greyscale printing and three forms of colorblindness.

The input palette must be a character vector of colors as 'hex' RGB or RGBA strings
(e.g."#006633" or "#006633FF"), such as produced by most color functions in R.

NA's may occur in a palette if it was produced with \code{\link[grDevices:hcl]{grDevices::hcl()}} using \code{fixup=FALSE}. NA's
produced by an HCL function are out-of-gamut colors (not possible to display in RGB space). NA's
are displayed as white gaps in the colorbar and black verticals in the plots of H, C and L.

Although the HCL colorspace is often used to create perceptually uniform color palettes, these
palettes are converted to RGB colorspace for display on screen, which introduces artifacts
arising from the conversion from high precision (float) to the 8-bit precision of RGB. Since
\code{colorplot()} analyzes the palette \emph{after} it has been converted to RGB for display, it
accurately represents the palette as it will be displayed on screen, with any artifacts.
}
\examples{
# Draw a colorbar or row of dots
pal <- grDevices::rainbow(36)
colorbar(pal)
colorbar(pal,dots=TRUE)
colorbar(pal,colorblind=TRUE)
colorbar(pal,dots=TRUE,colorblind=TRUE)

# plot colobar (or dots) and HCL for the palette
colorplot(pal)
colorplot(pal,dots=TRUE)
colorplot(pal,colorblind=TRUE)
colorplot(pal,dots=TRUE,colorblind=TRUE)

}
