% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/circle.R
\name{circle.svg}
\alias{circle.svg}
\title{Generate circle SVG element}
\usage{
circle.svg(cx = NULL, cy = NULL, r = NULL, fill, fill.opacity, stroke,
  stroke.width, stroke.opacity, stroke.dasharray, style.sheet = NULL)
}
\arguments{
\item{cx}{a number, x coordinate information}

\item{cy}{a number, y corrdinate information}

\item{r}{a number, radius of the circle}

\item{fill}{a character, color of the circle, eg. "#000000"(default), "red"}

\item{fill.opacity}{a number, stroke opacity of the circle, default:1. If the fill opacity is 0, the circle's internal color is invisible}

\item{stroke}{a characher, color of the circle line, eg. "#000000"(default), "red"}

\item{stroke.width}{a number, stroke width of the circle line, default: 1}

\item{stroke.opacity}{a number, stroke opacity of the circle line, default:1. If the stroke opacity is 0, the line is invisible}

\item{stroke.dasharray}{a vector, plot the dotted circle line, eg. c(9, 5)}

\item{style.sheet}{a vector or a chatacter, other style of the circle, eg. "stroke-linecap: round"}
}
\value{
the characher type of SVG element
}
\description{
This function can generate a circle form SVG element
}
\details{
The <circle> SVG element is an SVG basic shape, used to create circles based on a center point and a radius.
}
\examples{
circle.svg(cx = 10, cy = 20, r = 10, fill = "blue")
circle.svg(cx = 10, cy = 20, r = 10, fill = "blue", stroke.width = 2)

}
