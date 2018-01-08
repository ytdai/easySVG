#' Generate polyline SVG element
#'
#' This function can generate a polyline form SVG element
#' The <polyline> SVG element is an SVG basic shape that creates straight
#' lines connecting several points. Typically a polyline is used to create
#' open shapes as the last point doesn't have to be connected to the first
#' point. For closed shapes see the <polygon> element.
#'
#' @param points a matrix, a series of coordinates
#' @param fill a character, color of the polyline, eg. "#000000"(default), "red"
#' @param stroke a characher, color of the polyline line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the polyline line, default: 1
#' @param stroke.opacity a number, stroke opacity of the polyline line, default:1. If the stroke opacity is 0, the polygon line is invisible
#' @param style.sheet a vector or a chatacter, other style of the polyline, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' points <- matrix(c(1,2,3, 11,12,13), nrow = 3, ncol = 2)
#' polyline.svg(points = points)
#' polyline.svg(points = points, stroke = "yellow")
#'
#'
polyline.svg <- function(points = NULL,
                         fill,
                         stroke,
                         stroke.width,
                         stroke.opacity,
                         style.sheet = NULL) {
  if (is.null(points)) {
    stop("[ERROR] Basic line elements are required (points)!")
  } else {
    if (!is.matrix(points)) {
      points <- as.matrix(points)
    }
  }
  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
  } else {
    style.sheet.ele <- ""
  }
  if (!missing(fill)) {
    style.sheet.ele <- paste0(style.sheet.ele, "fill:", fill, ";")
  }
  if (!missing(stroke)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke:", stroke, ";")
  }
  if (!missing(stroke.width)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke-width:", stroke.width, ";")
  }
  if (!missing(stroke.opacity)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke-opacity:", stroke.opacity, ";")
  }
  if (style.sheet.ele != "") {
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  }

  points.ele <- lapply(1:nrow(points), function(x) { paste(points[x, 1], points[x, 2], sep = " ") })
  points.ele <- paste(points.ele, collapse = " , ")

  polyline.svg.ele <- sprintf('<polyline points="%s" %s />', points.ele, style.sheet.ele)
  return(polyline.svg.ele)
}
