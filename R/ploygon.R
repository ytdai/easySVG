#' Generate polygon SVG element
#'
#' This function can generate a polygon form SVG element
#' The <polygon> element defines a closed shape consisting of a set of
#' connected straight line segments. The last point is connected to the
#' first point. For open shapes see the <polyline> element.
#'
#' @param points a matrix, a series of coordinates
#' @param fill a character, color of the polygon, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the polygon, default:1. If the fill opacity is 0, the polygon's internal color is invisible
#' @param stroke a characher, color of the polygon line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the polygon line, default: 1
#' @param stroke.opacity a number, stroke opacity of the polygon line, default:1. If the stroke opacity is 0, the polygon line is invisible
#' @param fill.rule a character, fill rule of polygon, eg. "nonzero", "evenodd"
#' @param style.sheet a vector or a chatacter, other style of the polygon, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' points <- matrix(c(1,2,3, 11,12,13), nrow = 3, ncol = 2)
#' polygon.svg(points = points)
#' polygon.svg(points = points, fill = "red", stroke = "yellow", fill.rule = "evenodd")
#'
#'
polygon.svg <- function(points = NULL,
                        fill,
                        fill.opacity,
                        stroke,
                        stroke.width,
                        stroke.opacity,
                        fill.rule,
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
  if (!missing(fill.opacity)) {
    style.sheet.ele <- paste0(style.sheet.ele, "fill-opacity:", fill.opacity, ";")
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
  if (!missing(fill.rule)) {
    style.sheet.ele <- paste0(style.sheet.ele, "fill-rule", fill.rule, ";")
  }
  if (style.sheet.ele != "") {
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  }

  points.ele <- lapply(1:nrow(points), function(x) { paste(points[x, 1], points[x, 2], sep = ",") })
  points.ele <- paste(points.ele, collapse = " ")

  polygon.svg.ele <- sprintf('<polygon points="%s" %s />', points.ele, style.sheet.ele)
  return(polygon.svg.ele)
}
