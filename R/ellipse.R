#' Generate ellipse SVG element
#'
#' This function can generate a ellipse form of SVG element
#' The ellipse element is an SVG basic shape, used to create ellipses
#' based on a center coordinate, and both their x and y radius.
#'
#' @param cx a number, x coordinate information
#' @param cy a number, y corrdinate information
#' @param rx a number, x radius of the ellipse
#' @param ry a number, y radius of the ellipse
#' @param fill a character, color of the ellipse, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the ellipse, default:1. If the fill opacity is 0, the ellipse's internal color is invisible
#' @param stroke a characher, color of the ellipse line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the ellipse line, default: 1
#' @param stroke.opacity a number, stroke opacity of the ellipse line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted ellipse line, eg. c(9, 5)
#' @param style.sheet a vector or a chatacter, other style of the ellipse, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' ellipse.svg(cx = 10, cy = 20, rx = 10, ry = 5, fill = "blue")
#' ellipse.svg(cx = 10, cy = 20, rx = 10, ry = 5, fill = "blue", stroke.width = 2)
#'

ellipse.svg <- function(cx = NULL,
                        cy = NULL,
                        rx = NULL,
                        ry = NULL,
                        fill,
                        fill.opacity,
                        stroke,
                        stroke.width,
                        stroke.opacity,
                        stroke.dasharray,
                        style.sheet = NULL) {
  if (is.null(cx) | is.null(cy) | is.null(rx) | is.null(ry)) {
    stop("[ERROR] Basic ellipse elements are required (cx, cy, rx, ry)!")
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
  if (!missing(stroke.dasharray)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke-dasharray:", paste(stroke.dasharray, collapse = " "), ";")
  }
  if (style.sheet.ele != "") {
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  }

  ellipse.svg.ele <- sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" %s />', cx, cy, rx, ry, style.sheet.ele)
  return(ellipse.svg.ele)
}








