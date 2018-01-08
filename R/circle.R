#' Generate circle SVG element
#'
#' This function can generate a circle form SVG element
#'
#' The <circle> SVG element is an SVG basic shape, used to create circles based on a center point and a radius.
#'
#' @param cx a number, x coordinate information
#' @param cy a number, y corrdinate information
#' @param r a number, radius of the circle
#' @param fill a character, color of the circle, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the circle, default:1. If the fill opacity is 0, the circle's internal color is invisible
#' @param stroke a characher, color of the circle line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the circle line, default: 1
#' @param stroke.opacity a number, stroke opacity of the circle line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted circle line, eg. c(9, 5)
#' @param style.sheet a vector or a chatacter, other style of the circle, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' circle.svg(cx = 10, cy = 20, r = 10, fill = "blue")
#' circle.svg(cx = 10, cy = 20, r = 10, fill = "blue", stroke.width = 2)
#'

circle.svg <- function(cx = NULL,
                       cy = NULL,
                       r = NULL,
                       fill,
                       fill.opacity,
                       stroke,
                       stroke.width,
                       stroke.opacity,
                       stroke.dasharray,
                       style.sheet = NULL) {
  if (is.null(cx) | is.null(cy) | is.null(r)) {
    stop("[ERROR] Basic circle elements are required (cx, cy, r)!")
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


  circle.svg.ele <- sprintf('<circle cx="%s" cy="%s" r="%s" %s />', cx, cy, r, style.sheet.ele)
  return(circle.svg.ele)
}








