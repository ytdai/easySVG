#' Generate rectangle SVG element
#'
#' This function can generate a rect form SVG element
#' The <rect> element is a basic SVG shape that creates rectangles,
#' defined by their corner's position, their width, and their height.
#' The rectangles may have their corners rounded.
#'
#' @param x a number, x coordinate information
#' @param y a number, y corrdinate information
#' @param width a number, width of the rect
#' @param height a number, height of the rect
#' @param rx a number, x coordinate of rounded rectangle
#' @param ry a number, y coordinate of rounded rectangle
#' @param fill a character, color of the rect, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the rect, default:1. If the fill opacity is 0, the rect's internal color is invisible
#' @param stroke a characher, color of the rect line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the rect line, default: 1
#' @param stroke.opacity a number, stroke opacity of the rect line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted rect line, eg. c(9, 5)
#' @param style.sheet a vector or a chatacter, other style of the rect, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' rect.svg(x = 1, y = 2, width = 10, height = 20, fill = "blue")
#' rect.svg(x = 1, y = 2, width = 10, height = 20, stroke.dasharray = c(9, 5))
#' rect.svg(x = 1, y = 2, width = 10, height = 20, rx = 2, ry = 4, fill = "blue")
#'

rect.svg <- function(x = NULL,
                     y = NULL,
                     width = NULL,
                     height = NULL,
                     rx = NULL,
                     ry = NULL,
                     fill,
                     fill.opacity,
                     stroke,
                     stroke.width,
                     stroke.opacity,
                     stroke.dasharray,
                     style.sheet = NULL) {
  if (is.null(x) | is.null(y) | is.null(width) | is.null(height)) {
    stop("[ERROR] Basic rect elements are required (x, y, width, height)!")
  }
  if (!is.null(rx)) {
    rx.ele <- paste0('rx="', rx, '"')
  } else {
    rx.ele = ""
  }
  if (!is.null(ry)) {
    ry.ele <- paste0('ry="', ry, '"')
  } else {
    ry.ele <- ""
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

  rect.svg.ele <- sprintf('<rect x="%s" y="%s" width="%s" height="%s" %s %s %s/>', x, y, width, height, rx.ele, ry.ele, style.sheet.ele)
  return(rect.svg.ele)
}








