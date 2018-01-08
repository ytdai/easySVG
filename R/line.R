#' Generate line SVG element
#'
#' This function will generate a line form SVG element.
#' The <line> element is an SVG basic shape used to create a line connecting two points.
#'
#'
#' @param x1 a number, x1 coordinate information
#' @param y1 a number, y1 corrdinate information
#' @param x2 a number, x2 corrdiante information
#' @param y2 a number, y2 corrdinate information
#' @param stroke a characher, color of the line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the line, default: 1
#' @param stroke.opacity a number, stroke opacity of the line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted line, eg. c(9, 5)
#' @param style.sheet a vector or a chatacter, other style of the line, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' line.svg(x1 = 1, y1 = 2, x2 = 10, y2 = 20)
#' line.svg(x1 = 1, y1 = 2, x2 = 10, y2 = 20, stroke = "#00FF00")
#' line.svg(x1 = 1, y1 = 2, x2 = 10, y2 = 20, stroke.dasharray = c(9, 5))
#'
#'

line.svg <- function(x1 = NULL,
                     y1 = NULL,
                     x2 = NULL,
                     y2 = NULL,
                     stroke,
                     stroke.width,
                     stroke.opacity,
                     stroke.dasharray,
                     style.sheet = NULL) {
  if (is.null(x1) | is.null(y1) | is.null(x2) | is.null(y2)) {
    stop("[ERROR] Basic line elements are required (x1, y1, x2, y2)!")
  }
  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
  } else {
    style.sheet.ele <- ""
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
    style.sheet.ele <- paste0(style.sheet.ele, "stroke-dasharray:", paste(stroke.dasharray, collapse = ","), ";")
  }
  if (style.sheet.ele != "") {
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  }
  line.svg.ele <- sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" %s/>', x1, y1, x2, y2, style.sheet.ele)
  return(line.svg.ele)
}





