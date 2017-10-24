#' Generate rectangle SVG element
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
#' @return the characher type of svg element
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
                     fill = "#000000",
                     fill.opacity = 1,
                     stroke = "#000000",
                     stroke.width = 1,
                     stroke.opacity = 1,
                     stroke.dasharray = NULL
                     ) {
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
  if (!is.null(stroke.dasharray)) {
    stroke.dasharray <- paste0("stroke-dasharray:", paste(stroke.dasharray, collapse = " "), ";")
  }
  style.element <- paste0("fill:", fill, ";",
                          "fill-opacity:", fill.opacity, ";",
                          "stroke:", stroke, ";",
                          "stroke-width:", stroke.width, ";",
                          "stroke-opacity:", stroke.opacity, ";",
                          stroke.dasharray)

  rect.svg.ele <- sprintf('<rect x="%s" y="%s" width="%s" height="%s" %s %s style="%s" />', x, y, width, height, rx.ele, ry.ele, style.element)
  return(rect.svg.ele)
}








