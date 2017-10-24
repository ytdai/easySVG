#' Generate line SVG element
#'
#' @param x1 a number, x1 coordinate information
#' @param y1 a number, y1 corrdinate information
#' @param x2 a number, x2 corrdiante information
#' @param y2 a number, y2 corrdinate information
#' @param stroke a characher, color of the line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the line, default: 1
#' @param stroke.opacity a number, stroke opacity of the line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted line, eg. c(9, 5)
#' @return the characher type of svg element
#' @export
#' @examples
#' line.svg(x1 = 1, y1 = 2, x2 = 10, y2 = 20)
#' line.svg(x1 = 1, y1 = 2, x2 = 10, y2 = 20, stroke.dasharray = c(9, 5))
#'
#'

line.svg <- function(x1 = NULL,
                     y1 = NULL,
                     x2 = NULL,
                     y2 = NULL,
                     stroke = "#000000",
                     stroke.width = 1,
                     stroke.opacity = 1,
                     stroke.dasharray = NULL) {
  if (is.null(x1) | is.null(y1) | is.null(x2) | is.null(y2)) {
    stop("[ERROR] Basic line elements are required (x1, y1, x2, y2)!")
  }
  if (!is.null(stroke.dasharray)) {
    stroke.dasharray <- paste0("stroke-dasharray:", paste(stroke.dasharray, collapse = ","), ";")
  }
  style.element <- paste0("stroke:", stroke, ";",
                         "stroke-width:", stroke.width, ";",
                         "stroke-opacity:", stroke.opacity, ";",
                         stroke.dasharray)
  line.svg.ele <- sprintf('<line x1="%s" y1="%s" x2="%s" y2="%s" style="%s" />', x1, y1, x2, y2, style.element)
  return(line.svg.ele)
}





