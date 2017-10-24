#' Generate ellipse SVG element
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
#' @return the characher type of svg element
#' @examples
#' ellipse.svg(cx = 10, cy = 20, rx = 10, ry = 5, fill = "blue")
#' ellipse.svg(cx = 10, cy = 20, rx = 10, ry = 5, fill = "blue", stroke.width = 2)
#'

ellipse.svg <- function(cx = NULL,
                        cy = NULL,
                        rx = NULL,
                        ry = NULL,
                        fill = "#000000",
                        fill.opacity = 1,
                        stroke = "#000000",
                        stroke.width = 1,
                        stroke.opacity = 1,
                        stroke.dasharray = NULL
) {
  if (is.null(cx) | is.null(cy) | is.null(rx) | is.null(ry)) {
    stop("[ERROR] Basic ellipse elements are required (cx, cy, rx, ry)!")
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

  ellipse.svg.ele <- sprintf('<ellipse cx="%s" cy="%s" rx="%s" ry="%s" style="%s" />', cx, cy, rx, ry, style.element)
  return(ellipse.svg.ele)
}








