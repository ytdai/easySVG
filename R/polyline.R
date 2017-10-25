#' Generate polyline SVG element
#'
#' @param points a matrix, a series of coordinates
#' @param fill a character, color of the polyline, eg. "#000000"(default), "red"
#' @param stroke a characher, color of the polyline line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the polyline line, default: 1
#' @param stroke.opacity a number, stroke opacity of the polyline line, default:1. If the stroke opacity is 0, the polygon line is invisible
#' @param style.sheet a vector or a chatacter, other style of the polyline, eg. "stroke-linecap: round"
#' @return the characher type of svg element
#' @export
#' @examples
#' points <- matrix(c(1,2,3, 11,12,13), nrow = 3, ncol = 2)
#' polyline.svg(points = points)
#' polyline.svg(points = points, stroke = "yellow")
#'
#'
polyline.svg <- function(points = NULL,
                         fill = "none",
                         stroke = "#000000",
                         stroke.width = 1,
                         stroke.opacity = 1,
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

  points.ele <- lapply(1:nrow(points), function(x) { paste(points[x, 1], points[x, 2], sep = " ") })
  points.ele <- paste(points.ele, collapse = " , ")
  style.element <- paste0("fill:", fill, ";",
                          "stroke:", stroke, ";",
                          "stroke-width:", stroke.width, ";",
                          "stroke-opacity:", stroke.opacity, ";",
                          style.sheet.ele)
  polyline.svg.ele <- sprintf('<polyline points="%s" style="%s" />', points.ele, style.element)
  return(polyline.svg.ele)
}
