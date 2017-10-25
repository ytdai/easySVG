#' Generate polygon SVG element
#'
#' @param points a matrix, a series of coordinates
#' @param fill a character, color of the polygon, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the polygon, default:1. If the fill opacity is 0, the polygon's internal color is invisible
#' @param stroke a characher, color of the polygon line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the polygon line, default: 1
#' @param stroke.opacity a number, stroke opacity of the polygon line, default:1. If the stroke opacity is 0, the polygon line is invisible
#' @param fill.rule a character, fill rule of polygon, eg. "nonzero", "evenodd"
#' @param style.sheet a vector or a chatacter, other style of the polygon, eg. "stroke-linecap: round"
#' @return the characher type of svg element
#' @export
#' @examples
#' points <- matrix(c(1,2,3, 11,12,13), nrow = 3, ncol = 2)
#' polygon.svg(points = points)
#' polygon.svg(points = points, fill = "red", stroke = "yellow", fill.rule = "evenodd")
#'
#'
polygon.svg <- function(points = NULL,
                        fill = "#000000",
                        fill.opacity = 1,
                        stroke = "#000000",
                        stroke.width = 1,
                        stroke.opacity = 1,
                        fill.rule = NULL,
                        style.sheet = NULL) {
  if (is.null(points)) {
    stop("[ERROR] Basic line elements are required (points)!")
  } else {
    if (!is.matrix(points)) {
      points <- as.matrix(points)
    }
  }
  if (!is.null(fill.rule)) {
    fill.rule.ele <- paste0("fill-rule:", fill.rule, ";")
  } else {
    fill.rule.ele <- ""
  }
  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
  } else {
    style.sheet.ele <- ""
  }

  points.ele <- lapply(1:nrow(points), function(x) { paste(points[x, 1], points[x, 2], sep = ",") })
  points.ele <- paste(points.ele, collapse = " ")
  style.element <- paste0(fill.rule.ele,
                          "fill:", fill, ";",
                          "fill-opacity:", fill.opacity, ";",
                          "stroke:", stroke, ";",
                          "stroke-width:", stroke.width, ";",
                          "stroke-opacity:", stroke.opacity, ";",
                          style.sheet.ele)
  polygon.svg.ele <- sprintf('<polygon points="%s" style="%s" />', points.ele, style.element)
  return(polygon.svg.ele)
}
