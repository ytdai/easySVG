#' Generate polyline SVG element
#'
#' @param x a number, x coordinate information
#' @param y a number, y corrdinate information
#' @param ry a number, y coordinate of rounded rectangle
#' @param fill a character, color of the text, eg. "#000000"(default), "red"
#' @param stroke a characher, color of the rect text, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the rect text, default: 1
#' @param font.family a character, font family of text, default: "Arial"
#' @param font.size a number, font size of text, default: 8
#' @param font.weight a character, font weight of text, eg. "normal"(default), "bold"
#' @param font.style a character, font style of text, eg. "normal"(default), "italic"
#' @param text.decoration a character, text decoration, eg. "none"(default), "underline", "overline", "line-through"
#' @param word.spacing a number or character, default: "normal"
#' @param letter.spacing a number or character, defailt: "normal"
#' @param text.anchor a character, eg. "start"(default), "middle", "end"
#' @param rotate a number, rotation angle of text
#' @param text.path a character, fit text path
#' @return the characher type of svg element
#' @export
#' @examples
#' text.svg(x = 10, y = 20, content = "Hello Word", fill = "blue")
#' text.svg(x = 10, y = 20, content = "Hello Word", fill = "blue", rotate = 90, font.family = "Helvetica")
#'

text.svg <- function(x = NULL,
                     y = NULL,
                     content = "",
                     fill = "#000000",
                     stroke = "none",
                     stroke.width = 1,
                     font.family = "Arial",
                     font.size = 8,
                     font.weight = "normal",
                     font.style = "normal",
                     text.decoration = "none",
                     word.spacing = "normal",
                     letter.spacing = "normal",
                     text.anchor = "start",
                     rotate = NULL,
                     text.path = NULL) {

  if (is.null(x) | is.null(y)) {
    stop("[ERROR] Basic text elements are required (x, y)!")
  }
  if (fill != "#000000") {
    fill.ele <- paste0("fill:", fill, ";")
  } else {
    fill.ele = ""
  }
  if (stroke != "none") {
    stroke.ele <- paste0("stroke:", stroke, ";", "stroke-width:", stroke.width, ";")
  } else {
    stroke.ele = ""
  }
  if (font.weight != "normal") {
    font.weight.ele <- paste0("font-weight", font.weight, ";")
  } else {
    font.weight.ele = ""
  }
  if (font.style != "normal") {
    font.style.ele <- paste0("font-style:", font.style, ";")
  } else {
    font.style.ele <- ""
  }
  if (text.decoration != "none") {
    text.decoration.ele <- paste0("text-decoration:", text.decoration, ";")
  } else {
    text.decoration.ele<- ""
  }
  if (word.spacing != "normal") {
    word.spacing.ele <- paste0("word-spacing:", word.spacing, ";")
  } else {
    word.spacing.ele <- ""
  }
  if (letter.spacing != "normal") {
    letter.spacing.ele <- paste0("letter-spacing:", letter.spacing, ";")
  } else {
    letter.spacing.ele <- ""
  }
  if (!is.null(rotate)) {
    rotate.ele <- paste0('transform="rotate(', rotate, ',', x, ',', y, ')"')
  } else {
    rotate.ele <- ""
  }

  font.family.ele <- paste0("font-family:", font.family, ";")

  style.element <- paste0(font.family.ele, fill.ele, stroke.ele, font.weight.ele,
                          font.style.ele, text.decoration.ele,
                          word.spacing.ele, letter.spacing.ele)

  text.svg.ele <- sprintf('<text x="%s" y="%s" style="%s" %s>%s</text>', x, y, style.element, rotate.ele, content)

  if (!is.null(text.path)) {
    text.svg.ele <- sprintf('<text x="%s" y="%s" style="%s" %s><textPath xlink:href="%s">%s</textPath></text>',
                            x, y, style.element, rotate.ele, text.path, content)
  }

  return(text.svg.ele)

}
