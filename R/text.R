#' Generate text SVG element
#'
#' This function can generate a text form SVG element
#' The SVG <text> element defines a graphics element consisting of text.
#' It's possible to apply a gradient, pattern, clipping path, mask, or
#' filter to <text>, just like any other SVG graphics element.
#'
#' @param x a number, x coordinate information
#' @param y a number, y corrdinate information
#' @param text.content a character, text content
#' @param fill a character, color of the text, eg. "#000000"(default), "red"
#' @param stroke a characher, color of the rect text, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the rect text, default: 1
#' @param font.family a character, font family of text, eg. "Arial"
#' @param font.size a number, font size of text, default: 8
#' @param font.weight a character, font weight of text, eg. "normal"(default), "bold"
#' @param font.style a character, font style of text, eg. "normal"(default), "italic"
#' @param text.decoration a character, text decoration, eg. "none"(default), "underline", "overline", "line-through"
#' @param word.spacing a number or character, default: "normal"
#' @param letter.spacing a number or character, defailt: "normal"
#' @param text.anchor a character, eg. "start"(default), "middle", "end"
#' @param rotate a number, rotation angle of text
#' @param text.path a character, fit text path
#' @param style.sheet a vector or a chatacter, other style of the text, eg. "stroke-linecap: round"
#' @return the characher type of SVG element
#' @export
#' @examples
#' get.text.svg(x = 10, y = 20, text.content = "Hello Word", fill = "blue")
#' get.text.svg(x = 10, y = 20, text.content = "Hello Word", fill = "blue",
#'              rotate = 90, font.family = "Helvetica")
#'

get.text.svg <- function(x = NULL,
                     y = NULL,
                     text.content = "",
                     fill,
                     stroke,
                     stroke.width,
                     font.family,
                     font.size,
                     font.weight,
                     font.style,
                     text.decoration,
                     word.spacing,
                     letter.spacing,
                     text.anchor,
                     rotate,
                     text.path,
                     style.sheet = NULL) {

  if (is.null(x) | is.null(y)) {
    stop("[ERROR] Basic text elements are required (x, y)!")
  }
  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
  } else {
    style.sheet.ele <- ""
  }
  if (!missing(fill)) {
    style.sheet.ele <- paste0(style.sheet.ele, "fill:", fill, ";")
  }
  if (!missing(stroke)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke:", stroke, ";")
  }
  if (!missing(stroke.width)) {
    style.sheet.ele <- paste0(style.sheet.ele, "stroke-width:", stroke.width, ";")
  }
  if (!missing(font.family)) {
    style.sheet.ele <- paste0(style.sheet.ele, "font-family:", font.family, ";")
  }
  if (!missing(font.size)) {
    style.sheet.ele <- paste0(style.sheet.ele, "font-size:", font.size, ";")
  }
  if (!missing(font.weight)) {
    style.sheet.ele <- paste0(style.sheet.ele, "font-weight:", font.weight, ";")
  }
  if (!missing(font.style)) {
    style.sheet.ele <- paste0(style.sheet.ele, "font-style:", font.style, ";")
  }
  if (!missing(text.decoration)) {
    style.sheet.ele <- paste0(style.sheet.ele, "text-decoration:", text.decoration, ";")
  }
  if (!missing(word.spacing)) {
    style.sheet.ele <- paste0(style.sheet.ele, "word-spacing:", word.spacing, ";")
  }
  if (!missing(letter.spacing)) {
    style.sheet.ele <- paste0(style.sheet.ele, "letter-spacing:", letter.spacing, ";")
  }
  if (!missing(text.anchor)) {
    style.sheet.ele <- paste0(style.sheet.ele, "text-anchor:", text.anchor, ";")
  }
  if (style.sheet.ele != "") {
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  }

  transform.ele <- ""
  if (!missing(rotate)) {
    transform.ele <- paste0(transform.ele, 'rotate(', rotate, ',', x, ',', y, ') ')
  }
  if (transform.ele != "") {
    transform.ele <- paste0('transform="', transform.ele, '"')
  }

  text.svg.ele <- sprintf('<text x="%s" y="%s" %s %s>%s</text>', x, y, style.sheet.ele, transform.ele, text.content)

  if (!missing(text.path)) {
    text.svg.ele <- sprintf('<text x="%s" y="%s" %s %s><textPath xlink:href="%s">%s</textPath></text>',
                            x, y,  style.sheet.ele, transform.ele, text.path, text.content)
  }

  return(text.svg.ele)

}
