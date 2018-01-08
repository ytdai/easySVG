#' make svg group
#'
#' The <g> SVG element is a container used to group other SVG elements.
#' Transformations applied to the <g> element are performed on all of
#' its child elements, and any of its attributes are inherited by its
#' child elements. It can also group multiple elements to be referenced
#' later with the <use> element.
#'
#' @param id a character, group id
#' @param group.content a character or a list or a vector, group content
#' @param fill a character, color of the group, eg. "#000000"(default), "red"
#' @param fill.opacity a number, stroke opacity of the group, default:1. If the fill opacity is 0, the rect's internal color is invisible
#' @param stroke a characher, color of the group line, eg. "#000000"(default), "red"
#' @param stroke.width a number, stroke width of the group line, default: 1
#' @param stroke.opacity a number, stroke opacity of the group line, default:1. If the stroke opacity is 0, the line is invisible
#' @param stroke.dasharray a vector, plot the dotted group line, eg. c(9, 5)
#' @param font.family a character, font family of text, eg. "Arial"
#' @param font.size a number, font size of text, default: 8
#' @param font.weight a character, font weight of text, eg. "normal"(default), "bold"
#' @param font.style a character, font style of text, eg. "normal"(default), "italic"
#' @param text.decoration a character, text decoration, eg. "none"(default), "underline", "overline", "line-through"
#' @param word.spacing a number or character, default: "normal"
#' @param letter.spacing a number or character, defailt: "normal"
#' @param text.anchor a character, eg. "start"(default), "middle", "end"
#' @param scale a number. transform scale of the object
#' @param rotate a vector, rotation of the object
#' @param translate a vector, translate of the object
#' @param skewX a number
#' @param skewY a number
#' @param style.sheet a vector or a chatacter, other style of the group, eg. "stroke-linecap: round"
#' @param transform.sheet a vector or a chatacter, other transform of the group
#' @return the characher type of SVG element
#' @export
#' @examples
#' group.svg(id = "group_1", group.content = "this is a svg element")
#' group.content <- list(svg1 = "this is a svg element",
#'                       svg2 = "this is a svg element")
#' group.svg(id = "group_1", group.content = group.content)
#' group.svg(id = "group_1", group.content = group.content,
#'           style.sheet = c("stroke:red", "stroke-width:1"),
#'           transform.sheet = c("translate(100, 100)"))
#'

group.svg <- function(id = NULL,
                      group.content = NULL,
                      fill,
                      fill.opacity,
                      stroke,
                      stroke.width,
                      stroke.opacity,
                      stroke.dasharray,
                      font.family,
                      font.size,
                      font.weight,
                      font.style,
                      text.decoration,
                      word.spacing,
                      letter.spacing,
                      text.anchor,

                      scale,
                      rotate,
                      translate,

                      skewX,
                      skewY,
                      style.sheet = NULL,
                      transform.sheet = NULL) {
  if (!is.null(id)) {
    id.ele <- paste0('id="', id, '"')
  }
  if (!is.null(group.content)) {
    if (is.list(group.content)) {
      group.content <- unlist(group.content)
      group.content <- paste(group.content, collapse = "\n")
    } else if (is.vector(group.content)) {
      group.content <- paste(group.content, collapse = "\n")
    } else {
      group.content <- paste(group.content, sep = "\n")
    }
    group.content.ele <- paste(group.content, collapse = "\n")
  } else {
    group.content.ele <- ""
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


  if (!is.null(transform.sheet)) {
    transform.ele <- paste(transform.sheet, collapse = " ")
  } else {
    transform.ele <- ""
  }
  if (!missing(scale)) {
    if (length(scale) == 1) {
      scale.ele <- paste0('scale(', scale, ')')
    } else {
      scale.ele <- paste0('scale(', scale[1], ',', scale[2], ')')
    }
    transform.ele <- paste0(transform.ele, scale.ele, " ")
  }
  if (!missing(rotate)) {
    if (length(rotate) < 1) {
      rotate.ele <- paste0('rotate(', rotate[1], ')')
    } else {
      rotate.ele <- paste0('rotate(', rotate[1], ',', rotate[2], ',', rotate[3], ')')
    }
    transform.ele <- paste0(transform.ele, rotate.ele, " ")
  }
  if (!missing(translate)) {
    transform.ele <- paste0(transform.ele, 'translate(', translate[1], ',', translate[2], ')', " ")
  }
  if (!missing(skewX)) {
    transform.ele <- paste0(transform.ele, 'skewX(', skewX, ')', " ")
  }
  if (!missing(skewY)) {
    transform.ele <- paste0(transform.ele, 'skewX(', skewY, ')', " ")
  }
  if (transform.ele != "") {
    transform.ele <- paste0('transform="', transform.ele, '"')
  }

  group.svg.ele <- sprintf('<g %s %s %s>\n%s</g>\n', id.ele, style.sheet.ele, transform.ele, group.content.ele)
  return(group.svg.ele)
}

