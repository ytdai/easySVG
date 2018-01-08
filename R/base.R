#' make SVG defs element
#'
#' @param defs.content a character or a list, group content
#' @return the characher type of SVG element
#' @export
#' @examples
#' defs.svg(defs.content = "<text x=\"10\" y=\"20\"> an SVG element </text>")
#' defs.content <- list(svg1 = "<text x=\"10\" y=\"30\"> an SVG element </text>",
#'                      svg2 = "<text x=\"10\" y=\"40\"> an SVG element </text>")
#' defs.svg(defs.content = defs.content)
#'

defs.svg <- function(defs.content = NULL) {
  if (is.list(defs.content)) {
    defs.content <- paste(defs.content, collapse = "\n")
  }
  defs.svg.ele <- sprintf('<defs>\n%s</defs>\n\n', defs.content)
  return(defs.svg.ele)
}


#' pack svg
#'
#' @param width a number, width of the plot
#' @param height a number, height of the plot
#' @param output.svg.name a character, the output svg file name
#' @param pack.content a character or a list, group content
#' @return the characher type of svg element
#' @export
#' @examples
#' pack.svg(pack.content = "<text x=\"10\" y=\"20\"> this is a svg element </text>")
#' pack.content <- list(svg1 = "<text x=\"10\" y=\"20\"> this is a svg element </text>",
#'                      svg2 = "<text x=\"10\" y=\"40\"> this is a svg element </text>")
#' pack_info <- pack.svg(pack.content = pack.content)
#' message(pack_info)
#'

pack.svg <- function(width = 1200,
                     height = 800,
                     output.svg.name = NULL,
                     pack.content = pack.content) {
  if (is.list(pack.content)) {
    pack.content <- unlist(pack.content)
    pack.content <- paste(pack.content, collapse = "\n")
  } else if (is.vector(pack.content)) {
    pack.content <- paste(pack.content, collapse = "\n")
  } else {
    pack.content <- paste(pack.content, sep = "\n")
  }
  svg_header <- sprintf("<?xml version=\"1.0\" standalone=\"no\"?> \n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> \n\n<svg width=\"%s\" height=\"%s\" version=\"1.1\" \n  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n", width, height)
  svg_footer <- sprintf("\n</svg>")

  pack.svg.ele <- sprintf('%s\n\n%s\n%s\n\n', svg_header, pack.content, svg_footer)

  if (!is.null(output.svg.name)) {
    write(pack.svg.ele, file = output.svg.name, sep = "\n")
  } else {
    return(pack.svg.ele)
  }
}

#' use svg
#'
#' The <use> element takes nodes from within the SVG document, and duplicates them somewhere else.
#'
#' @param id a character, target of the link
#' @param x a number, x transform coordinate
#' @param y a number, y transform coordinate
#' @param scale a number. transform scale of the object
#' @param rotate a vector, rotation of the object
#' @param translate a vector, translate of the object
#' @param skewX a number
#' @param skewY a number
#' @param style.sheet a vector or a chatacter, other style of the link, eg. "stroke-linecap: round"
#' @param transform.sheet a vector or a chatacter, other transform of the link,
#' @return the characher type of svg element
#' @export
#' @examples
#' use.svg(id = "target", x = 100, y = 200)
#' use.svg(id = "target", x = 100, y = 200, rotate = c(90, 100, 200))
#'

use.svg <- function(id = NULL,
                    x = NULL,
                    y = NULL,
                    scale,
                    rotate,
                    translate,
                    skewX,
                    skewY,
                    style.sheet = NULL,
                    transform.sheet = NULL) {
  if (is.null(id)) {
    stop("[ERROR] target object id is reuqired!")
  }
  if (!is.null(x)) {
    x.ele <- paste0('x="', x, '"')
  } else {
    x.ele <- ""
  }
  if (!is.null(y)) {
    y.ele <- paste0('y="', y, '"')
  } else {
    y.ele <- ""
  }

  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
  } else {
    style.sheet.ele <- ""
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


  use.svg.ele <- sprintf('<use xlink:href="#%s" %s %s %s %s/>', id, x.ele, y.ele, transform.ele, style.sheet.ele)
  return(use.svg.ele)

}

