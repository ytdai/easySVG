#' make svg defs
#'
#' @param content a character or a list, group content
#' @return the characher type of svg element
#' @export
#' @examples
#' defs.svg(content = "this is a svg element")
#' content <- list(svg1 = "this is a svg element",
#'                 svg2 = "this is a svg element")
#' defs.svg(content = content)
#'

defs.svg <- function(content = NULL) {
  if (is.list(content)) {
    content <- paste(content, collapse = "\n")
  }
  defs.svg.ele <- sprintf('<defs>\n%s</defs>\n\n', content)
  return(defs.svg.ele)
}


#' pack svg
#'
#' @param width a number, width of the plot
#' @param height a number, height of the plot
#' @param content a character or a list, group content
#' @return the characher type of svg element
#' @examples
#' pack.svg(content = "<text x=\"10\" y=\"20\"> this is a svg element </text>")
#' content <- list(svg1 = "<text x=\"10\" y=\"20\"> this is a svg element </text>",
#'                 svg2 = "<text x=\"10\" y=\"20\"> this is a svg element </text>")
#' pack.svg(content = content)
#' pack.svg(content = "<text x=\"10\" y=\"20\"> this is a svg element </text>", output = T, output.name = "test.svg")
#'

pack.svg <- function(width = 1200,
                     height = 800,
                     output = F,
                     output.name = NULL,
                     content = content) {
  if (is.list(content)) {
    content <- paste(content, collapse = "\n")
  }
  svg_header <- sprintf("<?xml version=\"1.0\" standalone=\"no\"?> \n<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \n  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\"> \n\n<svg width=\"%s\" height=\"%s\" version=\"1.1\" \n  xmlns=\"http://www.w3.org/2000/svg\" xmlns:xlink=\"http://www.w3.org/1999/xlink\">\n", width, height)
  svg_footer <- sprintf("\n</svg>")

  pack.svg.ele <- sprintf('%s\n\n%s\n%s\n\n', svg_header, content, svg_footer)

  if (output) {
    if (is.null(output.name)) {
      message("[WARNING] the output svg file name must be given!")
    } else {
      write(pack.svg.ele, file = output.name, sep = "\n")
    }
  }
  return(pack.svg.ele)
}


