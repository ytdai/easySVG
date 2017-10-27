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
#' @param style.sheet a vector or a chatacter, other style of the group, eg. "stroke-linecap: round"
#' @return the characher type of svg element
#' @export
#' @examples
#' group.svg(id = "group_1", group.content = "this is a svg element")
#' group.content <- list(svg1 = "this is a svg element",
#'                       svg2 = "this is a svg element")
#' group.svg(id = "group_1", group.content = group.content)
#'

group.svg <- function(id = NULL,
                      group.content = NULL,
                      style.sheet = NULL) {
  if (!is.null(id)) {
    id.ele <- paste0('id="', id, '"')
  }
  if (!is.null(group.content)) {
    group.content.ele <- paste(group.content, collapse = "\n")
  } else {
    group.content.ele <- ""
  }
  if (!is.null(style.sheet)) {
    style.sheet.ele <- paste(style.sheet, collapse = ";")
    style.sheet.ele <- paste0('style="', style.sheet.ele, '"')
  } else {
    style.sheet.ele <- ""
  }
  group.svg.ele <- sprintf('<g %s %s>\n%s</g>\n', id.ele, style.sheet.ele, group.content.ele)
  return(group.svg.ele)
}

