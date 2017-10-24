#' make svg group
#'
#' @param id a character, group id
#' @param content a character or a list, group content
#' @return the characher type of svg element
#' @export
#' @examples
#' group.svg(id = "group_1", content = "this is a svg element")
#' content <- list(svg1 = "this is a svg element",
#'                 svg2 = "this is a svg element")
#' group.svg(id = "group_1", content = content)
#'

group.svg <- function(id = NULL,
                      content = NULL) {
  if (!is.null(id)) {
    id.ele <- paste0('id="', id, '"')
  }
  if (is.list(content)) {
    content <- paste(content, collapse = "\n")
  }
  group.svg.ele <- sprintf('<g %s>%s</g>', id.ele, content)
  return(group.svg.ele)
}
