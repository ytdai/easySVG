#' Generate SVG element of axis
#'
#' This function will generate a axis form SVG element.
#'
#' @param x a vector, the range of your number
#' @param stroke a number, the line stroke of the axis
#' @param stroke.width a number, the line stroke of the axis
#' @param line.length a number, the line length of the axis
#' @param axis.font.size a number, the axis font size of axis
#' @param digit a number, the significant digits number of axis
#' @param span a number, distance between number and axis line
#' @param id a character, the id name of this axis
#' @param unit the unit of this axis
#' @return the characher type of SVG element
#' @export
#' @examples
#' lim.axis.1 <- lim.axis.svg(x = c(100, 900), id = "test")
#' pack_info_1 <- pack.svg(pack.content = lim.axis.1)
#' # You can write it in a svg file
#' # message(pack_info_1)
#'
#' lim.axis.2 <- lim.axis.svg(x = c(3.3, 4,5), id = "test", unit = 4000, axis.font.size = 4)
#' pack_info_2 <- pack.svg(pack.content = lim.axis.2)
#' # You can write it in a SVG file
#' # message(pack_info_2)
#'
lim.axis.svg <- function(x = NULL,
                         stroke = "#000000",
                         stroke.width = 1,
                         line.length = 100,
                         axis.font.size = 8,
                         digit = 2,
                         span = 5,
                         id = NULL,
                         unit = NULL) {

  if (is.null(x)) {
    stop("[ERROR] The range of axis is required x = c(start, end)!")
  }
  lim_range <- x[2] - x[1]

  lim_tag <- strsplit(format(lim_range,scientific=TRUE,digit=1), split = "e")

  if (is.null(unit)) {
    lim_tag_m <- as.integer(lim_tag[[1]][1])
    lim_unit_len <- line.length / lim_range * (10 ** as.integer(lim_tag[[1]][2]))
    lim_unit_num <- 10 ** as.integer(lim_tag[[1]][2])
  } else {
    lim_tag_m <- floor( lim_range / unit)
    lim_unit_len <- line.length / lim_range * unit
    lim_unit_num <- unit
  }

  uh <- min(10, line.length / 50)
  ul <- min(5, line.length / 100)

  line.1 <- line.svg(x1 = 0, y1 = 0, x2 = line.length, y2 = 0,
                     stroke = stroke, stroke.width = stroke.width)

  line.2 <- lapply(1:lim_tag_m, function(m) line.svg(x1 = (m-1)*lim_unit_len, y1 = 0,
                                                   x2 = (m-1)*lim_unit_len, y2 = -1 * uh,
                                                   stroke = stroke,
                                                   stroke.width = stroke.width))
  line.3 <- line.svg(x1 = line.length, y1 = 0,
                     x2 = line.length, y2 = -1 * uh,
                     stroke = stroke,
                     stroke.width = stroke.width)

  text.1 <- lapply(1:lim_tag_m, function(m) get.text.svg(x = (m-1)*lim_unit_len, y = ul + span,
                                                         text.content = signif((x[1] + (m-1)*lim_unit_num), digits = 2),
                                                         font.size = axis.font.size,
                                                         text.anchor = "middle"))
  text.2 <- get.text.svg(x = line.length, y = ul + span,
                         text.content = signif(x[2], digits = 2),
                         font.size = axis.font.size,
                         text.anchor = "middle")

  lim.axis.svg.ele <- paste(c(line.1, unlist(line.2), line.3, unlist(text.1), text.2), collapse = "\n")
  lim.axis.svg.ele <- group.svg(id = id, group.content = lim.axis.svg.ele)

  return(lim.axis.svg.ele)

}
