#' @title An empty `ggplot()` theme
#' @description An empty `ggplot()` theme, to be added to a \code{ggplot} object
#' @return theme object for ggplot
#' @examples
#' \dontrun{
#' ggplot(data = data, aes(x = x, y = y)) +
#'   geom_point() +
#'   theme_empty()
#' }
#' @export
theme_empty <- function() {
  theme_empty <- theme_classic() +
    theme(
      axis.line = element_blank(), axis.text.x = element_blank(),
      axis.text.y = element_blank(), axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
  return(theme_empty)
}
