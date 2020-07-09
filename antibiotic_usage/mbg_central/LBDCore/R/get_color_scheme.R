#' @title Get color scheme
#' @description Grabs a color scheme from a set of defaults
#'
#' @param theme Name of theme
#' @return A vector of color hex codes
#' @examples
#' \dontrun{
#' get_color_scheme("carto_discrete")
#' }
#' @export
get_color_scheme <- function(theme) {

  # Set up categorical colors
  carto_discrete <- c(
    "#7F3C8D", "#11A579", "#F2B701", "#E73F74",
    "#3969AC", "#80BA5A", "#E68310", "#008695",
    "#CF1C90", "#f97b72", "#4b4b8f", "#A5AA99",
    "#66C5CC", "#F6CF71", "#F89C74", "#DCB0F2",
    "#87C55F", "#9EB9F3", "#FE88B1", "#C9DB74",
    "#8BE0A4", "#B497E7", "#D3B484", "#B3B3B3"
  )

  return(get(theme))
}
