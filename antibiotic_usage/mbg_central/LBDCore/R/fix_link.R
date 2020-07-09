#' @title Fix area fractions in link table
#'
#' @description Scales area fractions for pixels with water. Pixels with 1 admin unit and
#' water have the area fraction set to 1, as all values will occur on the land. Pixels
#' containing multiple admin units and water are scaled proportionally. Internal function
#' used in \code{build_link_table()}
#'
#' @param link link table
#'
#' @return a link table with adjusted area fractions
#'
#' @export
fix_link <- function(link) {
  # figure out which pixels have multiple rows
  link_fix <- link %>%
    group_by(ID) %>%
    summarise(
      total_area = sum(area_fraction),
      n = n()
    ) %>%
    data.table()

  # merge link, link_fix and cast some variables to numeric
  link_fix2 <- merge(link, link_fix, by = "ID")
  link_fix2[, area_fraction := as.numeric(area_fraction)]
  link_fix2[, total_area := as.numeric(total_area)]
  # set area to 1 for all pixels in a single admin unit and water
  link_fix2[n == 1 & area_fraction < 1, area_fraction := 1]
  # recalculate area for pixels in multiple admin units and water proportionally
  link_fix2[n > 1 & total_area < 1, area_fraction := area_fraction * (1 / (1 - (1 - total_area)))]
  return(link_fix2)
}
