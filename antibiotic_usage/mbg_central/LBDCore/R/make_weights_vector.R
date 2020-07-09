#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[raster]{extract}}
#' @rdname make_weights_vector
#' @export
#' @importFrom raster extract
make_weights_vector <- function(x) {
  ad2_code <- raster::extract(adm$layer, cell_idx)
  ad2_code[is.na(ad2_code)] <- 0
  pop_cell <- raster::extract(pop[[x]], cell_idx)
  pop_cell[is.na(pop_cell)] <- 0

  message(paste0(length(unique(ad2_code)), " unique admin codes in this level."))

  pop_totals_adm <- aggregate(pop_cell ~ ad2_code, FUN = sum)
  pop_totals_adm$pop_cell[pop_totals_adm$pop_cell == 0] <- 0.0001

  ## replicate totals for all cells in area
  pop_totals_adm_cell <- as.vector(pop_totals_adm)[match(
    adm_cell,
    pop_totals_adm$ad2_code
  ), ]
  pop_totals_adm_cell$ad2_code <- NULL

  ## get  population weights for each cell
  pop_wt_adm <- pop_cell / pop_totals_adm_cell
  pop_wt_adm[is.na(pop_wt_adm)] <- 0
  pop_wt_adm_vector <- pop_wt_adm$pop_cell

  wt_sum_ad2 <- tapply(pop_wt_adm_vector, ad2_code, sum)
  stopifnot(all.equal(wt_sum_ad2, round(wt_sum_ad2)))

  names(pop_wt_adm_vector) <- x
  return(pop_wt_adm_vector)
}
