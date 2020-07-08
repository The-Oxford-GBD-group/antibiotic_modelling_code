#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param admin_level PARAM_DESCRIPTION
#' @param simple_raster PARAM_DESCRIPTION
#' @param pop_raster PARAM_DESCRIPTION
#' @param gaul_list PARAM_DESCRIPTION
#' @param custom_admin_raster PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[seegSDM]{character(0)}}
#'  \code{\link[raster]{extract}}
#' @rdname make_population_weights
#' @export
make_population_weights <- function(admin_level,
                                    simple_raster,
                                    pop_raster,
                                    gaul_list,
                                    custom_admin_raster = NULL) {

  #################################################################################
  ### Use admin and population rasters and returns a weights raster
  ## Inputs:
  # admin_level: 0,1, or 2 are accepted. Level to which weights aggregate to 1
  # simple_raster: this is the raster template, should be the one that cell_preds was made with.
  # countries: should typically be template, by just a list of countries you want included in the output
  ## Outputs: returns a list with the following:
  # pop_wts: a cell_idx by years matrix of population weights, each number representing a cell
  # admin_level: 0,1,or 2 which admin level this represents
  # adm_code: which admin code (gaul) each cell is coded to
  ## Notes:
  # Later will need to be explicit about years (for now everyone is doing 2000-2015, 4 yr intervals)
  #################################################################################

  # use custom admin raster for raking
  if (!is.null(custom_admin_raster)) {
    adm <- custom_admin_raster
  } else {
    # load admin raster, crop down to simple_raster
    adm <- load_admin_raster(admin_level, simple_raster) # had to add this here, not working in call otherwise ers
  }

  adm <- crop(adm, simple_raster)
  adm <- mask(adm, simple_raster)
  # if(admin_level==0) adm <- simple_raster

  if (admin_level == 0) adm[adm == 1013965] <- 227

  # load population data
  pop <- pop_raster

  # do using cell index (vectorized)
  cell_idx <- notMissingIdx(simple_raster)
  adm_cell <- raster::extract(adm, cell_idx)

  pop_layer_names <- names(pop_raster)

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

  pop_weights_by_layer <- lapply(pop_layer_names, make_weights_vector)
  pop_wt_matrix <- do.call(cbind, pop_weights_by_layer)
  rownames(pop_wt_matrix) <- cell_idx
  colnames(pop_wt_matrix) <- pop_layer_names

  # return
  return(list(
    "pop_wt" = as.matrix(pop_wt_matrix),
    "admin_level" = admin_level,
    "adm_code" = adm_cell
  ))
}
