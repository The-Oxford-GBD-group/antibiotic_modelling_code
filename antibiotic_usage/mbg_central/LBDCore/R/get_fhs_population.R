#' @title Get pre-saved FHS population outputs
#' @description Access pre-saved RDS population outputs from FHS (saved out by Nafis Sadat in \code{/share/scratch/users/sadatnfs/geospatial_shared/population_fbd})
#' 
#' @param population_version A version tag found from looking into: \code{/share/scratch/users/sadatnfs/geospatial_shared/population_fbd}.
#' Default: \code{"20190403_test_new_cluster_1000d_rerun_fix_draw_squeezed_agg_ordered"}
#' @param pop_measure WorldPop measure; only one allowed for now! Default: a0004t
#' @param sex_id Sex ID. Default: 3
#' @param scenario FHS scenario. Default: 0 (reference).
#' @param year_ids. Year_id to query. Default: NULL (get all years)
#' @param gbd_regions Regions to query (GBD Location IDs). Default: NULL (get all regions)
#'
#' @return A data.table with location_id, year_id, age_group_id = pop_measure, sex_id, run_id, population
#' 
#' @export
get_fhs_population <- function(population_version = "20190403_test_new_cluster_1000d_rerun_fix_draw_squeezed_agg_ordered",
                               pop_measure = "a0004t",
                               sex_ids = 3,
                               scenarios = 0,
                               year_ids = NULL,
                               gbd_regions = NULL) {
  
  ## Get pop data
  pop_data <- readRDS(paste0("/share/scratch/users/sadatnfs/geospatial_shared/population_fbd/", population_version, ".rds"))
  
  ## Keep on subsettin'
  pop_data <- pop_data[(age_group_id %in% get_age_group_from_worldpop(pop_measure = pop_measure)) & (scenario %in% scenarios) & (sex_id %in% sex_ids)]
  
  if(!is.null(year_ids)) {
    pop_data <- pop_data[year_id %in% year_ids]
  }
  if(!is.null(gbd_regions)) {
    pop_data <- pop_data[location_id %in% gbd_regions]
  }
  
  ## Return data with alignment of our choice, keeping unique summed up population entries
  pop_data[, age_group_id:= NULL]
  pop_data[, age_group_id:= pop_measure]
  pop_data <- pop_data[, .(population = sum(population)), by = c('location_id', 'year_id', 'age_group_id', 'sex_id')]
  
  ## Make fake run_id column
  pop_data[, run_id:= paste0(population_version)]
  
  setkeyv(pop_data, c('location_id', 'year_id', 'age_group_id', 'sex_id'))
  return(pop_data)
  
}


