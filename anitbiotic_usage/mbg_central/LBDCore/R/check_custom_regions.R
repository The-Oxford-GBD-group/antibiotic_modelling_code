#' @title Check custom regions
#' @description Check custom region list against base MBG list and see what's added
#' or missing in comparison
#'
#' @param region_list vector of region names (defined in `pull_custom_modeling_regions()`)
#' @return list of two tables
#'         [[1]] gaul_codes in custom, but not default regions,
#'           [[2]] gaul_codes in default, but not custom regions, and
#'       [[3]] duplicated gaul_codes
#' @examples
#' \dontrun{
#' region_list <- c(
#'   "vax_soas", "vax_seas", "vax_eaas", "vax_caeu",
#'   "vax_crbn", "vax_ctam", "vax_ansa", "vax_trsa",
#'   "vax_name", "vax_cssa", "vax_essa", "vax_sssa", "vax_wssa"
#' )
#' check_list <- check_custom_regions(region_list)
#' }
#' @export
check_custom_regions <- function(region_list, shapefile_version = "current") {

  # Function to pull a table of gauls by region
  make_gaul_table <- function(rr) {
    data.table(
      region = rr,
      gaul_code = get_adm0_codes(rr, shapefile_version = shapefile_version)
    )
  }

  # Pull gauls for custom regions
  custom_reg_table <- lapply(region_list, make_gaul_table) %>% rbindlist()
  setnames(custom_reg_table, "region", "custom_region")

  # Pull gauls for default regions
  default_reg_table <- lapply(c("stage1", "stage2"), make_gaul_table) %>% rbindlist()
  setnames(default_reg_table, "region", "default_region")

  # Merge tables together
  reg_table <- merge(custom_reg_table, default_reg_table, all.x = T, all.y = T)

  # Grab the rest of the information about these gaul codes
  info_table <- fread("/home/j/WORK/11_geospatial/10_mbg/stage_master_list.csv")
  reg_table <- merge(reg_table, info_table, all.x = T, all.y = F, by.x = "gaul_code", by.y = "GAUL_CODE")

  custom_not_default <- subset(reg_table, !is.na(custom_region) & is.na(default_region))
  default_not_custom <- subset(reg_table, is.na(custom_region) & !is.na(default_region))

  # Check for duplicated gauls
  dups <- reg_table$gaul_code[duplicated(reg_table$gaul_code)] %>% unique()
  duplicated_table <- subset(reg_table, gaul_code %in% dups)

  message(paste0("There are ", nrow(custom_not_default), " gaul code(s) in the CUSTOM list not included in the default MBG regions."))
  message(paste0("There are ", nrow(default_not_custom), " gaul code(s) in the DEFAULT mbg regions not included in your custom list."))
  message(paste0("There are ", length(dups), " duplicated gaul code(s) in the CUSTOM list"))
  message(paste0(
    "\nReturning a list of three tables:\n",
    "  1) gaul codes in custom, but not default regions\n",
    "  2) gaul codes in default, but not custom regions\n",
    "  3) duplicated gaul codes"
  ))

  return(list(
    custom_not_default = custom_not_default,
    default_not_custom = default_not_custom,
    duplicated = duplicated_table
  ))
}
