#' @title Get Admin0 (Country) Codes
#'
#' @description Pull Admin0 Codes for the specified countries or regions,
#'   optionally excluding certain Admin0 codes.
#'
#' @param adm0s Vector of ISO-3 codes, four-letter modeling region names, region
#'   group names as defined in get_region_groups, or 'all' (returns all Admin0
#'   codes)
#' @param strict (default `FALSE`) Causes the function to fail if an ISO code or
#'   region name is not included in the full list.
#' @param lookup_table (default `NULL`) Sets a data.frame or data.table to be
#'   used as the lookup_table. If `NULL`, the lookup table will be loaded using
#'   the `load_adm0_lookup_table()` function.
#' @param core_repo (default NULL) THIS ARGUMENT IS DEPRECATED AND WILL BE
#'   REMOVED IN THE FUTURE. Please remove it from your function calls.
#' @param adm0_type (default 'detect') Which class of admin0 codes
#'   should be pulled? Must be one of 'gaul', 'gadm', or 'detect'. If
#'   'gaul' or 'gadm', it will simply use what was specified. If
#'   'detect', the function will detect the adm0_type directly from an
#'   admin_shapefile. It will check the adm0_type specified by the
#'   shapefile_version argument if it is specified, otherwise if will
#'   detect the adm0_type using 'current'.
#' @param shapefile_version string specifying shapefile_version to be
#'   used in adm0_type determination if adm0_type is set to 'detect'
#' @param subnational_raking Logical. set to true if you want to use raking version of shapefile
#' @return Vector of numeric ADM0 codes. Warns if not all regions align with a
#'   ADM0 code.
#'
#' @export
get_adm0_codes <- function(adm0s,
                           strict = FALSE,
                           lookup_table = NULL,
                           core_repo = NULL,
                           adm0_type = "detect",
                           shapefile_version = NULL,
                           subnational_raking = FALSE) {

  # Check adm0_type input
  if (!(adm0_type %in% c("gaul", "gadm", "detect"))) {
    stop("You must select either 'gaul', 'gadm', or 'detect' for adm0_type!")
  }
  # Check that data.table has been loaded
  if (!("data.table" %in% .packages())) {
    stop("Please load the data.table package before running get_adm0_codes!")
  }
  # core_repo deprecation message
  if (!is.null(core_repo)) {
    message(paste0(
      "WARNING: The 'core_repo' argument has been deprecated in get_adm0_codes()",
      " -- please remove this argument."
    ))
  }

  # Determine adm0_type
  if (adm0_type == "detect") {
    if (is.null(shapefile_version)) {
      adm0_type <- detect_adm_shapefile_date_type(
        get_admin_shapefile(version = "current", raking = subnational_raking)
      )$shpfile_type
    } else {
      adm0_type <- detect_adm_shapefile_date_type(
        get_admin_shapefile(version = shapefile_version, raking = subnational_raking)
      )$shpfile_type
    }
  }
  message(sprintf("Using ADM codes from %s", adm0_type))

  # Read in the ADM0 code data.table once at the beginning of the function
  if (is.null(lookup_table)) lookup_table <- load_adm0_lookup_table()

  # Check that a valid adm0_type has been defined
  # If so, define the field that will be pulled from the lookup table
  adm_type <- tolower(adm0_type)
  adm0_field_reference <- list(
    gadm = "gadm_geoid",
    gaul = "GAUL_CODE"
  )
  if (!(adm0_type %in% names(adm0_field_reference))) {
    stop(paste0(
      "Invalid adm0_type value. Valid values are: ",
      paste(names(adm0_field_reference, collapse = ", "))
    ))
  }
  pull_field <- adm0_field_reference[[adm0_type]]

  ## PROCESS INCOMING ADM0 TEXT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # In case multiple character strings were passed in, concatenate with '+'
  adm0s <- paste(adm0s, collapse = "+")
  # Set all as lowercase
  adm0s <- tolower(adm0s)

  # Split on - and + characters
  adm0s <- gsub(";", "", adm0s)
  adm0s <- gsub("\\+", ";;\\+", adm0s)
  adm0s <- gsub("-", ";;-", adm0s)
  adm0s_vec <- unlist(base::strsplit(adm0s, ";;"))

  # Making the function past-compatible, for modelers who are still using the
  #  original 'name' modeling region for North Africa (now 'noaf').
  #  'name_historic' is now a custom region, equivalent to 'noaf-esh', in the
  #  pull_custom_modeling_regions().
  adm0s_vec <- gsub("^([+-])?name$", "\\1name_historic", adm0s_vec)

  # If the string was empty, end now
  if (length(adm0s_vec) == 0) {
    message("No GAUL codes were returned; exiting")
    return(numeric(0))
  }

  # The first region and all regions beginning in '+' are included
  # Strip the beginning + signs
  include <- gsub("\\+", "", c(adm0s_vec[1], adm0s_vec[ grepl("^\\+", adm0s_vec) ]))
  # All regions beginning with '-' are treated as exclusions
  # Strip any minus signs from them
  exclude <- gsub("-", "", adm0s_vec[ grepl("^-", adm0s_vec) ])

  ## CHECK FOR CUSTOM REGIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Define regex for all non-custom regions
  standard_code_regex <- "^([a-z]{3,4}|[a-z]{3}_d[0-9]{1,5})$"
  # Split both included and excluded data into custom and non-custom regions
  standard_inc <- include[ grepl(standard_code_regex, include) ]
  custom_inc <- include[ !grepl(standard_code_regex, include) ]
  # Check in excluded data
  standard_exc <- exclude[ grepl(standard_code_regex, exclude) ]
  custom_exc <- exclude[ !grepl(standard_code_regex, exclude) ]

  # Get definitions and ADM0 codes for included and excluded GAULs, if needed
  # `custom_inc_codes` and `custom_exc_codes` will be added/subtracted at the end
  # Check out that functional recursion
  recursive_pull <- function(x) get_adm0_codes(
      x,
      lookup_table = lookup_table, adm0_type = adm0_type
    )

  if (length(custom_inc) > 0) {
    custom_inc_defs <- pull_custom_modeling_regions(custom_inc)
    custom_inc_codes <- unlist(lapply(custom_inc_defs, recursive_pull))
  } else {
    custom_inc_codes <- integer(0)
  }
  if (length(custom_exc) > 0) {
    custom_exc_defs <- pull_custom_modeling_regions(custom_exc)
    custom_exc_codes <- unlist(lapply(custom_exc_defs, recursive_pull))
  } else {
    custom_exc_codes <- integer(0)
  }

  ## PROCESS NON-CUSTOM REGIONS (MBG MODELING REGIONS + ISOS) ~~~~~~~~~~~~~~~~~~
  # Helper function to pull all GAUL codes for a set of modeling regions or
  #  ISO codes
  utility_pull_adm0_codes <- function(region_codes, standard_regex) {
    # Return none if the length of the inputs is 0
    if (length(region_codes) == 0) return(integer(0))
    # Input data assertions
    if (!all(grepl(standard_regex, region_codes))) {
      stop("All region codes must match the MBG or ISO code formats.")
    }
    isos <- region_codes[nchar(region_codes) != 4]
    mbg_regs <- region_codes[nchar(region_codes) == 4]
    if ("all" %in% isos) {
      # 'all' is a special case where all GAUL codes are pulled
      pulled_adm0s <- unique(lookup_table[get(pull_field) >= 0, get(pull_field)])
    } else {
      (
        # Pull all GAUL codes matching the ISOs or regions
        pulled_adm0s <- unique(
          lookup_table[
            ((iso3 %in% isos) | (mbg_reg %in% mbg_regs)) & (get(pull_field) >= 0),
            get(pull_field)
          ]
        )
      )
    }
    # Warn if any iso codes or MBG regions aren't in the lookup table
    missing_isos <- isos[ !(isos %in% lookup_table[, iso3]) & (isos != "all") ]
    if (length(missing_isos) > 0) {
      message(paste0(
        "WARNING: Missing these ISOs: ",
        paste(missing_isos, collapse = ",")
      ))
    }
    missing_regs <- mbg_regs[ !(mbg_regs %in% lookup_table[, mbg_reg]) ]
    if (length(missing_regs) > 0) {
      message(paste0(
        "WARNING: Missing these MBG regions: ",
        paste(missing_regs, collapse = ",")
      ))
    }
    return(as.integer(pulled_adm0s))
  }

  # Pull GAUL codes for standard include and exclude regions
  standard_inc_codes <- utility_pull_adm0_codes(standard_inc, standard_code_regex)
  standard_exc_codes <- utility_pull_adm0_codes(standard_exc, standard_code_regex)

  ## COMBINE TO CREATE FINAL ADM0 CODE SET ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Get all (standard + custom) codes to include
  all_include_codes <- unique(c(standard_inc_codes, custom_inc_codes))
  # Get all (standard + custom) codes to exclude
  all_exclude_codes <- unique(c(standard_exc_codes, custom_exc_codes))
  # Final codes = codes to include - codes to exclude
  return_codes <- all_include_codes[ !(all_include_codes %in% all_exclude_codes) ]

  return(return_codes)
}
