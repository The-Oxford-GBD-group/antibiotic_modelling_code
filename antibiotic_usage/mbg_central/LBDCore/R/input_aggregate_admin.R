#' @title Correctly assign and then collapse input data to the admin0/ admin1/ admin2 level
#' to be used in the visualization function and add input data to plots
#'
#' @description This function pulls the input data from the model directory, however it requires that you include
#' a column that retains the sum of the sample weights when collapsing to lat/long or shapefiles in your collapse
#' code in order to correctly recollapse to the specified admin. For this reason, it is important to include this
#' information in the collapse code before using this function. You can name the column whatever you want, though the
#' default name is sum_of_sample_weights. This is an argument to the function, sample_column. The input data must also have
#' a column labeled "point" that is an indicator for whether the data is point data (1) or polygon data (0).
#'
#' You may also specify the input
#' data yourself by passing the loaded input data as an argument to the function, do this if your input data is not specified
#' in the model directory, though this should be the case for most using the mbg pipeline.
#'
#' This function uses the sf package, which will currently only work when running on the lbd_singularity image or in
#' RStudio on the cluster.
#'
#' @author Michael Cork, \email{mcork23\@uw.edu}
#'
#' @param indicator indicator name used in file structure for mbg
#' @param indicator_group indicator group
#' @param regions Regions specified from your model
#' @param run_date  model run date
#' @param input_data If specified, provides the preloaded input data so the function does not look in your model directory
#' @param indicator_family If specified as Gaussian, this makes sure to not divide the prevalence by N which is required for binomial indicators
#' @param svy_id This is the unique survey id, usually labeled "nid" but other teams might use different terminology
#' @param sample_column This is the name of the column that contains the sum of the sample weights for the collapsed points.
#' @param subnational_nids If specified, these are the nids of surveys that are not nationally representative, and appear on the data plots with a different shape
#' @param shapefile_version String indicating version of shapefile to pull
#' @param use_kish boolean - If TRUE, admin aggregated N's are calculated using Kish approx
#'
#' @return a list containing three data tables that correspond to collapsed admin levels, named ad0/ad1/ad2. This list should then
#' be assigned to parameters ad0_data/ad1_data/ad2_data for the visualization function if plot_data == T. The admin data will have columns
#' named outcome
#'
#' @examples
#' \dontrun{
#' # Set up directories and files
#' # Set `run_date`, `indicator`, `indicator_group`, `sample_column` as you wish
#' 
#' admin_data <- input_aggregate_admin(indicator = indicator, indicator_group, regions = c("cssa", "wssa", "essa", "sssa"), indicator_family = "binomial", sample_column = "sum_of_sample_weights")
#' ad0_data <- admin_data$ad0
#' ad1_data <- admin_data$ad1
#' ad2_data <- admin_data$ad2
#' 
#' ## Running the plotting code with plot_data = T
#' subnational_ts_plots(
#'   ad0_df = ad0_df,
#'   ad1_df = ad1_df,
#'   ad2_df = ad2_df,
#'   ad0_shape = ad0_shape,
#'   ad1_shape = ad1_shape,
#'   ad2_shape = ad2_shape,
#'   ind_title = "My Indicator",
#'   out_dir = out_dir,
#'   highisbad = F,
#'   val_range = c(0, 1),
#'   ad0_map_regions = c("cssa", "essa", "name", "sssa", "wssa"),
#'   ad0_map_region_titles = c(
#'     "Central Sub-Saharan Africa",
#'     "Eastern Sub-Saharan Africa",
#'     "Northern Africa",
#'     "Southern Sub-Saharan Africa",
#'     "Western Sub-Saharan Africa"
#'   ),
#'   verbose = T,
#'   plot_data = T,
#'   ad0_data = ad0_data,
#'   ad1_data = ad1_data,
#'   ad2_data = ad2_data
#' )
#' }
#' @export
input_aggregate_admin <- function(indicator,
                                  indicator_group,
                                  regions = c("cssa", "wssa", "essa", "sssa", "name"),
                                  run_date = NULL,
                                  input_data = NULL,
                                  indicator_family = "binomial",
                                  svy_id = "nid",
                                  sample_column = "sum_of_sample_weights",
                                  subnational_nids = NULL,
                                  shapefile_version = "current",
                                  use_kish = FALSE) {

  # Make sure this is run on singularity container in Rstudio or lbd singularity to use sf package
  if (!is_lbd_singularity() & !is_rstudio(check_singularity = TRUE)) {
    stop("must be run in lbd singularity or Rstudio singularity container")
  }

  # If input_data not passed to the function, load in input data from model, or from mbg if the appropriate column name is not specified.
  if (is.null(input_data)) {
    mod_dir <- sprintf("/share/geospatial/mbg/%s/%s/output/%s/", indicator_group, indicator, run_date)
    message(paste0(
      "Input data was not provided, so loading from model directory: ", mod_dir,
      ". \n If this does not exist, consider passing input data as an argument to this function"
    ))
    input_data <- fread(paste0(mod_dir, "input_data.csv"))

    if (!sample_column %in% names(input_data)) {
      message("Sample weights column not found in model directory, pulling from input data on J drive")
      input_data <- fread(paste0("/home/j/WORK/11_geospatial/10_mbg/input_data/", indicator, ".csv"))
      if (!sample_column %in% names(input_data)) stop("Sample weights column not found on J drive, make sure to add a sum of sample weights column) in collapse code and specify columns name as sample_column argument in function")
    }
  } else {
    if (!sample_column %in% names(input_data)) {
      stop("Sample weights column not found in the provided input data. Make sure that this column is included and specified by the sample_column
           argument in the function call.")
    }
  }

  if (!"source" %in% names(input_data)) stop("Need to specify the source of the data under a source column in the input data")
  if (!"point" %in% names(input_data)) stop("You need a column in your input data called 'points' that classifies point data as 1 and polygon as 0")

  # Load in shapefile
  admin_shp <- sf::st_read(get_admin_shapefile(admin_level = 2, version = shapefile_version), quiet = T)

  # Add country name (not just 3 letter abbreviation) to input data
  # Subset input data to given region defined by the regions argument
  gaul_codes <- get_adm0_codes(regions, shapefile_version = shapefile_version)

  gaul_to_loc_id <-
    get_location_code_mapping(shapefile_version = shapefile_version) %>%
    dplyr::select(GAUL_CODE, loc_name, ihme_lc_id) %>%
    dplyr::rename(location_name = loc_name) %>%
    filter(GAUL_CODE %in% gaul_codes)

  # edit gaul_to_loc_id to use location names from admin shapefile, because the subnational viz function merges on names
  admin_shp_data <- as.data.table(admin_shp)
  admin_shp_data <- unique(admin_shp_data[, c("ADM0_CODE", "ADM0_NAME")])
  gaul_to_loc_id <- gaul_to_loc_id[, c("GAUL_CODE", "ihme_lc_id")]
  gaul_to_loc_id <- merge(gaul_to_loc_id, admin_shp_data, by.x = "GAUL_CODE", by.y = "ADM0_CODE")
  setnames(gaul_to_loc_id, "ADM0_NAME", "location_name")

  # Join input data to location names to match shapefile. Some of those countries need to be manually changed to fit shapefile names
  input_data <-
    input_data %>%
    left_join(gaul_to_loc_id, by = c("country" = "ihme_lc_id")) %>%
    rowwise() %>%
    ungroup() %>%
    data.table() %>%
    setnames(c(svy_id, sample_column), c("svy_id", "sample_column"))

  missing <-
    input_data %>%
    filter(is.na(location_name)) %>%
    nrow()

  message(paste0(round(100 * missing / nrow(input_data), 2), " % of data is from outside of specified regions: ", paste(regions, collapse = " ")))

  input_data <-
    input_data %>%
    filter(!is.na(location_name))

  # Subset shapefile to include countries we have data on
  countries <- input_data$location_name %>% unique()

  admin_shp <-
    admin_shp %>%
    mutate(ADM0_NAME = as.character(ADM0_NAME)) %>%
    filter(ADM0_NAME %in% countries)

  # Assign input data to correct admin0/admin1/dmin2 in one step
  message("Assigning lat/longs to correct admin level")
  input_admin <-
    input_data %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = st_crs(admin_shp)) %>%
    st_join(admin_shp) %>%
    st_set_geometry(NULL) %>%
    setnames(eval(indicator), "prev")

  missing <-
    input_admin %>%
    filter(is.na(ADM0_CODE), is.na(ADM1_CODE), is.na(ADM2_CODE)) %>%
    nrow()

  message(paste0(round(100 * missing / nrow(input_admin), 3), " % of data could not be matched to an admin level"))

  input_admin <-
    input_admin %>%
    filter(!is.na(ADM0_CODE), !is.na(ADM1_CODE), !is.na(ADM2_CODE)) %>%
    data.table() # remove those that are not assigned to an admin0/admin2

  # Make sure they are assigned to the correct country, some are right over the border and so are pushed into a different area.
  # These are excluded (usually not substantial) becuase they then do not nest well with admin1 and admin2 estimates
  wrong_admin0 <- nrow(input_admin[location_name != ADM0_NAME])
  message(paste0(round(wrong_admin0 / nrow(input_admin), 3), " % of input data is matched to a different country.\nThese are usually located on the border and will be dropped for the visualization."))

  input_admin <- input_admin[location_name == ADM0_NAME]

  # If binomial make sure it is prevalence space
  if (indicator_family == "binomial") input_admin[, prev := prev / N]

  # Collapse to admin 0 level
  message("collapsing to admin 0 level")
  input_admin0 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = ifelse(use_kish,
        sum(as.numeric(sample_column))^2 / sum(as.numeric(sample_column^2 / N)),
        sum(N * weight)
      )
    ) %>%
    ungroup() %>%
    data.table()

  # Collapse to admin 1 level
  message("collapsing to admin 1 level")
  input_admin1 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = ifelse(use_kish,
        sum(as.numeric(sample_column))^2 / sum(as.numeric(sample_column^2 / N)),
        sum(N * weight)
      )
    ) %>%
    ungroup() %>%
    data.table()

  # Collapse to admin 2 level
  message("collapsing to admin 2 level")
  input_admin2 <-
    input_admin %>%
    group_by(svy_id, source, point, ADM0_NAME, ADM0_CODE, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE) %>%
    dplyr::summarise(
      year = floor(median(year, na.rm = T)),
      outcome = weighted.mean(prev, sample_column),
      N = ifelse(use_kish,
        sum(as.numeric(sample_column))^2 / sum(as.numeric(sample_column^2 / N)),
        sum(N * weight)
      )
    ) %>%
    ungroup() %>%
    data.table()

  # Change source/polygon to factor and shorten survey names to fit on plot legend

  input_admin0 <- input_clean(input_admin0)
  input_admin1 <- input_clean(input_admin1)
  input_admin2 <- input_clean(input_admin2)

  # If subnational NID's are included, assign them values 2 and 3 for point and polygon data that are subnationally representative, respectively


  if (!is.null(subnational_nids)) {
    input_admin0 <- subnational_nid_subset(input_admin0)
    input_admin1 <- subnational_nid_subset(input_admin1)
    input_admin2 <- subnational_nid_subset(input_admin2)
  }

  # Final list contains each aggregated admin
  input_admins <- list(ad0 = input_admin0, ad1 = input_admin1, ad2 = input_admin2)
  return(input_admins)
}
