#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param indicator PARAM_DESCRIPTION
#' @param indicator_group PARAM_DESCRIPTION
#' @param run_date PARAM_DESCRIPTION
#' @param this_gaul PARAM_DESCRIPTION
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @rdname plot_varios_and_violins
#' @export
plot_varios_and_violins <- function(indicator,
                                    indicator_group,
                                    run_date,
                                    this_gaul,
                                    shapefile_version = "current") {
  in_dir <- paste0("/share/geospatial/mbg/", indicator_group, "/", indicator, "/output/", run_date)
  default_rf_path <- paste0(in_dir, "/", indicator, "_rf.csv")
  all_rfs <- fread(default_rf_path)

  ## Define path to .tif of results and raked results, load in raster bricks.
  default_raked_results_path <- brick(paste0(in_dir, "/", indicator, "_mean_raked_raster.tif"))
  results_raked <- brick(default_raked_results_path)
  default_results_path <- brick(paste0(in_dir, "/", indicator, "_mean_raster.tif"))
  results <- brick(default_results_path)
  total_periods <- length(names(results))
  regions <- get_output_regions(in_dir)
  for (reg in regions) {
    if (this_gaul %in% get_adm0_codes(reg, shapefile_version = shapefile_version)) this_region <- reg
  }
  input_data <- list.files(in_dir, pattern = paste0("input_data[a-zA-Z0-9_]*", this_region), full.names = T) %>% fread()

  gaul_to_loc_id <- get_location_code_mapping(shapefile_version = shapefile_version)
  gauls <- this_gaul
  this_country <- gaul_to_loc_id[GAUL_CODE %in% gauls, ihme_lc_id]

  if (indicator != "edu_mean") input_data <- input_data[, rate := get(indicator) / N]
  if (indicator == "edu_mean") input_data <- input_data[, rate := get(indicator)]
  country_input <- input_data[country %in% this_country, ]
  country_input <- subset(country_input, year >= 1998)
  names(country_input)[names(country_input) == "year"] <- "original_year"
  country_input <- country_input[original_year >= 1998 & original_year < 2003, year := 2000]
  country_input <- country_input[original_year >= 2003 & original_year < 2008, year := 2005]
  country_input <- country_input[original_year >= 2008 & original_year < 2013, year := 2010]
  country_input <- country_input[original_year >= 2013 & original_year < 2018, year := 2015]
  country_input_dt <- country_input
  coordinates(country_input) <- ~ longitude + latitude

  ## Calculate variogram for data
  v_all <- variogram(log(rate + 0.001) ~ 1, country_input)
  v_all$year_category <- "All years"
  ## Do five-year period variograms
  vario_list <- list()
  for (this_year in unique(country_input_dt[, year])) {
    year_subset <- country_input_dt[year == this_year, ]
    coordinates(year_subset) <- ~ longitude + latitude
    v <- variogram(log(rate + 0.001) ~ 1, year_subset)
    v$year_category <- as.character(paste0(this_year, " window"))
    # message(paste0('vario_plot_', this_year))
    if ("data.frame" %in% class(v) == FALSE) v <- NULL
    assign(paste0("vario_plot_", this_year), v)
    vario_list[[as.character(this_year)]] <- v
  }
  # vario_years <- ls()[(grep("vario_plot_", ls()))]
  # vario_years <- rbindlist(lapply(vario_years, get))
  vario_years <- rbindlist(vario_list)
  vario_years <- rbind(v_all, vario_years)
  ## Make variogram
  gg_country_vario <- ggplot(data = vario_years, aes(x = dist, y = gamma, col = year_category)) +
    geom_point() +
    geom_line() +
    ylab("Semivariance") +
    xlab("Distance") +
    theme_minimal() +
    guides(col = guide_legend(title = "Year category"))

  ## Make PACF
  country_input_dt_acf <- country_input_dt[, list(new_rate = weighted.mean(x = rate, w = weight * N)), by = c("original_year")]
  country_input_dt_acf <- country_input_dt_acf[order(original_year)]
  # country_pacf <- pacf(country_input_dt_acf[, new_rate], main = "PACF")
  country_pacf <- "placeholder" ## Super annoying to grab plot object right now

  ## Calculate variogram for predictions
  ## Extract preds for each year in country data
  default_raked_results_path <- paste0(in_dir, "/", indicator, "_mean_raked_raster.tif")
  results_raked <- brick(default_raked_results_path)
  default_results_path <- paste0(in_dir, "/", indicator, "_mean_raster.tif")
  results <- brick(default_results_path)

  country_input_wpreds <- rbindlist(lapply(unique(country_input_dt[, year]), extract_year_preds))
  country_input_wpreds <- country_input_wpreds[!is.na(pred), ]
  country_input_wpreds <- country_input_wpreds[, data := rate]
  violin_vars <- c("data", "pred")
  country_input_wpreds <- country_input_wpreds[, c("latitude", "longitude", "weight", violin_vars, "year"), with = F]
  country_input_wpreds <- melt(country_input_wpreds, id.vars = c("longitude", "latitude", "weight", "year"), measure.vars = violin_vars)
  country_input_wpreds <- country_input_wpreds[, country := this_country]
  if (file.exists(paste0(results_dir, "/", indicator, "_rf.csv"))) {
    rfs <- fread(paste0(results_dir, "/", indicator, "_rf.csv"))
    country_input_wpreds <- country_input_wpreds[, name := as.integer(this_gaul)]
    country_input_wpreds <- merge(country_input_wpreds, rfs, by = c("name", "year"))
  }
  violin_countries <- this_country
  loc_names <- get_location_code_mapping(shapefile_version = shapefile_version)
  convert_to_iso3 <- function(x) {
    if (nchar(x) > 3) x <- loc_names[loc_name == x, ihme_lc_id]
    return(x)
  }
  violin <- ggplot(data = country_input_wpreds) +
    geom_violin(aes(x = variable, y = value, fill = country, weight = weight))
  if (file.exists(paste0(results_dir, "/", indicator, "_rf.csv"))) {
    violin <- violin + geom_hline(aes(yintercept = rake_to_mean))
  }
  violin <- violin +
    theme(axis.text.x = element_text(face = "bold", size = 10, angle = 90)) +
    theme(legend.position = "none") +
    xlab("") +
    ylab("Mean outcome") +
    facet_wrap(~ country + year)

  all_plots <- list(gg_country_vario, country_pacf, violin)

  ## Return ggs for violin, vario, and pacf model object to plot() later.
  return(all_plots)
}
