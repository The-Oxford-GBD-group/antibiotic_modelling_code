#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param countries PARAM_DESCRIPTION
#' @param from PARAM_DESCRIPTION, Default: 'iso3'
#' @param verbose PARAM_DESCRIPTION, Default: F
#' @param shapefile_version PARAM_DESCRIPTION, Default: 'current'
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   # EXAMPLE1
#' }
#' }
#' @seealso
#'  \code{\link[stringr]{str_match}}
#' @rdname gaul_convert
#' @export
#' @importFrom stringr str_match
gaul_convert <- function(countries, from = "iso3", verbose = F, shapefile_version = "current") {

  # Purpose: Convert a vector of countries (ihme_loc_id format) to vector of GAUL codes
  # Inputs:
  #         countries: vector of countries in ihme_loc_id format
  #         from: format of input
  #               options: "iso3" = "ihme_loc_id" = "ihme_lc_id"
  #                        "name" (the loc_name or loc_nm_short)
  #               ("iso3" is treated as "ihme_loc_id" for backwards compatability)
  #
  # Outputs: a vector of gaul codes

  # load reference table

  if (Sys.info()["sysname"] == "Linux") {
    j_root <- "/home/j/"
  } else {
    j_root <- "J:/"
  }

  str_match <- stringr::str_match

  # Catch if already passed gaul codes
  if (class(countries) == "numeric") return(countries)
  if (all(grepl("^[[:digit:]]+$", countries))) return(countries)

  gaul_table <- get_location_code_mapping(shapefile_version = shapefile_version)

  # convert input & output to lower case for easier matching
  # lowercase_cols <- c("short_name", "official_name", "iso3", "iso2", "uni", "undp")
  # gaul_table[, (lowercase_cols) := lapply(.SD, tolower), .SDcols = lowercase_cols,]

  ## ## convert input & output to lower case for easier matching
  if (verbose == T) message("\nlowercasing columns. the columns that get lowered are:")
  for (i in 1:ncol(gaul_table)) {
    if (any(class(gaul_table[[i]]) %in% c("factor", "character"))) {
      if (verbose == T) message(sprintf("On column: %s", colnames(gaul_table)[i]))
      gaul_table[[i]] <- tolower(gaul_table[[i]])
    }
  }

  # Lowercase & ensure character for input
  countries <- tolower(countries)
  countries <- as.character(countries)

  ## Catch if a subnational in XXX_##### IHME syntax
  ## This returns national-level gaul codes only
  if (any(grepl("_", countries))) {
    countries[grepl("_", countries)] <- str_match(countries[grepl("_", countries)], "(.*)_.*")[, 2]
  }

  if (from == "iso3" | from == "ihme_loc_id" | from == "ihme_lc_id") {
    gaul_code <- sapply(countries, function(x) gaul_table[ihme_lc_id == x, GAUL_CODE]) %>% as.numeric()
  } else if (from == "name") {

    # Matching only national-level for now
    # Drop undefined & subnational rows
    gaul_table_nat <- subset(gaul_table, GAUL_CODE != -1)
    gaul_table_nat <- subset(gaul_table_nat, level == 3)

    gaul_code <- sapply(countries, function(x) gaul_table_nat[loc_nm_sh == x, GAUL_CODE]) %>% as.numeric()

    # check to see if this matched all of the provided items in the vector; use partial / fuzzy matching if not
    if (length(gaul_code[is.na(gaul_code)]) > 0) {

      # Create a table to fill in
      table_matching <- cbind(countries, gaul_code) %>% as.data.table()
      names(table_matching) <- c("country", "gaul_code")
      table_matching$gaul_code <- as.numeric(table_matching$gaul_code)

      approx_matched <- table_matching[is.na(gaul_code), country]

      # Indicate that approximate matching took place

      message("\nNot all country names provided were found in the lookup table.")
      message("Attempting to match names provided with those in lookup table.")
      message(paste0("Approximate matching attempted for: ", paste(approx_matched, collapse = ", "), "\n"))

      approx_match <- function(country) {
        # First, try matching to long form of name
        gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name), ]$GAUL_CODE

        # If that doesn't work, grep within the short name
        if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_nm_sh), ]$GAUL_CODE

        # If that doesn't work, grep within the long name
        if (length(gaul_code) == 0) gaul_code <- gaul_table_nat[grep(country, gaul_table_nat$loc_name), ]$GAUL_CODE

        # Could fill in other matching here if desired

        # Warn if nonspecific
        if (length(gaul_code) > 1) warning(paste0("\"", country, "\" matches multiple country names in the lookup table. Please be more specific."))

        # Finally, if no matches, return NA
        if (length(gaul_code) != 1) gaul_code <- NA

        return(as.numeric(gaul_code))
      }

      # Try approximate matching
      table_matching[is.na(gaul_code)]$gaul_code <- sapply(table_matching[is.na(gaul_code)]$country, approx_match)

      not_matched <- table_matching[is.na(gaul_code)]$country

      # Error checking
      if (length(not_matched) > 0) {
        warning(paste0("Some countries could not be matched:\n", paste(not_matched, collapse = ", ")))
      }
      gaul_code <- table_matching$gaul_code %>% as.numeric()
    }
  } else {
    # Error catching for non-supported country type
    stop("\nPlease enter a valid country code type")
  }

  if (length(gaul_code[is.na(gaul_code)]) > 0) {
    # Error catching for failure to match all country codes
    message(paste0(
      "CAUTION! Returning NA values.\nMatches not found for all country codes in input list.\n",
      "Please check your input values"
    ))
  }
  return(gaul_code)
}
