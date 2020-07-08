#' @title Configuration data.table: Description
#' @description A data.table with default config values and description for MBG
#' @examples
#' \dontrun{
#' data(config_description)
#' }
"config_description"

#' @title Configuration data.table
#' @description A data.table with default config values for MBG
#' @examples
#' \dontrun{
#' data(config_values)
#' }
"config_values"


#' @title Configuration must-haves for MBG
#' @description Configuration must-haves for MBG
#' @examples
#' \dontrun{
#' data(must_haves)
#' }
#' @format A vector of necessary configuration names
"must_haves"



#' @title Tests for config files
#' @description A list of tests for checking types (and some values) for all the MBG configuration variables
#' @examples
#' \dontrun{
#' data(config_tests)
#' }
"config_tests"


#' @title Full List of All Reference Regions
#' @description Define modeling regions that are not simple combinations of the
#'   default MBG regions (in other words, regions that are not combinations of
#'   four-letter MBG regions such as "wssa" or "seas+ocea" or ISO-3 codes such
#'   as 'ZAF' or 'CHN').
#' If you need to add a new custom region, add it to this list
#' @examples
#' \dontrun{
#' data(ref_reg_list)
#' }
"ref_reg_list"


## Slot estimation fits ##
#' @title Fitted model objects for estimating the memory of parallel_model
"ace.r"


#' @title Fitted model objects for estimating the memory of parallel_model
#' @description Based off of profiled model runs from 2018, AOZ
#' performed some nonlinear regression to estimate the relationship
#' between peak RAM usage and number of pixels, number of years, and
#' the number of posterior samples in prediction. The resulting model fit
#' objects are stored in this data object to allow RAM use prediction with the
#' function estimate_parallel_model_mem().
"fss"



#' @title Fitted model objects for estimating the memory of parallel_model
"percent.over"


#' @title Fitted model objects for estimating the memory of parallel_model
"res.sd"
