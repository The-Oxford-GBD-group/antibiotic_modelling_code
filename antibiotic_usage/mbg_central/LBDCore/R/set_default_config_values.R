#' @title Set unsupplied configuration values with defaults
#' @description Checks the existing \code{config} object and compares it to \code{default_config}. Any values not set in \code{config} are set with the default values.
#'
#' Default values may be string values OR functions. Functions must be written in the form "func_name(config)" (no quotes necessary). This allows for dynamically set default values e.g., a way to always refer to the newest worldpop covariate release.
#'
#' All non-function default values will be processed first, and then function default values will be processed. Each function MUST take exactly 1 parameter: \code{config} (the current configuration data.table object). This allows for default values which require contextual information e.g., the latest worldpop a0004t release may differ from the latest worldpop a0509f release.
#'
#' \strong{IMPORTANT}: the configuration values provided have not yet been converted to appropriate R types so they \strong{will all be characters}.
#'
#' @param config a \code{data.table} with the existing configuration. This MUST have two columns named V1 and V2. V1 should contain configuration variable names e.g., "pop_measure", "long_col". The V2 should contain configuration values e.g., "a0004t", "long".
#' @param default_config a \code{data.table} with default configuration values. This MUST have two rows. The first row must be configuration variable names e.g., "pop_measure", "long_col". The second row should contain default values which must either be regular string values (which will be converted to appropriate types later) OR function calls in the form of \code{function_returning_default_value(config)}
#' @export
#' @return updated \code{config} table.
set_default_config_values <- function(config, default_config) {
  unset_values <- setdiff(names(default_config), config$V1)
  # remove any already set values. partition the remaining into static values and those using functions
  default_config <- default_config[, unset_values, with = FALSE]

  # this is the suffix we expect on any dynamically set configuration variables
  func_suffix_pat <- "\\(config\\)$"

  # process static configuration vars first
  static_config_vars <- grep(func_suffix_pat, default_config[1, ], invert = TRUE)
  if (length(static_config_vars) > 0) {
    static_defaults <- default_config[, static_config_vars, with = FALSE]
    for (conf_var in names(static_defaults)) {
      default <- default_config[1, conf_var, with = FALSE]
      message(sprintf("%s not found in config. Adding default value: %s", conf_var, default))
      config <- rbindlist(list(config, data.table(V1 = conf_var, V2 = default)))
    }
  }

  # process dynamic configuration vars second
  dynamic_config_vars <- grep(func_suffix_pat, default_config[1, ])
  if (length(dynamic_config_vars) > 0) {
    dynamic_defaults <- default_config[, dynamic_config_vars, with = FALSE]
    # this all seems in order...
    # stop(sprintf("dynamic_config_vars are %s with names %s", paste(dynamic_config_vars), paste(names(dynamic_defaults))))
    for (conf_var in names(dynamic_defaults)) {
      func_name_parens <- default_config[1, conf_var, with = FALSE]
      func_name <- gsub(func_suffix_pat, "", func_name_parens)
      if (!exists(func_name)) {
        stop(sprintf("Config %s has default of %s, but no function exists named", conf_var, func_name_parens, func_name))
      }
      func <- get(func_name)
      if (!is.function(func)) {
        stop(sprintf("Config %s has default of %s but %s is not a function", conf_var, func_name_parens, func_name))
      }
      default <- tryCatch({
        func(config)
      }, error = function(e) {
        stop(sprintf("%s errored with message: %s", func_name_parens, geterrmessage()))
      })
      message(sprintf("%s not found in config. Adding default value: %s", conf_var, default))
      config <- rbindlist(list(config, data.table(V1 = conf_var, V2 = default)))
    }
  }
  return(config)
}
