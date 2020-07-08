#' @title Query config info to SQLite database
#' @description This function will query the SQLite database with all the config records using run_date
#'
#' @param run_date Run date datestamp
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery
#' @importFrom RSQLite SQLite
#' @importFrom glue glue
#'
#' @export
#'
query_config_db <- function(run_date) {

  ## Open connection to database
  dbpath <- "/share/geospatial/run_logs/config_db/run_db_20190216.sqlite"
  configdb <- dbConnect(SQLite(), dbpath)

  ## Upload config
  config_out <- dbGetQuery(configdb, glue("select * from config_upload where run_date = '{run_date}'"))

  ## Disconnect db
  dbDisconnect(configdb)

  return(config_out)
}
